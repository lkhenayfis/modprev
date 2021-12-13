####################################################################################################
# FUNCOES DE ESTIMACAO E PREVISAO DE MODELOS PARA TEMPO REAL
####################################################################################################

# ESTIMACAO ----------------------------------------------------------------------------------------

#' Ajuste De Modelos
#' 
#' Wrapper de estimação para múltiplos modelos com interface e saída unificadas
#' 
#' Esta função facilita a estimação de diversos tipos de modelos com uma única interface e, mais 
#' importante ainda, única estrutura de saída. O tipo de modelo estimado para a série passada 
#' através de \code{serie} é selecionado através do argumento \code{tipo}, podendo ser um de
#' 
#' \describe{
#'     \item{\code{sarima}}{SARIMA(p, d, q)(P, D, Q)}
#'     \item{\code{ss_ar1_saz}}{Espaço de estados composto por processo AR(1) + Sazonalidade}
#'     \item{\code{ss_reg_din}}{Regressão univariada dinâmica}
#' }
#' 
#' Deve ser notado que no caso dos modelos com sazonalidade, o argumento \code{serie} \emph{DEVE SER
#' UM OBJETO SERIE TEMPORAL COM PERIODO ESPECIFICADO}. Isto e necessario para que a função possa 
#' automaticamente lidar com diversos tipos de séries sem a necessidade de demais argumentos.
#' 
#' No caso de modelos com variaveis explicativas deve ser fornecido um parametro \code{regdata} na
#' forma de uma matriz ou data.frame contendo apenas as colunas com variáveis a serem utilizadas.
#' 
#' O numero \code{out_sample} diz respeito ao numero de observacoes a serem deixadas para teste. Ou
#' seja, se desejamos deixar 10 horas para teste, o valor do parametro deve ser
#' 
#' out_sample = 20 (10 x 2 meia horas por hora)
#' 
#' Se for informado um valor diferente de zero, os últimos \code{out_sample} da série serão 
#' cortados e apenas o restante é passado para o fit. Esta parte removida continua com o objeto 
#' de saída e sera usada para comparação com a previsão no método \code{predict}
#' 
#' @param serie série para ajustar
#' @param out_sample número de pontos para deixar de fora da amostra. Ver Detalhes
#' @param tipo tipo de modelo a ser ajustado. Ver Detalhes
#' @param ... demais parâmetros passados para as funções de fit específicas de cada modelo
#' 
#' @examples 
#' 
#' # ajustando tipo SARIMA
#' mod_sarima <- estimamodelo(AirPassengers, tipo = "sarima")
#' 
#' # caso a serie nao possua sazonalidade explicita, o modelo sera ajustado sem isso
#' ss <- arima.sim(200, model = list(ar = .8))
#' mod_sarima_semsazo <- estimamodelo(ss, tipo = "sarima")
#' 
#' \dontrun{
#'     # estima so um AR(1) sem sazonalidade
#'     coef(mod_sarima_semsazo)
#' }
#' 
#' # ajustando uma regressao dinamica (com dado dummy interno do pacote)
#' serie <- window(datregdin[[1]], 1, 100)
#' varex <- window(datregdin[[2]], 1, 100)
#' mod_regdin <- estimamodelo(serie, regdata = varex, tipo = "reg_din")
#' 
#' @return objeto da classe mod_eol contendo modelo (classe dependente do modelo ajustato), serie
#'     ajustada e, caso \code{out_sample > 0}, a parte reservada para comparação
#' 
#' @export

estimamodelo <- function(serie, out_sample, tipo, ...) UseMethod("estimamodelo")

#' @export

estimamodelo.ts <- function(serie, out_sample = 0L, tipo = c("sarima", "ss_ar1_saz", "ss_reg_din"), ...) {

    # Separa in-sample e out-of-sample
    aux <- quebrats(serie, out_sample)
    serie_in  <- aux[[1]]
    serie_out <- aux[[2]]

    # Compoe chamada de fit para o tipo especificado
    tipo <- match.arg(tipo)
    fit_func <- match.call()
    fit_func[[1]] <- as.name(paste0("fit_", tipo))
    fit_mod <- eval(fit_func, parent.frame())

    # Adiciona classe e serie out-ot-sample
    out <- list(modelo = fit_mod, serie_in = serie_in, serie_out = serie_out)
    class(out) <- "mod_eol"
    attr(out, "tipo") <- tipo

    # Retorna
    return(out)
}

estimamodelo.ts_TR <- function(serie, out_sample, tipo = c("sarima", "ss_ar1_saz", "ss_reg_din"), ...) {

    if(missing(out_sample)) out_sample <- attr(serie, "out_sample")

    aux <- quebrats(serie, out_sample)
    serie_in  <- aux[[1]]
    serie_out <- aux[[2]]

    tipo <- match.arg(tipo)
    fit_func <- paste0("fit_", tipo)
    fit_mod  <- do.call(fit_func, c(list(serie = serie_in), list(...)))

    out <- list(modelo = fit_mod, serie_in = serie_in, serie_out = serie_out)
    class(out) <- "mod_eol"
    attr(out, "tipo") <- tipo

    return(out)
}

fit_sarima <- function(serie, ...) {
    out <- auto.arima(serie, allowdrift = FALSE)
    return(out)
}

fit_ss_ar1_saz <- function(serie, ...) {

    Z <- matrix(c(1, 1), 1)
    T <- matrix(c(1, 0, 0, 0), 2)
    R <- matrix(c(0, 1), 2)

    if(frequency(serie) == 1) {
        mod <- SSModel(serie ~ -1 +
            SSMcustom(Z = Z, T = T, R = R, a1 = c(1, 0), Q = NA),
            H = 0)
    } else {
        mod <- SSModel(serie ~ -1 +
            SSMcustom(Z = Z, T = T, R = R, a1 = c(1, 0), Q = NA) +
            SSMseasonal(period = frequency(serie), sea.type = "dummy", Q = NA),
            H = 0)
    }
    upfunc <- function(par, model) {
        model["Z", "custom"][1] <- par[1]
        model["T", "custom"][2, 2] <- par[2] / sqrt(1 + par[2]^2)
        model["Q", etas = "custom"] <- exp(par[3])
        model["Q", etas = "seasonal"] <- exp(par[4])
        model["P1", "custom"][2, 2] <- exp(par[3]) / (1 - (par[2] / sqrt(1 + par[2]^2))^2)
        model
    }
    fit <- fitSSM(mod, inits = c(mean(serie), 0, 0, 0), updatefn = upfunc, method = "BFGS")

    if(abs(logLik(fit$model)) < 1e-10) {
        fit$model$Z[] <- NA
    }

    return(fit$model)
}

fit_ss_reg_din <- function(serie, regdata, ...) {

    if(missing(regdata)) stop("Forneca a variavel explicativa atraves do parametro regdata")

    if("data.frame" %in% class(regdata)) {
        regdata <- data.matrix(regdata)
    } else {
        regdata <- as.matrix(regdata)
    }

    nvars <- ncol(regdata)

    mod <- SSModel(serie ~ SSMregression(~ regdata, Q = diag(NA_real_, nvars)), H = matrix(NA))
    fit <- fitSSM(mod, rep(0, 2), method = "BFGS")

    if(fit$optim.out$convergence < 0) {
        fit$model$Z[] <- NA
    }

    return(fit$model)
}

# PREVISAO -----------------------------------------------------------------------------------------

#' Previsao De Modelos mod_eol
#' 
#' Wrapper para previsao e plot de modelos ajustados por \code{estimamodelo}
#' 
#' Essa funcao e um simples facilitador do processo de previsao e plot out of sample para 
#' comparacao com verificado adiante.
#' 
#' Se nao forem fornecidos os argumentos n.ahead ou out_sample, ambos serao pegos por padrao do 
#' objeto mod_eol fornecido. Caso algum seja fornecido sera utilizado no lugar do padrao
#' 
#' @param fit modelo ajustado. Ver detalhes
#' @param n.ahead numero de passos a frente para prever. Ver detalhes
#' @param serie_out serie fora da amostra para comparacao. Ver detalhes
#' @param plot booleano indicando se deve ser plotado o resultado da previsao
#' @param ... parametros extras passados para o metodo de \code{predict} apropriado. Ver detalhes
#' 
#' @value vetor numerico vetor de previsoes. Opcionalmente um plot

# ;;TESTAR continuidade da serie_out em final, meio e inicio de dia

predict.mod_eol <- function(fit, n.ahead, serie_out, plot = TRUE, ...) {

    # Identifica se serie out foi fornecida
    if(missing(serie_out)) {
        serie_out <- fit$serie_out
    } else {
        # Garante continuidade
        S  <- frequency(fit$serie_in)
        FIM <- end(fit$serie_in)
        INI <- c(FIM[1] + 1 * (FIM[2] == S), (FIM[2] + 1) * (FIM[2] != 48) + 1 * (FIM[2] == 48))
        serie_out <- ts(serie_out, start = INI, freq = S)
    }

    # Identifica se n.ahead foi fornecido
    if(missing(n.ahead)) n.ahead <- length(serie_out)

    # Realiza previsao
    if(n.ahead != 0) {
        pred_func <- paste0("pred_", attr(fit, "tipo"))
        args      <- c(list(model = fit$modelo, n.ahead = n.ahead), list(...))
        prev <- do.call(pred_func, args)
    } else {
        prev <- cbind(prev = NULL, sd = NULL)
    }

    # Checa se deve ser feito o plot
    if(plot) {
        S      <- frequency(fit$serie_in)
        eixo_x <- c(start(fit$serie_in)[1] + start(fit$serie_in)[2] / S, NA)
        if(!is.null(prev)) {
            eixo_x[2] <- end(prev[, 1])[1] + end(prev[, 1])[2] / S
        } else {
            eixo_x[2] <- end(fit$serie_in)[1] + end(fit$serie_in)[2] / S
        }

        plot(fit, xlim = eixo_x, ylim = range(fit$serie_in) * c(1, 1.2), legend = FALSE)
        lines(serie_out)
        lines(prev[, 1], col = 4)
        legend("topright", inset = 0.02,
            legend = c("Serie", "Ajuste", "Previsao"),
            lty = c(1, 2, 1), col = c(1, 4, 4))
    }

    # Retona vetor de previao
    return(prev)
}

pred_sarima <- function(model, ...) {
    prev <- predict(model, ...)
    prev <- do.call(cbind, prev)
    colnames(prev) <- c("prev", "sd")
    prev
}

pred_ss_ar1_saz <- function(model, ...) {
    prev <- predict(model, se.fit = TRUE, filter = TRUE, ...)
    colnames(prev) <- c("prev", "sd")
    prev
}

# ATUALIZACAO --------------------------------------------------------------------------------------

#' Atualizacao De Modelos mod_eol
#' 
#' Wrapper para atualizar e possivelmete reajustar modelos mod_eol
#' 
#' @param fit modelo ajustado atraves de estimamodelo
#' @param newdata nova serie para associar ao modelo
#' @param refit booleano indicando se o modelo deve ser reajustado
#' 
#' @value modelo com novos dados e possivelmente reajustado

update.mod_eol <- function(fit, newdata, refit = FALSE) {

    if(refit) {
        fit <- estimamodelo(serie = newdata, tipo = attr(fit, "tipo"))
    } else {
        newdata <- quebrats(newdata)
        upd_func <- paste0("upd_", attr(fit, "tipo"))
        args     <- list(model = fit$modelo, newdata = newdata[[1]])
        fit$modelo <- do.call(upd_func, args)
        fit[2:3] <- newdata
    }

    return(fit)
}

upd_sarima <- function(model, newdata) Arima(newdata, model = model)

upd_ss_ar1_saz <- function(model, newdata) {

    # Se modelo nao convergiu, tenta reestimar
    if(all(is.na(model$Z))) return(estimamodelo(newdata, tipo = "ss_ar1_saz")$modelo)

    # Do contrario, atualiza normalmente
    model$y <- newdata
    attr(model$y, "dim") <- c(length(newdata), 1)
    attr(model, "n") <- as.integer(length(newdata))
    model
}

# JANELA MOVEL -------------------------------------------------------------------------------------

#' Previsao Em Horizonte Rolante
#' 
#' Funcao para realizar previsoes e reajustes em janela movel
#' 
#' \code{objeto} pode ser tanto um objeto mod_eol contendo um modelo ajustado ou uma serie temporal
#' simples. No primeiro caso, deve ser passado em conjunto uma serie temporal atraves do parametro
#' \code{serie} e, no segundo, deve ser informado o tipo de modelo a ser ajustado atraves do 
#' parametro \code{tipo}.
#' 
#' Embora esta estrutura pareca redundante, ela permite que um modelo ajustado independentemente 
#' possa ser utilizado para prever em janela rolante uma outra serie qualquer, diferente daquela 
#' utilizada para sua estimacao.
#' 
#' @param objeto serie ou modelo ajusdado para previsao rolante. Ver detalhes
#' @param tipo tipo de modelo a ser ajustado, caso \code{objeto} seja uma serie temporal
#' @param serie serie temporal pela qual passar a janela movel. Ver Detalhes
#' @param largura numero de observacoes na janela movel
#' @param n.ahead numero de passos a frente para prever
#' @param refit_cada escalar ou vetor inteiro. Se escalar, reajusta o modelo a cada 
#'     \code{refit_cada} observacoes. Se vetor, reajusta apos cada indice de \code{refit_cada}
#' @param verbose Escalar inteiro indicando quanta informacao a ser emitida durante rodada. 
#'     0 = nenhuma, 1: toda vez que reajusta modelo, 2: todo horizonte de previsao e reajuste
#' 
#' @value [lista] lista contendo previsoes de 1 a n.ahead passos a frente para cada janela

janelamovel <- function(objeto, ...) UseMethod("janelamovel")

janelamovel.ts <- function(objeto, tipo, largura, n.ahead = 1L, refit_cada = NA, verbose = 0) {

    # Caracteristicas da serie
    N <- length(objeto)
    S <- frequency(objeto)
    INI <- start(objeto)

    # Funcao de verbose
    verb_func <- switch(as.character(verbose),
        "0" = function(...) NULL,
        "1" = function(i, f, r) if(r) cat("\t Prevendo serie [", i, "] -> [", f, "]\n") else NULL,
        "2" = function(i, f, r) if(r) {
            cat("REFIT -- Prevendo serie [", i, "] -> [", f, "]\n")
        } else {
            cat("\t Prevendo serie [", i, "] -> [", f, "]\n")
        })

    # Janelas
    Nj <- N - largura + 1

    # Monta indices para refit
    if(length(refit_cada) == 1) {
        passo   <- ifelse(!is.na(refit_cada), refit_cada, Nj)
        v_refit <- 1:Nj %in% seq(1, N, by = passo)[-1]
    } else {
        v_refit <- refit_cada
    }

    # Fit do primeiro modelo
    fim_t <- deltats(INI, delta = largura - 1, freq = S)
    serie <- window(objeto, start = INI, end = fim_t)
    fit   <- estimamodelo(serie = serie, out_sample = 0L, tipo = tipo)

    # Loop da janela movel
    l_prev <- vector("list", N - largura + 1)
    for(i in 1:Nj) {

        # Reduz serie e atualiza modelo
        ini_t <- deltats(INI, delta = i - 1, freq = S)
        fim_t <- deltats(ini_t, delta = largura - 1, freq = S)
        verb_func(ini_t, fim_t, v_refit[i])
        serie <- window(objeto, start = ini_t, end = fim_t)
        fit   <- update(fit = fit, newdata = serie, refit = v_refit[i])

        # Preve n.ahead passos
        prev <- predict(fit = fit, n.ahead = n.ahead, plot = FALSE)

        # Salva na lista
        l_prev[[i]] <- prev
    }

    # Retorna
    return(l_prev)
}