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
#' forma de uma matriz ou data.frame contendo apenas as colunas com variáveis a serem utilizadas. Se
#' houver apenas uma variável explicativa, pode ser passada como um vetor ou série temporal.
#' 
#' @param serie série para ajustar
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
#' # estima so um AR(1) sem sazonalidade
#' coef(mod_sarima_semsazo)
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

estimamodelo <- function(serie, tipo, ...) UseMethod("estimamodelo")

#' @export

estimamodelo.ts <- function(serie, tipo = c("sarima", "ss_ar1_saz", "ss_reg_din"), ...) {

    tipo <- match.arg(tipo)
    fit_func <- match.call()
    fit_func[[1]] <- as.name(paste0("fit_", tipo))
    fit_mod <- eval(fit_func, parent.frame())

    out <- list(modelo = fit_mod, serie = serie)
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

    regdata <- as.matrix(regdata)

    nvars <- ncol(regdata)

    mod <- SSModel(serie ~ SSMregression(~ regdata, Q = diag(NA_real_, nvars)), H = matrix(NA))
    fit <- fitSSM(mod, rep(0, 2), method = "BFGS")

    if(fit$optim.out$convergence < 0) {
        fit$model$Z[] <- NA
    }

    return(fit$model)
}

# PREVISAO -----------------------------------------------------------------------------------------

#' Previsão De Modelos \code{mod_eol}
#' 
#' Wrapper para previsão e plot de modelos ajustados por \code{estimamodelo}
#' 
#' Essa função é um simples facilitador do processo de previsão e plot out of sample para 
#' comparação com verificado adiante.
#' 
#' Se não forem fornecidos os argumentos n.ahead ou out_sample, ambos serão pegos por padrão do 
#' objeto mod_eol fornecido. Caso algum seja fornecido será utilizado no lugar do padrão.
#' 
#' No caso de modelos com variáveis explicativas deve ser fornecido um parâmetro \code{newdata} na
#' forma de uma matriz ou data.frame contendo apenas as colunas com variáveis a serem utilizadas. Se
#' houver apenas uma variável explicativa, pode ser passada como um vetor ou série temporal. Nestes
#' modelos, se o argumento \code{n.ahead} também tiver sido fornecido, \code{newdata} será reduzida 
#' às \code{n.ahead} primeiras observações.
#' 
#' @param object modelo ajustado através de \code{\link{estimamodelo}}
#' @param n.ahead numero de passos a frente para prever. Ver detalhes
#' @param serie_out serie fora da amostra para comparação. Ver detalhes
#' @param plot booleano indicando se deve ser plotado o resultado da previsão
#' @param ... parametros extras passados para o metodo de \code{predict} apropriado. Ver detalhes
#' 
#' @examples
#' 
#' mod <- estimamodelo(AirPassengers, tipo = "sarima")
#' 
#' \dontrun{
#' predict(mod, n.ahead = 24)
#' predict(mod)
#' }
#' 
#' # em modelos de regressao, deve ser passada a variavel explicativa via newdata
#' 
#' serie <- window(datregdin[[1]], 1, 200)
#' varex <- window(datregdin[[2]], 1, 200)
#' mod_regdin <- estimamodelo(serie, regdata = varex, tipo = "ss_reg_din")
#' 
#' \dontrun{
#'     newdata <- window(datregdin[[2]], 201, 230)
#'     predict(mod_regdin, newdata = newdata)
#' }
#' 
#' @return série temporal multivariada contendo a previsão e o desvio padrão associado; caso 
#'     \code{plot = TRUE} plota a previsão
#' 
#' @export

predict.mod_eol <- function(object, n.ahead, serie_out, plot = TRUE, ...) {

    if(missing(serie_out)) {
        serie_out <- object$serie_out
    } else {
        S  <- frequency(object$serie_in)
        FIM <- end(object$serie_in)
        INI <- c(FIM[1] + 1 * (FIM[2] == S), (FIM[2] + 1) * (FIM[2] != 48) + 1 * (FIM[2] == 48))
        serie_out <- ts(serie_out, start = INI, freq = S)
    }

    pred_func <- match.call()
    pred_func[[1]] <- as.name(paste0("pred_", attr(object, "tipo")))
    prev <- eval(pred_func, parent.frame())

    if(plot) {
        S      <- frequency(object$serie_in)
        eixo_x <- c(start(object$serie_in)[1] + start(object$serie_in)[2] / S, NA)
        if(!is.null(prev)) {
            eixo_x[2] <- end(prev[, 1])[1] + end(prev[, 1])[2] / S
        } else {
            eixo_x[2] <- end(object$serie_in)[1] + end(object$serie_in)[2] / S
        }

        plot(object, xlim = eixo_x, ylim = range(object$serie_in) * c(1, 1.2), legend = FALSE)
        lines(serie_out)
        lines(prev[, 1], col = 4)
        legend("topright", inset = 0.02,
            legend = c("Serie", "Ajuste", "Previsao"),
            lty = c(1, 2, 1), col = c(1, 4, 4))
    }

    return(prev)
}

pred_sarima <- function(object, ...) {
    modelo <- object$modelo

    prev <- predict(modelo, ...)
    prev <- do.call(cbind, prev)
    colnames(prev) <- c("prev", "sd")

    return(prev)
}

pred_ss_ar1_saz <- function(object, ...) {
    modelo <- object$modelo

    prev <- predict(modelo, se.fit = TRUE, filter = TRUE, ...)
    colnames(prev) <- c("prev", "sd")

    return(prev)
}

pred_ss_reg_din <- function(object, newdata, ...) {
    modelo <- object$modelo

    if(missing(newdata)) stop("Forneca a variavel explicativa para previsao atraves do parametro newdata")

    newdata <- as.matrix(newdata)

    if("n.ahead" %in% names(list(...))) {
        regobs <- min(list(...)$n.ahead, nrow(newdata))
        newdata <- newdata[seq(regobs), , drop = FALSE]
    }

    Hmat <- modelo["H"]
    Qmat <- modelo["Q"]
    extmod <- SSModel(rep(NA_real_, nrow(newdata)) ~ SSMregression(~ newdata, Q = Qmat), H = Hmat)

    prev <- predict(modelo, newdata = extmod, se.fit = TRUE, filtered = TRUE, ...)
    colnames(prev) <- c("prev", "sd")

    return(prev)
}

# ATUALIZACAO --------------------------------------------------------------------------------------

#' Atualizacao De Modelos mod_eol
#' 
#' Wrapper para atualizar e possivelmete reajustar modelos \code{mod_eol}
#' 
#' O padrão desta função é simplesmente substituir \code{newseries} no modelo ajustado \code{fit}, 
#' isto é, mantendo todos os hiperparâmetros estimados originalmente. Através do argumento
#' \code{refit} é possível realizar o reajuste do modelo para a nova série.
#' 
#' No caso de modelos com variaveis explicativas deve ser fornecido um parametro \code{newregdata} 
#' na forma de uma matriz ou data.frame contendo apenas as colunas com variáveis a serem utilizadas.
#' Se houver apenas uma variável explicativa, pode ser passada como um vetor ou série temporal.
#' 
#' @param fit modelo ajustado atraves de estimamodelo
#' @param newseries nova serie para associar ao modelo
#' @param refit booleano indicando se o modelo deve ser reajustado
#' @param ... demais parâmetros passados para as updates específicas. Ver Detalhes
#' 
#' @examples 
#' 
#' serie1 <- window(datregdin[[1]], 1, 300)
#' serie2 <- window(datregdin[[1]], 501, 800)
#' 
#' mod_orig  <- estimamodelo(serie1, tipo = "sarima")
#' mod_upd   <- update(mod_orig, serie2, refit = FALSE)
#' mod_refit <- update(mod_orig, serie2, refit = TRUE)
#' 
#' \dontrun{
#' # comparando os casos
#' coef(mod_orig$modelo)
#' coef(mod_upd$modelo)
#' coef(mod_refit$modelo)
#' }
#' 
#' @return modelo com novos dados e possivelmente reajustado
#' 
#' @export

update.mod_eol <- function(object, newseries, refit = FALSE, ...) {

    tipo <- attr(object, "tipo")

    if(refit) {
        call <- list(...)
        call <- c(list(quote(estimamodelo), serie = newseries, tipo = tipo), call)
        names(call)[grep("newregdata", names(call))] <- "regdata"
        object <- eval(as.call(call), parent.frame())
    } else {
        upd_func <- match.call()
        upd_func[[1]] <- as.name(paste0("upd_", tipo))
        object$modelo <- eval(upd_func, parent.frame())
        object[[2]] <- newseries
    }

    return(object)
}

upd_sarima <- function(object, newseries, ...) Arima(newseries, model = object$modelo)

upd_ss_ar1_saz <- function(object, newseries, ...) {

    modelo <- object$modelo

    # Se modelo nao convergiu, tenta reestimar
    if(all(is.na(modelo$Z))) return(estimamodelo(newseries, tipo = "ss_ar1_saz")$modelo)

    # Do contrario, atualiza normalmente
    modelo$y <- newseries
    attr(modelo$y, "dim") <- c(length(newseries), 1)
    attr(modelo, "n") <- as.integer(length(newseries))

    return(modelo)
}

upd_ss_reg_din <- function(object, newseries, newregdata, ...) {

    modelo <- object$modelo

    if(missing(newregdata)) {
        stop("Forneca nova variavel explicativa atraves do parametro newregdata")
    }

    if("data.frame" %in% class(newregdata)) {
        colnames(newregdata) <- "xvar"
    } else {
        newregdata <- data.frame(xvar = newregdata)
    }

    # Se modelo nao convergiu, tenta reestimar
    if(all(is.na(modelo$Z))) return(estimamodelo(neseries, tipo = "ss_ar1_saz", regdata = newregdata)$modelo)

    # Do contrario, atualiza normalmente
    Hmat <- modelo["H"]
    Qmat <- modelo["Q"]
    newmod <- SSModel(
        newseries ~ SSMregression(~ xvar, data = newregdata, Q = Qmat), H = Hmat)

    return(newmod)
}

# JANELA MOVEL -------------------------------------------------------------------------------------

#' Previsão Em Horizonte Rolante
#' 
#' Função para realizar previsões e reajustes em janela móvel
#' 
#' Para cada janela de tamanho \code{largura} na \code{serie} fornecida será realizada uma previsão
#' \code{n.ahead} passos à frente, retornadas em uma lista. Isto significa que a última previsão 
#' realizada cobrirá até \code{n.ahead} além da última observação em \code{serie}.
#' 
#' O comportamento dessa função é mais fortemente impactado por \code{refit_cada}. Através deste 
#' argumento é possível indicar observações ou intervalos de tempo nos quais o modelo será 
#' reestimado. Nas demais janelas será feita apenas a atualização das informações.
#' 
#' Caso o modelo escolhido necessite de variáveis explicativas, é necessário que o argumento 
#' \code{regdata} seja passado na forma de uma matriz ou data.frame contendo apenas as colunas com 
#' variáveis a serem utilizadas. Se houver apenas uma variável explicativa, pode ser passada como um
#' vetor ou série temporal.
#' 
#' Este argumento \emph{DEVE CONTER AS VARIÁVEIS EXPLICATIVAS CORRESPONDENTES A TODAS AS OBSERVAÇÕES
#' DA SÉRIE MAIS \code{n.ahead} À FRENTE}. A primeira parte dessa restrição é natural, pois são 
#' necessárias as variáveis explicativas insample para ajustes do modelo. As observações 
#' \code{n.ahead} passos à frente do final da série são necessárias apenas para a previsão das 
#' últimas janelas.
#' 
#' @param serie serie temporal pela qual passar a janela movel
#' @param tipo tipo de modelo a ser ajustado, caso \code{objeto} seja uma serie temporal
#' @param largura numero de observacoes na janela movel
#' @param n.ahead numero de passos a frente para prever a cada passo
#' @param refit_cada escalar ou vetor inteiro. Se escalar, reajusta o modelo a cada 
#'     \code{refit_cada} observacoes. Se vetor, reajusta apos cada indice de \code{refit_cada}
#' @param verbose Escalar inteiro indicando quanta informacao a ser emitida durante rodada. 
#'     0 = nenhuma, 1: toda vez que reajusta modelo, 2: todo horizonte de previsao e reajuste
#' @param regdata por padrão é \code{NULL}, só sendo necessário caso \code{tipo} seja um modelo com
#'     variáveis explicativas. Ver Detalhes
#' 
#' @examples 
#' 
#' \dontrun{
#' serie <- ts(window(datregdin[[1]], 1, 250))
#' jm <- janelamovel(serie, "ss_ar1_saz", 200, refit_cada = 10)
#' 
#' serie <- ts(window(datregdin[[1]], 1, 720), freq = 48)
#' jm <- janelamovel(serie, "ss_ar1_saz", 480, refit_cada = 48)
#' 
#' serie <- ts(window(datregdin[[1]], 1, 250))
#' varex <- ts(window(datregdin[[2]], 1, 255))
#' jm <- janelamovel(serie, "ss_reg_din", 200, 5L, refit_cada = 10, regdata = varex)
#' # como esta funcao pode demorar um tempo para rodar (especialmente com multiplos refits), pode
#' # ser conveniente mandar o log para um arquivo
#' sink("log_janelamovel.txt")
#' jm <- janelamovel(serie, "ss_ar1_saz", 200, refit_cada = 10, verbose = 2)
#' sink()
#' }
#' 
#' @return lista contendo previsoes de 1 a n.ahead passos a frente para cada janela
#' 
#' @export

janelamovel <- function(serie, ...) UseMethod("janelamovel")

#' @export

janelamovel.ts <- function(serie, tipo, largura, n.ahead = 1L, refit_cada = NA, verbose = 0, regdata) {

    has_regdata <- !missing(regdata)

    # Caracteristicas da serie
    N <- length(serie)
    S <- frequency(serie)
    INI <- start(serie)

    if((tipo == "ss_reg_din") & !has_regdata) {
        stop("Forneca a variavel explicativa para previsao atraves do parametro newdata")
    }

    if(!has_regdata) {
        regdata <- NULL
    } else {
        regdata <- as.matrix(regdata)
    }

    # Funcao de verbose
    verb_func <- switch(as.character(verbose),
        "0" = function(...) NULL,
        "1" = function(i, f, r) if(r) cat("\t Prevendo serie [", i, "] -> [", f, "]\n") else NULL,
        "2" = function(i, f, r) if(r) {
            cat("REFIT -- Prevendo serie [", i, "] -> [", f, "]\n")
        } else {
            cat("\t Prevendo serie [", i, "] -> [", f, "]\n")
        }
    )

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
    fim_t    <- deltats(INI, delta = largura - 1, freq = S)
    iserie   <- window(serie, start = INI, end = fim_t)
    iregdata <- regdata[1:largura, , drop = FALSE]
    fit <- estimamodelo(serie = iserie, tipo = tipo, regdata = iregdata)

    # Loop da janela movel
    l_prev <- vector("list", N - largura + 1)
    for(i in 1:Nj) {

        # Reduz serie e atualiza modelo
        ini_t <- deltats(INI, delta = i - 1, freq = S)
        fim_t <- deltats(ini_t, delta = largura - 1, freq = S)
        verb_func(ini_t, fim_t, v_refit[i])

        iserie   <- window(serie, start = ini_t, end = fim_t)
        iregdata <- regdata[i:(largura + i - 1), , drop = FALSE]
        fit <- update(fit, newseries = iserie, refit = v_refit[i], newregdata = iregdata)

        # Preve n.ahead passos
        iniregdata <- (largura + i)
        fimregdata <- (largura + i + n.ahead - 1)
        inewdata <- regdata[iniregdata:fimregdata, , drop = FALSE]
        prev <- predict(fit, n.ahead = n.ahead, plot = FALSE, newdata = inewdata)

        # Salva na lista
        l_prev[[i]] <- prev
    }

    # Retorna
    return(l_prev)
}