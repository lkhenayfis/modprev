####################################################################################################
# FUNCOES DE ESTIMACAO E PREVISAO DE MODELOS PARA TEMPO REAL
####################################################################################################

library(KFAS)
library(forecast)

# ESTIMACAO ----------------------------------------------------------------------------------------

#' Generica para ajustar modelo a uma serie
#' 
#' Wrapper de multiplos modelos numa unica funcao com saida unificada
#' 
#' @param serie [objeto ts] serie para ajustar
#' @param out_sample [escalar inteiro] numero de pontos para deixar de fora da amostra. Ver Detalhes
#' @param tipo [escalar string] tipo de modelo a ser ajustado
#' 
#' @details O numero \code{out_sample} diz respeito ao numero de observacoes a serem deixadas para 
#'     teste. Ou seja, se desejamos deixar 10 horas para teste, o valor do parametro deve ser
#' 
#'     out_sample = 20 (10 x 2 meia horas por hora)
#' 
#'     Se for informado um valor diferente de zero, os ultimos \code{out_sample} da serie serao 
#'     cortados e apenas o restante e passado para o fit. Esta parte removida continua com o objeto 
#'     de saida e sera usado para previsao no metodo de \code{predict}
#' 
#' @value [objeto modTR] objeto contendo modelo (classe dependente do modelo ajustato)

estimamodelo <- function(serie, out_sample, tipo) UseMethod("estimamodelo")

estimamodelo.ts <- function(serie, out_sample = 0L, tipo = c("sarima", "ss_ar1_saz"), ...) {

    # Separa in-sample e out-of-sample
    aux <- quebrats(serie, out_sample)
    serie_in  <- aux[[1]]
    serie_out <- aux[[2]]

    # Compoe chamada de fit para o tipo especificado
    tipo <- match.arg(tipo)
    fit_func <- paste0("fit_", tipo)
    fit_mod  <- do.call(fit_func, c(list(serie = serie_in), list(...)))

    # Adiciona classe e serie out-ot-sample
    out <- list(modelo = fit_mod, serie_in = serie_in, serie_out = serie_out)
    class(out) <- "fit_TR"
    attr(out, "tipo") <- tipo

    # Retorna
    return(out)
}

estimamodelo.ts_TR <- function(serie, out_sample, tipo = c("sarima", "ss_ar1_saz"), ...) {

    # Olha se foi inputado um out_sample
    if(missing(out_sample)) out_sample <- attr(serie, "out_sample")

    # Separa in-sample e out-of-sample
    aux <- quebrats(serie, out_sample)
    serie_in  <- aux[[1]]
    serie_out <- aux[[2]]

    # Compoe chamada de fit para o tipo especificado
    tipo <- match.arg(tipo)
    fit_func <- paste0("fit_", tipo)
    fit_mod  <- do.call(fit_func, c(list(serie = serie_in), list(...)))

    # Adiciona classe e serie out-ot-sample
    out <- list(modelo = fit_mod, serie_in = serie_in, serie_out = serie_out)
    class(out) <- "fit_TR"
    attr(out, "tipo") <- tipo

    # Retorna
    return(out)
}

fit_sarima <- function(serie, ...) {
    out <- auto.arima(serie, allowdrift = FALSE)
    return(out)
}

fit_ss_ar1_saz <- function(serie, ...) {

    # Matrizes de sistema
    Z <- matrix(c(1, 1), 1)
    T <- matrix(c(1, 0, 0, 0), 2)
    R <- matrix(c(0, 1), 2)

    # Condicao inicial da sazonalidade
    a1saz <- matrix(sapply(1:(frequency(serie)-1), function(x) mean(serie[seq(x, length(serie), by = frequency(serie))])))
    a1saz <- a1saz - mean(a1saz)
    P1saz <- diag(rep(1000, 47))

    # Especifica modelo, funcao de atualizacao e estima
    mod <- SSModel(serie ~ -1 +
        SSMcustom(Z = Z, T = T, R = R, a1 = c(1, 0), Q = NA) +
        SSMseasonal(period = 48, sea.type = "dummy", Q = NA),
        H = 0)
    upfunc <- function(par, model) {
        model["Z", "custom"][1] <- par[1]
        model["T", "custom"][2, 2] <- par[2] / sqrt(1 + par[2]^2)
        model["Q", etas = "custom"] <- exp(par[3])
        model["Q", etas = "seasonal"] <- exp(par[4])
        model["P1", "custom"][2, 2] <- exp(par[3]) / (1 - (par[2] / sqrt(1 + par[2]^2))^2)
        model
    }
    fit <- fitSSM(mod, inits = c(mean(serie), 0, 0, 0), updatefn = upfunc, method = "BFGS")

    # Retorna apenas modelo ajustado
    return(fit$model)
}

# PREVISAO -----------------------------------------------------------------------------------------

#' Previsao de modelos fit_TR
#' 
#' Wrapper para previsao e plot de modelos ajustados por estimamodelo
#' 
#' @param fit [modelo ajustado] Ver detalhes
#' @param n.ahead [escalar inteiro] numero de passos a frente para prever. Ver detalhes
#' @param serie_out [vetor numerico ou ts] serie fora da amostra para comparacao. Ver detalhes
#' @param plot [escalar booleano] se deve ser plotado o resultado da previsao
#' @param ... parametros extras passados para o metodo de \code{predict} apropriado. Ver detalhes
#' 
#' @details Essa funcao e um simples facilitador do processo de previsao e plot out of sample para 
#'     comparacao com verificado adiante.
#' 
#'     Se nao forem fornecidos os argumentos n.ahead ou out_sample, ambos serao pegos por padrao do 
#'     objeto fit_TR fornecido. Caso algum seja fornecido sera utilizado no lugar do padrao
#' 
#' @value [vetor numerico] vetor de previsoes. Opcionalmente um plot

# ;;TESTAR continuidade da serie_out em final, meio e inicio de dia

predict.fit_TR <- function(fit, n.ahead, serie_out, plot = TRUE, ...) {

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

#' Atualizacao de modelos fit_TR
#' 
#' Wrapper para atualizar e possivelmete reajustar modelos fit_TR
#' 
#' @param fit [objeto fit_TR] modelo ajustado atraves de estimamodelo
#' @param newdata [serie temporal] nova serie para associar ao modelo
#' @param refit [booleano] se o modelo deve ser reajustado
#' 
#' @value [objeto fit_TR] modelo com novos dados e possivelmente reajustado

update.fit_TR <- function(fit, newdata, refit = FALSE) {

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
    model$y <- newdata
    attr(model$y, "dim") <- c(length(newdata), 1)
    attr(model, "n") <- as.integer(length(newdata))
    model
}

# JANELA MOVEL -------------------------------------------------------------------------------------

#' Previsao em horizonte rolante
#' 
#' Funcao para realizar previsoes e reajustes em janela movel
#' 
#' @param objeto [ts ou fit_TR] Ver detalhes
#' @param tipo [escalar string] tipo de modelo a ser ajustado
#' @param serie [ts] serie temporal pela qual passar a janela movel
#' @param largura [escalar inteiro] numero de observacoes na janela movel
#' @param n.ahead [escalar inteiro] numero de passos a frente para prever. Ver detalhes
#' @param refit_cada [escalar ou vetor inteiro] se escalar, reajusta o modelo a cada refit_cada 
#'     observacoes. Se for um vetor, reajusta apos cada indice de refit_cada
#' @param verbose [escalar numerico] Nivel de informacao a ser emitido durante rodada. 0 = nenhuma,
#'     1: toda vez que reajusta modelo, 2: todo horizonte de previsao e reajuste
#' 
#' @details \code{objeto} pode ser uma serie temporal ou objeto fit_TR. Se for uma serie temporal, 
#'    deve ser passado o parametro \code{tipo} conjuntamente. Caso \code{objeto} seja um fit_TR deve 
#'    ser passado o parametro \code{serie}
#' 
#'    A diferenca entre estes dois metodos e que passar um fit_TR permite filtrar toda a serie com 
#'    um modelo ajustado independentemente, com qualquer input que nao a propria serie da janela
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