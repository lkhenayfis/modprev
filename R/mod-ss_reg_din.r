####################################################################################################
# MODELO REGRESSAO DINAMICA SIMPLES
####################################################################################################

#' Modelos \code{ss_reg_din}
#' 
#' Estimacao e metodos de modelos da classe \code{ss_reg_din}
#' 
#' @name modelos_ss_reg_din
NULL

# ESTIMACAO ----------------------------------------------------------------------------------------

#' @param serie serie para ajustar
#' @param regdata vetor, matriz ou data.frame contendo variaveis explicativas
#' 
#' @return \code{ss_reg_din} retorna modelo de regressao dinamica simples
#' 
#' @rdname modelos_ss_reg_din

ss_reg_din <- function(serie, regdata, ...) {

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

# METODOS ------------------------------------------------------------------------------------------

#' @param object objeto com classes \code{c("ss_reg_din", "mod_eol")} contendo modelo
#' @param newdata vetor, matriz ou data.frame contendo variaveis explicativas fora da amostra
#' @param n.ahead numero de passos a frente para previsao
#' 
#' @return \code{predict} serie temporal multivariada contendo valor esperado e desvio padrao de
#'     previsao;
#' 
#' @rdname modelos_ss_reg_din
#' 
#' @export

predict.ss_reg_din <- function(object, newdata, n.ahead, ...) {
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

#' @details 
#' 
#' \bold{Update}:
#' 
#' A atualizacao de modelos \code{ss_reg_din} sempre vai checar se o modelo passado foi estimado
#' corretamente. Como modelos em espaco de estados dependem bastante de inicializacao, as vezes nao
#' da pra estimar direito. Nesses casos ele tenta reestimar o modelo independentemente de 
#' \code{refit}
#' 
#' @param newseries nova serie com a qual atualizar o modelo @param newregdata vetor, matriz ou
#' data.frame contendo variaveis explicativas na nova amostra @param refit booleano indicando se o
#' modelo deve ou nao ser reajustado
#' 
#' @return \code{update} retorna modelo com novos dados e, caso \code{refit == TRUE}, reajustado;
#' 
#' @rdname modelos_ss_reg_din
#' 
#' @export

update.ss_reg_din <- function(object, newseries, newregdata, refit = FALSE, ...) {

    if(refit) {
        object <- estimamodelo(newseries, "ss_reg_din", regdata = newregdata)
    } else {

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
        modelo <- SSModel(
            newseries ~ SSMregression(~ xvar, data = newregdata, Q = Qmat), H = Hmat)

        object$modelo <- modelo
    }

    return(object)
}