####################################################################################################
# MODELO SARIMAX
####################################################################################################

#' Modelos \code{sarimax}
#' 
#' Estimação e métodos de modelos da classe \code{sarimax}
#' 
#' Esta especificação identifica e ajusta um modelo SARIMAX(p, d, q)(P, D, Q)^s através da função
#' \code{\link[forecast]{auto.arima}}, que realiza uma busca no espaço de hiperparâmetros de ordem
#' do modelo por aquele conjunto com menor BIC.
#' 
#' @name modelos_sarimax
NULL

# ESTIMACAO ----------------------------------------------------------------------------------------

#' @param serie serie para ajustar
#' @param regdata \code{data.frame}-like contendo variáveis explicativas
#' @param formula formula da regressão. Se for omitido, todas as variaveis em \code{regdata} serão
#'     utilizadas. So tem uso se \code{regdata} for passado
#' 
#' @return Objeto da classe \code{modprev} e subclasse \code{sarimax}, uma lista de dois elementos:
#'     \code{modelo} e \code{serie} contendo o modelo estimado e a série passada
#' 
#' @rdname modelos_sarimax

sarimax <- function(serie, regdata = NULL, formula = NULL) {

    if(is.null(formula)) {
        formula <- paste0(colnames(regdata), collapse = " + ")
        formula <- as.formula(paste0("~ ", formula))
    }

    Xreg <- model.frame(formula, data = regdata)
    Xreg <- data.matrix(Xreg)

    mod <- auto.arima(serie, xreg = Xreg)

    mod_atrs <- list(formula = formula)
    out <- new_modprev(mod, serie, "sarimax", mod_atrs)

    return(out)
}

# METODOS ------------------------------------------------------------------------------------------

#' @param object objeto com classes \code{c("sarimax", "modprev")} contendo modelo
#' @param newdata \code{data.frame}-like contendo variéveis explicativas fora da amostra
#' @param n.ahead número de passos à frente para previsão. Este argumento não é necessario, caso não
#'     seja informado a previsão sera feita tantos passos à frente quanto amostras em \code{newdata}
#' @param ... nao possui uso, existe apenas para consistencia com a generica
#' 
#' @return \code{predict} serie temporal multivariada contendo valor esperado e desvio padrao de
#'     previsão \code{n.ahead} passos à frente;
#' 
#' @rdname modelos_sarimax
#' 
#' @export

predict.sarimax <- function(object, newdata, n.ahead, ...) {
    modelo <- object$modelo

    if(missing(newdata)) stop("Forneca a variavel explicativa para previsao atraves do parametro newdata")

    if(!missing(n.ahead)) {
        regobs <- min(n.ahead, nrow(newdata))
        newdata <- newdata[seq(regobs), , drop = FALSE]
    }

    formula <- attr(object, "mod_atrs")$formula
    Xreg    <- model.frame(formula, data = newdata)
    Xreg    <- data.matrix(Xreg)

    prev   <- forecast(modelo, xreg = Xreg, level = c(.95), ...)
    prevsd <- with(prev, mean - lower) / qnorm(.975)

    prev <- cbind(prev$mean, prevsd)
    colnames(prev) <- c("prev", "sd")

    return(prev)
}
