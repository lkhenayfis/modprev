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

sarimax <- function(serie, regdata, formula) {

    if(missing(regdata)) stop("Forneca a variavel explicativa atraves do parametro 'regdata'")

    if(missing(formula)) formula <- expandeformula(regdata)

    Xreg <- expandexreg(regdata, formula)

    mod <- auto.arima(serie, xreg = Xreg)

    mod_atrs <- list(formula = formula)
    out <- new_modprevU(mod, serie, "sarimax", mod_atrs)

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

    if(missing(newdata)) stop("Forneca a variavel explicativa para previsao atraves do parametro 'newdata'")

    if(!missing(n.ahead)) {
        regobs <- min(n.ahead, nrow(newdata))
        newdata <- newdata[seq(regobs), , drop = FALSE]
    }

    Xreg <- expandexreg(newdata, formula)

    prev   <- forecast(modelo, xreg = Xreg, level = c(.95), ...)
    prevsd <- with(prev, mean - lower) / qnorm(.975)

    prev <- cbind(prev$mean, prevsd)
    colnames(prev) <- c("prev", "sd")

    return(prev)
}

#' 
#' @param newseries nova série com a qual atualizar o modelo
#' @param newregdata \code{data.frame}-like contendo variáveis explicativas na nova amostra
#' @param refit booleano indicando se o modelo deve ou nao ser reajustado
#' @param ... nao possui uso, existe apenas para consistencia com a generica
#' 
#' @return \code{update} retorna modelo com novos dados e, caso \code{refit == TRUE}, reajustado. 
#'     Contrário à função de estimação, \code{update} já retorna o objeto da classe \code{modprev};
#' 
#' @rdname modelos_sarimax
#' 
#' @export

update.sarimax <- function(object, newseries, newregdata, refit = FALSE, ...) {

    mod_atrs <- attr(object, "mod_atrs")

    if(refit) {
        formula <- mod_atrs$formula
        object <- estimamodelo(newseries, "sarimax", regdata = newregdata, formula = formula)
    } else {

        if(missing(newregdata)) stop("Forneca nova variavel explicativa atraves do parametro 'newregdata'")

        Xreg <- expandexreg(newregdata, formula)

        newseries <- if(is.ts(newseries)) newseries else ts(newseries)
        modelo <- Arima(newseries, xreg = Xreg, model = object$modelo)

        object <- new_modprev(modelo, newseries, "sarima")
    }

    return(object)
}

# HELPERS ------------------------------------------------------------------------------------------

expandexreg <- function(data, formula) {
    formula <- formula
    Xreg <- model.frame(formula, data = data)
    Xreg <- data.matrix(Xreg)

    return(Xreg)
}