####################################################################################################
# MODELO SARIMA
####################################################################################################

#' Modelos \code{sarima}
#' 
#' Estimacao e metodos de modelos da classe \code{sarima}
#' 
#' @name modelos_sarima
NULL

# ESTIMACAO ----------------------------------------------------------------------------------------

#' @param serie serie para ajustar
#' 
#' @return \code{sarima} retorna modelo sarima estimado; 
#' 
#' @rdname modelos_sarima

sarima <- function(serie) {
    out <- auto.arima(serie, allowdrift = FALSE)
    return(out)
}

# METODOS ------------------------------------------------------------------------------------------

#' @param object objeto com classes \code{c("sarima", "mod_eol")} contendo modelo
#' @param n.ahead numero de passos a frente para previsao
#' @param ... demais argumentos passados a \code{\link[stats]{predict.Arima}}
#' 
#' @return \code{predict} serie temporal multivariada contendo valor esperado e desvio padrao de
#'     previsao;
#' 
#' @rdname modelos_sarima
#' 
#' @export

predict.sarima <- function(object, n.ahead, ...) {
    modelo <- object$modelo

    prev <- predict(modelo, n.ahead = n.ahead, ...)
    prev <- do.call(cbind, prev)
    colnames(prev) <- c("prev", "sd")

    return(prev)
}

#' @param newseries nova serie com a qual atualizar o modelo
#' @param refit booleano indicando se o modelo deve ou nao ser reajustado
#' @param ... existe apenas para consistência com a genérica
#' 
#' @return \code{update} retorna modelo com novos dados e, caso \code{refit == TRUE}, reajustado;
#' 
#' @rdname modelos_sarima
#' 
#' @export

update.sarima <- function(object, newseries, refit = FALSE, ...) {
    if(refit) {
        object <- estimamodelo(newseries, "sarima")
    } else {
        object$modelo <- Arima(newseries, model = object$modelo)
    }

    return(object)
}