####################################################################################################
# MODELO PERIODICOS AUTORREGRESSIVOS
####################################################################################################

#' Modelos \code{PAR}
#' 
#' Estimação e métodos de modelos da classe \code{PAR}
#' 
#' Modelos periódicos autorregressivos, possivelmente com parcela anual, similares aos do GEVAZP
#' 
#' @name modelos_par
NULL

# ESTIMACAO ----------------------------------------------------------------------------------------

#' \bold{Estimação}:
#' 
#' `...` pode conter qualquer, ou nenhum, argumento de [parmodels::par()] exceto por `serie`, que
#' sera passado automaticamente como `serie` desta funcao.
#' 
#' @param serie série para ajustar
#' @param ... demais argumentos de [parmodels::par()]
#' 
#' @return Objeto da classe `modprev` e subclasse `PAR`, uma lista de dois 
#'     elementos: `modelo` e `serie` contendo o modelo estimado e a série passada
#' 
#' @rdname modelos_par

PAR <- function(serie, ...) {
    cc <- match.call()
    fit <- parmodels::par(serie = serie, ...)
    new_modprevU(fit, serie, "PAR", list(call = cc))
}

# METODOS ------------------------------------------------------------------------------------------

#' @param object objeto com classes \code{c("PAR"|"parA", "modprev")} contendo modelo
#' @param n.ahead número de passos à frente para previsão
#' @param ... nao possui uso, existe apenas para consistencia com a generica
#' 
#' @return \code{predict} retorna uma série temporal multivariada contendo valor esperado e desvio 
#'     padrão da previsão \code{n.ahead} passos à frente;
#' 
#' @rdname modelos_par
#' 
#' @export

predict.PAR <- function(object, n.ahead, ...) {
    predict(object$modelo, n.ahead = n.ahead, ...)
}

#' @param newseries nova série com a qual atualizar o modelo
#' @param refit booleano indicando se o modelo deve ou não ser reajustado
#' @param ... para \code{update} nao tem uso, existe apenas para consistência com a genérica
#' 
#' @return \code{update} retorna modelo com novos dados e, caso \code{refit == TRUE}, reajustado. 
#'     Contrário à função de estimação, \code{update} já retorna o objeto da classe \code{modprev};
#' 
#' @rdname modelos_par
#' 
#' @export

update.PAR <- function(object, newseries, refit = FALSE, ...) {
    mod_atrs <- attr(object, "mod_atrs")

    if (refit) {
        call <- mod_atrs$call
        call$serie <- newseries
        object <- eval(call, parent.frame())
    } else {

        modelo <- object$modelo

        modelo$residuals <- NULL
        modelo$residuals <- as.numeric(newseries) - fitted(modelo)

        res <- residuals(modelo)
        sigma2 <- var(res, na.rm = TRUE)

        modelo$residuals <- res
        modelo$sigma2 <- sigma2

        object <- new_modprevU(modelo, newseries, "PAR", mod_atrs)
    }

    return(object)
}
