####################################################################################################
# CLASSE ABSTRATA CONTENDO MODELOS HIERARQUICOS
####################################################################################################

# CONSTRUTOR ---------------------------------------------------------------------------------------

estimamodelo_H <- function(serie, tipo1, tipo2, ...) {

    modelo1 <- estimamodelo_U(serie, tipo1, ...)
    modelo2 <- estimamodelo_U(residuals(modelo1), tipo2)

    new_modprevH(modelo1, modelo2, serie)
}

# METODOS ------------------------------------------------------------------------------------------

new_modprevH <- function(modelo1, modelo2, serie) {
    new <- list(
        modelos = list(outer = modelo1, inner = modelo2),
        serie = serie
    )
    class(new) <- c("modprevH", "modprev")

    return(new)
}

predict.modprevH <- function(object, n.ahead, ...) {
    pred1 <- predict(object$modelos$outer, n.ahead, ...)
    pred2 <- predict(object$modelos$inner, n.ahead = nrow(pred1))

    pred <- pred1[, 1] + pred2[, 1]

    return(pred)
}

update.modprevH <- function(object, newseries, refit = FALSE, ...) {
    newouter <- update(object$modelos$outer, newseries, refit, ...)
    newinner <- update(object$modelos$inner, residuals(newouter))

    new_modprevH(newouter, newinner, newseries)
}