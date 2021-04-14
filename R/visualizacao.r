####################################################################################################
# FUNCOES DE VISUALIZACAO
####################################################################################################

#' Plot de fit_TR
#' 
#' Metodo para plotar ajustes fit_TR
#' 
#' @param fit [objeto fit_TR] objeto para plotar
#' @param ... demais parametros repassados para \code{plot}
#' 
#' @value plot da serie e ajuste realizado

plot.fit_TR <- function(fit, ..., legend = TRUE) {

    # Ajuste do modelo
    args <- c(list(object = fit$modelo, filter = TRUE))
    fitt <- do.call(fitted, args)

    # Plota
    plot(fit$serie_in, panel.first = grid(col = "grey85"),
        xlab = "Tempo", ylab = "Geracao", ...)
    lines(fitt, col = 4, lty = 2)
    if(legend) {
        legend("topright", inset = 0.02, legend = c("Serie", "Ajuste"), lty = c(1,2), col = c(1, 4))
    }
}