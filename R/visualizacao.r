####################################################################################################
# FUNCOES DE VISUALIZACAO
####################################################################################################

#' Plot de mod_eol
#' 
#' Metodo para plotar ajustes mod_eol
#' 
#' @param fit [objeto mod_eol] objeto para plotar
#' @param ... demais parametros repassados para \code{plot}
#' 
#' @value plot da serie e ajuste realizado

plot.mod_eol <- function(fit, ..., legend = TRUE) {

    # Ajuste do modelo
    args <- c(list(object = fit$modelo, filter = TRUE))
    fitt <- do.call(fitted, args)

    # Plota
    plot(fit$serie_in, panel.first = grid(col = "grey85"),
        xlab = "Tempo", ylab = "Geracao", ...)
    if(!("main" %in% names(list(...)))) title(main = attr(fit, "tipo"))
    lines(fitt, col = 4, lty = 2)
    if(legend) {
        legend("topright", inset = 0.02, legend = c("Serie", "Ajuste"), lty = c(1,2), col = c(1, 4))
    }
}