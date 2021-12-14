####################################################################################################
# FUNCOES DE VISUALIZACAO
####################################################################################################

#' Plot De Objetos \code{mod_eol}
#' 
#' Metodo para plotar ajustes \code{mod_eol}
#' 
#' @param fit objeto para plotar
#' @param ... demais parametros repassados para \code{plot}
#' 
#' @return plot da serie e ajuste realizado
#' 
#' @export

plot.mod_eol <- function(x, ...) {

    # Ajuste do modelo
    args <- c(list(object = x$modelo, filter = TRUE))
    fitt <- do.call(fitted, args)

    # Plota
    plot(x$serie, panel.first = grid(col = "grey85"),
        xlab = "Tempo", ylab = "Geracao", ...)
    if(!("main" %in% names(list(...)))) title(main = attr(x, "tipo"))
    lines(fitt, col = 4, lty = 2)
}