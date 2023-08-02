####################################################################################################
# FUNCOES DE VISUALIZACAO
####################################################################################################

#' Plot De Objetos \code{modprev}
#' 
#' Metodo para plotar ajustes \code{modprev}
#' 
#' @param x objeto para plotar
#' @param ... demais parametros repassados para \code{plot}
#' 
#' @return plot da serie e ajuste realizado
#' 
#' @export

plot.modprev <- function(x, ...) {

    # Ajuste do modelo
    args <- c(list(object = x$modelo, filter = TRUE))
    fitt <- do.call(fitted, args)

    # Plota
    plot(x$serie, panel.first = grid(col = "grey85"),
        xlab = "Tempo", ylab = "Geracao", ...)
    if(!("main" %in% names(list(...)))) title(main = attr(x, "tipo"))
    lines(fitt, col = 4, lty = 2)
}

plot.modprev_acf <- function(x, ...) {

    lags <- seq_along(x$phi)
    acfs <- x$phi
    conf <- qnorm((1 + .95) / 2) / sqrt(x$n.used) # intervalo 95%
    ylim <- range(-conf, conf, range(acfs))

    ylab <- ifelse(inherits(x, "parcial"), "FAC Parcial Periodica", "FAC Condicional Periodica")

    plot(lags, acfs, type = "h", ylim = ylim, ylab = ylab, ...)
    if (!("main" %in% ...names())) title(paste0("Periodo m = ", x$m))
    abline(h = 0)
    abline(h = c(-conf, conf), lty = 2, col = "blue")
}