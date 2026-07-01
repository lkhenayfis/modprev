########################################### JM CONFIG ############################################

new_jm_config <- function(janela, passo, n.ahead, refit.cada, verbose, output.level) {
    structure(
        list(
            janela = janela,
            passo = passo,
            n.ahead = n.ahead,
            refit.cada = refit.cada,
            verbose = verbose,
            output.level = output.level
        ),
        class = "jm_config"
    )
}

validate_jm_config <- function(config) {

    if (!inherits(config, "jm_config")) {
        stop("'config' must be a jm_config object")
    }

    if (!is.numeric(config$janela) || any(config$janela <= 0)) {
        stop("'janela' must be positive numeric")
    }

    if (length(config$janela) > 2) {
        stop("'janela' must be scalar or length-2 vector")
    }

    if (!is.numeric(config$passo) || length(config$passo) != 1 || config$passo <= 0) {
        stop("'passo' must be a positive integer scalar")
    }

    if (!is.numeric(config$n.ahead) || length(config$n.ahead) != 1 || config$n.ahead <= 0) {
        stop("'n.ahead' must be a positive integer scalar")
    }

    if (length(config$refit.cada) != 1) {
        stop("'refit.cada' must be NA or a positive integer scalar")
    }

    if (!is.na(config$refit.cada)) {
        if (!is.numeric(config$refit.cada) || config$refit.cada <= 0) {
            stop("'refit.cada' must be NA or a positive integer scalar")
        }
    }

    if (!is.numeric(config$verbose) || length(config$verbose) != 1 ||
        !config$verbose %in% c(0, 1, 2)) {
        stop("'verbose' must be 0, 1, or 2")
    }

    if (!is.numeric(config$output.level) || length(config$output.level) != 1 ||
        !config$output.level %in% c(0, 1, 2)) {
        stop("'output.level' must be 0, 1, or 2")
    }

    invisible(config)
}

#' Create janelamovel Configuration Object
#'
#' Build a configuration object for janelamovel execution with validated parameters.
#'
#' @param janela Scalar or two-element vector specifying window size. If scalar,
#'     rolling window of that size. If vector, expanding window starting at
#'     `janela[1]` with initial width `janela[2]`.
#' @param passo Integer step size between windows. Default 1L.
#' @param n.ahead Integer number of steps ahead to forecast. Default 1L.
#' @param refit.cada Interval for model refitting. If NA (default), model is
#'     fit once and updated. If integer, model is refit every `refit.cada` windows.
#' @param verbose Integer verbosity level: 0 = silent, 1 = refit events,
#'     2 = all windows. Default 0.
#' @param output.level Integer output level controlling what each window returns:
#'     0 = forecast only (default), 1 = `list(pred, mod)` with `mod` present only
#'     on windows where the model was refitted (`NULL` otherwise), 2 = full
#'     `list(pred, mod, regdata)`.
#'
#' @return S3 object of class `jm_config` containing validated configuration
#'
#' @examples
#' # Rolling window of 60, predict 1 step ahead
#' config <- jm_config(janela = 60)
#'
#' # Expanding window, predict 12 steps
#' config <- jm_config(janela = c(1, 50), n.ahead = 12)
#'
#' # With refit and verbose output
#' config <- jm_config(janela = 60, passo = 6, refit.cada = 6, verbose = 2)
#'
#' @export

jm_config <- function(janela, passo = 1L, n.ahead = 1L, refit.cada = NA,
    verbose = 0L, output.level = 0L) {

    passo <- as.integer(passo)
    n.ahead <- as.integer(n.ahead)
    verbose <- as.integer(verbose)
    output.level <- as.integer(output.level)

    if (length(refit.cada) == 1 && is.numeric(refit.cada) && !is.na(refit.cada)) {
        refit.cada <- as.integer(refit.cada)
    }

    config <- new_jm_config(janela, passo, n.ahead, refit.cada, verbose, output.level)
    validate_jm_config(config)

    config
}

#' Print Method for jm_config
#'
#' @param x jm_config object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisible x
#' @export

print.jm_config <- function(x, ...) {
    cat("janelamovel Configuration\n\n")

    window_type <- if (length(x$janela) == 1) "Rolling" else "Expanding"
    cat("Window Type:", window_type, "\n")

    if (length(x$janela) == 1) {
        cat("  Window Size:", x$janela, "\n")
    } else {
        cat("  Start Position:", x$janela[1], "\n")
        cat("  Initial Width:", x$janela[2], "\n")
    }

    cat("Step Size:", x$passo, "\n")
    cat("Forecast Horizon:", x$n.ahead, "\n")

    if (is.na(x$refit.cada)) {
        cat("Refit Strategy: Fit once, update thereafter\n")
    } else {
        cat("Refit Interval:", x$refit.cada, "windows\n")
    }

    verbosity <- switch(as.character(x$verbose),
        "0" = "Silent",
        "1" = "Refit events only",
        "2" = "All windows"
    )
    cat("Verbosity:", verbosity, "\n")

    output_desc <- switch(as.character(x$output.level),
        "0" = "Forecasts only",
        "1" = "Forecasts + refitted models",
        "2" = "Full (forecasts, models, regdata)"
    )
    cat("Output Level:", output_desc, "\n")

    invisible(x)
}
