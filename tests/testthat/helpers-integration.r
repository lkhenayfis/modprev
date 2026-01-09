####################################################################################################
# INTEGRATION TEST UTILITIES
####################################################################################################

#' Execute Function with Registered Models
#'
#' Helper to iterate over registered models and execute a test function
#' for each. Automatically filters by requirements (e.g., univariate only).
#'
#' @param fn Function to execute for each model (receives tipo as argument)
#' @param univariate_only Logical; test only models that don't require regdata
#' @param regression_only Logical; test only regression models
#' @param exclude Character vector of model types to exclude
#'
#' @return List of results (invisibly)
#'
#' @keywords internal

with_registered_models <- function(
    fn,
    univariate_only = FALSE,
    regression_only = FALSE,
    exclude = character(0)
) {
    models <- list_models()

    results <- list()

    for (tipo in models) {
        if (tipo %in% exclude) next

        spec <- get_model(tipo)

        if (univariate_only && spec$requires_regdata) next
        if (regression_only && !spec$requires_regdata) next

        results[[tipo]] <- fn(tipo)
    }

    invisible(results)
}

#' Test Complete Model Workflow
#'
#' Tests fit → predict → update workflow for a model type.
#'
#' @param tipo Model type
#' @param serie Time series (if NULL, generates one)
#' @param regdata Regression data (if NULL and needed, generates)
#' @param n.ahead Number of steps to predict
#'
#' @return List with fit, predict, and update results
#'
#' @keywords internal

test_model_workflow <- function(tipo, serie = NULL, regdata = NULL, n.ahead = 12) {
    spec <- get_model(tipo)

    if (is.null(serie)) {
        if (spec$requires_regdata) {
            data <- make_regression_data(n = 100, seed = 123)
            serie <- data$serie
            regdata <- data$regdata
        } else {
            serie <- make_univariate_series(seed = 123)
        }
    }

    if (spec$requires_regdata) {
        fit <- estimamodelo(serie, tipo, regdata = regdata)
    } else {
        fit <- estimamodelo(serie, tipo)
    }

    expect_modprev_structure(fit, tipo)

    if (spec$requires_regdata) {
        newdata <- make_newdata(n.ahead, regdata)
        pred <- predict(fit, newdata = newdata, n.ahead = n.ahead)
    } else {
        pred <- predict(fit, n.ahead = n.ahead)
    }

    expect_prediction_format(pred, n.ahead = n.ahead)

    newseries <- ts(
        as.numeric(serie) + rnorm(length(serie), sd = 0.1),
        start = start(serie),
        frequency = frequency(serie)
    )

    if (spec$requires_regdata) {
        updated <- update(fit, newseries, newregdata = regdata, refit = FALSE)
    } else {
        updated <- update(fit, newseries, refit = FALSE)
    }

    expect_update_preserves_class(fit, updated)

    list(fit = fit, pred = pred, updated = updated)
}

#' Validate Model Works with janelamovel
#'
#' Tests that a model type works correctly in rolling window estimation.
#'
#' @param tipo Model type
#' @param serie Time series (if NULL, generates one)
#' @param janela Window size
#' @param ... Additional arguments to janelamovel
#'
#' @return janelamovel object (invisibly)
#'
#' @keywords internal

expect_janelamovel_compatible <- function(tipo, serie = NULL, janela = 24, ...) {
    spec <- get_model(tipo)

    if (spec$requires_regdata) {
        testthat::skip(sprintf("%s requires regdata (not tested here)", tipo))
    }

    if (is.null(serie)) {
        serie <- make_univariate_series(n = 100, seed = 123)
    }

    jan <- janelamovel(serie, tipo, janela = janela, ...)

    testthat::expect_type(jan, "list")
    testthat::expect_true(length(jan) > 0)

    for (window_result in jan) {
        testthat::expect_s3_class(window_result, "ts")
    }

    invisible(jan)
}

#' Validate Model Works with modprevP
#'
#' Tests that a model type can be wrapped in periodic model.
#'
#' @param tipo Model type
#' @param serie Time series (if NULL, generates one)
#' @param ... Additional arguments to estimamodelo
#'
#' @return modprevP object (invisibly)
#'
#' @keywords internal

expect_periodic_compatible <- function(tipo, serie = NULL, ...) {
    if (is.null(serie)) {
        serie <- make_univariate_series(n = 120, frequency = 12, seed = 123)
    }

    mod <- estimamodelo(serie, tipo, periodico = TRUE, ...)

    testthat::expect_s3_class(mod, "modprevP")
    testthat::expect_s3_class(mod, "modprev")
    expect_modprev_structure(mod, tipo, subclass = "modprevP")

    pred <- predict(mod, n.ahead = 12)
    expect_prediction_format(pred, n.ahead = 12)

    invisible(mod)
}
