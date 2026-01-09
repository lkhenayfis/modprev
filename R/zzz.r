####################################################################################################
# PACKAGE INITIALIZATION
####################################################################################################

#' Package Load Hook
#'
#' Initializes the model registry and registers all built-in models when the
#' package is loaded.
#'
#' @param libname Library name (unused, required by R)
#' @param pkgname Package name (unused, required by R)
#'
#' @return NULL invisibly
#'
#' @keywords internal

.onLoad <- function(libname, pkgname) {
    .init_registry()
    .register_builtin_models()
    invisible(NULL)
}

#' Package Unload Hook
#'
#' Clears the model registry when the package is unloaded.
#'
#' @param libpath Library path (unused, required by R)
#'
#' @return NULL invisibly
#'
#' @keywords internal

.onUnload <- function(libpath) {
    .clear_registry()
    invisible(NULL)
}

#' Register Built-in Models
#'
#' Internal function that registers all built-in model types with the registry.
#' Called during package initialization.
#'
#' @return NULL invisibly
#'
#' @keywords internal

.register_builtin_models <- function() {
    register_model(
        tipo = "sarima",
        fit_fn = sarima,
        predict_fn = predict.sarima,
        update_fn = update.sarima,
        requires_regdata = FALSE,
        deps = c("forecast"),
        metadata = list(
            description = "SARIMA(p,d,q)(P,D,Q)^s models via auto.arima",
            category = "univariate_ts"
        )
    )

    register_model(
        tipo = "ss_ar1_saz",
        fit_fn = ss_ar1_saz,
        predict_fn = predict.ss_ar1_saz,
        update_fn = update.ss_ar1_saz,
        requires_regdata = FALSE,
        deps = c("KFAS"),
        metadata = list(
            description = "State space AR(1) + seasonal model",
            category = "univariate_ts"
        )
    )

    register_model(
        tipo = "sarimax",
        fit_fn = sarimax,
        predict_fn = predict.sarimax,
        update_fn = update.sarimax,
        requires_regdata = TRUE,
        deps = c("forecast"),
        metadata = list(
            description = "SARIMAX models with exogenous regressors",
            category = "regression_ts"
        )
    )

    register_model(
        tipo = "reg_lin",
        fit_fn = reg_lin,
        predict_fn = predict.reg_lin,
        update_fn = update.reg_lin,
        requires_regdata = TRUE,
        deps = c("stats"),
        metadata = list(
            description = "Static linear regression via lm()",
            category = "regression_static"
        )
    )

    register_model(
        tipo = "reg_quant",
        fit_fn = reg_quant,
        predict_fn = predict.reg_quant,
        update_fn = update.reg_quant,
        requires_regdata = TRUE,
        deps = c("quantreg"),
        metadata = list(
            description = "Quantile regression via rq()",
            category = "regression_static"
        )
    )

    register_model(
        tipo = "ss_reg_din",
        fit_fn = ss_reg_din,
        predict_fn = predict.ss_reg_din,
        update_fn = update.ss_reg_din,
        requires_regdata = TRUE,
        deps = c("KFAS"),
        metadata = list(
            description = "Dynamic regression in state space form",
            category = "regression_dynamic"
        )
    )

    register_model(
        tipo = "GAM",
        fit_fn = GAM,
        predict_fn = predict.GAM,
        update_fn = update.GAM,
        requires_regdata = TRUE,
        deps = c("mgcv"),
        metadata = list(
            description = "Generalized Additive Models via mgcv",
            category = "ml_regression"
        )
    )

    register_model(
        tipo = "BOOST",
        fit_fn = BOOST,
        predict_fn = predict.BOOST,
        update_fn = update.BOOST,
        requires_regdata = TRUE,
        deps = c("mboost"),
        metadata = list(
            description = "Gradient boosting via mboost",
            category = "ml_regression"
        )
    )

    register_model(
        tipo = "LGBM",
        fit_fn = LGBM,
        predict_fn = predict.LGBM,
        update_fn = update.LGBM,
        requires_regdata = TRUE,
        deps = c("lightgbm"),
        metadata = list(
            description = "Light Gradient Boosting Machine",
            category = "ml_regression"
        )
    )

    invisible(NULL)
}
