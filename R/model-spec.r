####################################################################################################
# MODEL SPECIFICATION CLASS
####################################################################################################

#' Model Specification Class
#'
#' Container for model metadata and function references used by the model registry system.
#'
#' A `model_spec` object encapsulates all information needed to register and use a model type
#' within the modprev framework. It stores references to estimation, prediction, and update
#' functions, along with metadata about model requirements.
#'
#' # Structure
#'
#' The specification includes:
#' - **tipo**: Model type identifier (character)
#' - **fit_fn**: Function for model estimation
#' - **predict_fn**: Function for generating predictions
#' - **update_fn**: Function for model updates
#' - **requires_regdata**: Whether model needs regression data
#' - **deps**: Required package dependencies
#' - **metadata**: Additional descriptive information
#'
#' @param tipo Character string uniquely identifying the model type. Must be non-empty and
#'     match the class name used in model objects (e.g., `"sarima"`, `"reg_lin"`).
#' @param fit_fn Function for model estimation. Should accept `serie` as first argument and
#'     return a `modprevU` object.
#' @param predict_fn Function for generating predictions. Should be an S3 method compatible
#'     with `predict()` generic.
#' @param update_fn Function for updating models with new data. Should be an S3 method
#'     compatible with `update()` generic.
#' @param requires_regdata Logical indicating whether the model requires regression data
#'     (`regdata` argument). Defaults to `FALSE`.
#' @param deps Character vector of required package names. Use empty character vector if
#'     no dependencies. Defaults to `character()`.
#' @param metadata Optional named list containing additional model information such as
#'     description, author, or version. Defaults to empty list.
#' @param spec A `model_spec` object to validate.
#'
#' @return Object of class `"model_spec"` containing model metadata and function references.
#'
#' @examples
#' dummy_fit <- function(serie, ...) {
#'     new_modprevU(list(data = serie), serie, "dummy", list())
#' }
#' dummy_predict <- function(object, n.ahead, ...) {
#'     ts(rep(0, n.ahead))
#' }
#' dummy_update <- function(object, newseries, refit, ...) {
#'     object
#' }
#'
#' spec <- model_spec(
#'     tipo = "dummy",
#'     fit_fn = dummy_fit,
#'     predict_fn = dummy_predict,
#'     update_fn = dummy_update,
#'     requires_regdata = FALSE,
#'     deps = character()
#' )
#'
#' spec_with_deps <- model_spec(
#'     tipo = "sarima",
#'     fit_fn = dummy_fit,
#'     predict_fn = dummy_predict,
#'     update_fn = dummy_update,
#'     requires_regdata = FALSE,
#'     deps = "forecast",
#'     metadata = list(description = "SARIMA model using forecast package")
#' )
#'
#' @family model_registry
#'
#' @export

model_spec <- function(tipo, fit_fn, predict_fn, update_fn,
    requires_regdata = FALSE, deps = character(),
    metadata = list()) {

    spec <- new_model_spec(tipo, fit_fn, predict_fn, update_fn,
        requires_regdata, deps, metadata)
    validate_model_spec(spec)
}

#' @rdname model_spec
new_model_spec <- function(tipo, fit_fn, predict_fn, update_fn,
    requires_regdata, deps, metadata) {
    structure(
        list(
            tipo = tipo,
            fit_fn = fit_fn,
            predict_fn = predict_fn,
            update_fn = update_fn,
            requires_regdata = requires_regdata,
            deps = deps,
            metadata = metadata
        ),
        class = "model_spec"
    )
}

#' @rdname model_spec
validate_model_spec <- function(spec) {
    stopifnot(
        inherits(spec, "model_spec"),
        is.character(spec$tipo),
        length(spec$tipo) == 1,
        nchar(spec$tipo) > 0,
        is.function(spec$fit_fn),
        is.function(spec$predict_fn),
        is.function(spec$update_fn),
        is.logical(spec$requires_regdata),
        length(spec$requires_regdata) == 1,
        is.character(spec$deps),
        is.list(spec$metadata)
    )

    invisible(spec)
}

#' Print Method for model_spec
#'
#' Displays model specification information in a readable format.
#'
#' @param x Object of class `"model_spec"`.
#' @param ... Additional arguments (unused).
#'
#' @return The input object `x`, invisibly.
#'
#' @export

print.model_spec <- function(x, ...) {
    cat("Model Specification:", x$tipo, "\n")
    cat("  Requires regdata:", x$requires_regdata, "\n")
    cat("  Dependencies:",
        if (length(x$deps) > 0) paste(x$deps, collapse = ", ") else "none",
        "\n")

    if (length(x$metadata) > 0) {
        cat("  Metadata:\n")
        for (name in names(x$metadata)) {
            cat("   ", name, ":", x$metadata[[name]], "\n")
        }
    }

    invisible(x)
}
