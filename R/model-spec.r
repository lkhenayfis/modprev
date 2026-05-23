####################################################################################################
# MODEL SPECIFICATION CLASS
####################################################################################################

#' Model Specification Class
#'
#' Container for model metadata and function references used by the model registry system.
#'
#' Stores the estimation function and metadata for a model type. Prediction and update
#' dispatch happen via plain S3 method dispatch and are not stored here.
#'
#' @param tipo Character string uniquely identifying the model type. Must be non-empty and
#'     match the class name used in model objects (e.g., `"sarima"`, `"reg_lin"`).
#' @param fit_fn Function for model estimation. Should accept `serie` as first argument and
#'     return a `modprevU` object.
#' @param requires_regdata Logical indicating whether the model requires regression data
#'     (`regdata` argument). Defaults to `FALSE`.
#' @param deps Character vector of required package names. Defaults to `character()`.
#' @param metadata Optional named list with additional model information (description, author,
#'     version, etc.). Defaults to empty list.
#' @param spec A `model_spec` object to validate.
#'
#' @return Object of class `"model_spec"` with elements: `tipo`, `fit_fn`,
#'     `requires_regdata`, `deps`, `metadata`.
#'
#' @examples
#' dummy_fit <- function(serie, ...) {
#'     new_modprevU(list(data = serie), serie, "dummy", list())
#' }
#'
#' spec <- model_spec(
#'     tipo = "dummy",
#'     fit_fn = dummy_fit,
#'     deps = "forecast",
#'     metadata = list(description = "A dummy model")
#' )
#'
#' @family model_registry
#'
#' @export

model_spec <- function(tipo, fit_fn,
    requires_regdata = FALSE, deps = character(),
    metadata = list()) {

    spec <- new_model_spec(tipo, fit_fn, requires_regdata, deps, metadata)
    validate_model_spec(spec)
}

#' @rdname model_spec
new_model_spec <- function(tipo, fit_fn, requires_regdata, deps, metadata) {
    structure(
        list(
            tipo = tipo,
            fit_fn = fit_fn,
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
