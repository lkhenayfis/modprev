####################################################################################################
# MODEL REGISTRY STORAGE
####################################################################################################

.model_registry <- new.env(parent = emptyenv())

.init_registry <- function() {
    assign("models", list(), envir = .model_registry)
    invisible(NULL)
}

.clear_registry <- function() {
    .init_registry()
    invisible(NULL)
}

.lookup <- function(tipo) {
    if (!exists("models", envir = .model_registry)) .init_registry()
    .model_registry$models[[tipo]]
}

#' Model Registry
#'
#' Access the global model registry containing all registered model specifications.
#'
#' @return Object of class `"model_registry"` with component `models`: a named list of
#'     [model_spec] objects.
#'
#' @examples
#' get_registry()
#'
#' @seealso [register_model()], [get_model()], [list_models()], [model_spec]
#'
#' @family model_registry
#'
#' @export

get_registry <- function() {
    if (!exists("models", envir = .model_registry)) .init_registry()
    models <- .model_registry$models

    structure(
        list(models = models),
        class = "model_registry"
    )
}

#' Print Method for model_registry
#'
#' @param x Object of class `"model_registry"`.
#' @param ... Additional arguments (unused).
#'
#' @return The input object `x`, invisibly.
#'
#' @export

print.model_registry <- function(x, ...) {
    n_models <- length(x$models)

    cat("Model Registry\n")
    cat("==============\n")
    cat("Registered models:", n_models, "\n\n")

    if (n_models > 0) {
        tipos <- names(x$models)
        requires_reg <- vapply(x$models, function(m) m$requires_regdata, logical(1))

        cat("Available models:\n")
        for (i in seq_along(tipos)) {
            regdata_flag <- if (requires_reg[i]) "*" else " "
            cat(sprintf("  %s %s\n", regdata_flag, tipos[i]))
        }
        cat("\n* Requires regdata argument\n")
    }

    invisible(x)
}

.validate_model_functions <- function(spec) {
    fit_args <- names(formals(spec$fit_fn))
    if (!"serie" %in% fit_args) {
        stop(sprintf(
            "fit_fn for model '%s' must have 'serie' argument",
            spec$tipo
        ))
    }
    if (!"..." %in% fit_args) {
        warning(sprintf(
            "fit_fn for model '%s' should have '...' argument for flexibility",
            spec$tipo
        ))
    }

    invisible(NULL)
}

#' Check if Model is Registered
#'
#' Tests whether a model type exists in the global model registry.
#'
#' @param tipo Character string identifying the model type (e.g., `"sarima"`, `"reg_lin"`).
#'
#' @return Logical value: `TRUE` if the model is registered, `FALSE` otherwise.
#'
#' @examples
#' is_registered("sarima")
#' is_registered("nonexistent_model")
#'
#' @seealso [register_model()], [get_registry()]
#'
#' @family model_registry
#'
#' @export

is_registered <- function(tipo) {
    if (!is.character(tipo) || length(tipo) != 1) {
        stop("'tipo' must be a single character string")
    }

    !is.null(.lookup(tipo))
}

#' Register Model with Registry
#'
#' Adds a model specification to the global model registry, making it available for use
#' with [estimamodelo()].
#'
#' `fit_fn` must have a `serie` argument and return a `modprevU` object. Prediction and
#' update dispatch use plain S3 method dispatch and are not stored in the registry.
#'
#' @param tipo Character string uniquely identifying the model type.
#' @param fit_fn Function for model estimation. Must have `serie` argument.
#' @param requires_regdata Logical. Whether the model requires a `regdata` argument.
#'     Defaults to `FALSE`.
#' @param deps Character vector of required package names. Defaults to `character()`.
#' @param metadata Named list of additional model information. Defaults to `list()`.
#' @param overwrite Logical. Whether to replace an existing registration. Defaults to `FALSE`.
#'
#' @return The created [model_spec] object, invisibly.
#'
#' @examples
#' my_fit <- function(serie, ...) {
#'     new_modprevU(list(mean = mean(serie)), serie, "my_model", list())
#' }
#'
#' register_model("my_model", my_fit, metadata = list(description = "Mean model"))
#' is_registered("my_model")
#'
#' register_model("my_reg_model", my_fit, requires_regdata = TRUE, deps = "mgcv")
#'
#' @seealso [is_registered()], [model_spec], [get_registry()]
#'
#' @family model_registry
#'
#' @export

register_model <- function(tipo, fit_fn,
    requires_regdata = FALSE, deps = character(),
    metadata = list(), overwrite = FALSE) {

    if (!overwrite && is_registered(tipo)) {
        stop(sprintf(
            "Model '%s' is already registered. Use overwrite = TRUE to replace.",
            tipo
        ))
    }

    spec <- model_spec(
        tipo = tipo,
        fit_fn = fit_fn,
        requires_regdata = requires_regdata,
        deps = deps,
        metadata = metadata
    )

    .validate_model_functions(spec)

    .model_registry$models[[tipo]] <- spec

    invisible(spec)
}

#' Retrieve Model Specification from Registry
#'
#' Retrieves a registered model's specification. Used internally by [estimamodelo()]
#' but can be called directly to inspect or use a model's functions programmatically.
#'
#' Set `error = FALSE` to return `NULL` instead of erroring when the model is not found.
#'
#' @param tipo Character string identifying the model type (e.g., `"sarima"`, `"reg_lin"`).
#' @param error Logical. Whether to throw an error if the model is not found. Defaults to
#'     `TRUE`. Set to `FALSE` to return `NULL` for missing models.
#'
#' @return A [model_spec] object if the model is registered, `NULL` if not found and
#'     `error = FALSE`.
#'
#' @examples
#' dummy_fit <- function(serie, ...) {
#'     new_modprevU(list(data = serie), serie, "ex_get", list())
#' }
#'
#' register_model("ex_get", dummy_fit)
#' spec <- get_model("ex_get")
#' get_model("nonexistent", error = FALSE)
#'
#' @seealso [register_model()], [is_registered()], [list_models()]
#'
#' @family model_registry
#'
#' @export

get_model <- function(tipo, error = TRUE) {
    if (!is.character(tipo) || length(tipo) != 1) {
        stop("'tipo' must be a single character string")
    }

    spec <- .lookup(tipo)

    if (is.null(spec) && error) {
        registered <- names(.model_registry$models)
        msg <- sprintf(
            "Model '%s' is not registered.\nAvailable models: %s",
            tipo,
            if (length(registered) > 0) paste(registered, collapse = ", ") else "none"
        )
        stop(msg, call. = FALSE)
    }

    spec
}

#' List Registered Models
#'
#' Returns names and optionally detailed information about all registered models.
#'
#' @param details Logical. Whether to return detailed information. Defaults to `FALSE`.
#'     If `FALSE`, returns a sorted character vector of model names. If `TRUE`, returns a
#'     data.frame with columns `tipo`, `requires_regdata`, `deps`, and `description`.
#'
#' @return Sorted character vector of model names when `details = FALSE`, or a data.frame
#'     with model metadata when `details = TRUE`.
#'
#' @examples
#' dummy_fit <- function(serie, ...) {
#'     new_modprevU(list(data = serie), serie, "ex_list", list())
#' }
#'
#' register_model("ex_list", dummy_fit)
#' list_models()
#' list_models(details = TRUE)
#'
#' @seealso [get_model()], [register_model()], [get_registry()]
#'
#' @family model_registry
#'
#' @export

list_models <- function(details = FALSE) {
    if (!exists("models", envir = .model_registry)) .init_registry()
    models <- .model_registry$models
    tipos <- if (is.null(names(models))) character(0) else names(models)

    if (!details) return(sort(tipos))

    if (length(tipos) == 0) {
        return(data.frame(
            tipo = character(),
            requires_regdata = logical(),
            deps = character(),
            description = character(),
            stringsAsFactors = FALSE
        ))
    }

    data.frame(
        tipo = tipos,
        requires_regdata = vapply(models, function(m) m$requires_regdata, logical(1)),
        deps = vapply(models, function(m) {
            if (length(m$deps) > 0) paste(m$deps, collapse = ", ") else ""
        }, character(1)),
        description = vapply(models, function(m) {
            desc <- m$metadata$description
            if (is.null(desc)) "" else as.character(desc)
        }, character(1)),
        stringsAsFactors = FALSE,
        row.names = NULL
    )
}
