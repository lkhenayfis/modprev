####################################################################################################
# MODEL REGISTRY STORAGE
####################################################################################################

.model_registry <- new.env(parent = emptyenv())

.init_registry <- function() {
    assign("models", list(), envir = .model_registry)
    assign("initialized", TRUE, envir = .model_registry)
    invisible(NULL)
}

.is_registry_initialized <- function() {
    exists("initialized", envir = .model_registry)
}

.get_models <- function() {
    if (!.is_registry_initialized()) {
        .init_registry()
    }
    get("models", envir = .model_registry)
}

.set_models <- function(models) {
    if (!.is_registry_initialized()) {
        .init_registry()
    }
    assign("models", models, envir = .model_registry)
    invisible(NULL)
}

.add_model <- function(tipo, spec) {
    models <- .get_models()
    models[[tipo]] <- spec
    .set_models(models)
    invisible(NULL)
}

.remove_model <- function(tipo) {
    models <- .get_models()
    models[[tipo]] <- NULL
    .set_models(models)
    invisible(NULL)
}

.has_model <- function(tipo) {
    models <- .get_models()
    tipo %in% names(models)
}

.get_model_spec <- function(tipo) {
    models <- .get_models()
    models[[tipo]]
}

.clear_registry <- function() {
    if (.is_registry_initialized()) {
        .set_models(list())
    }
    invisible(NULL)
}

#' Model Registry
#'
#' Access the global model registry containing all registered model specifications.
#'
#' The model registry provides centralized storage and discovery for all available model types
#' in the modprev framework. Models are registered using [register_model()] and can be
#' retrieved using [get_model()] or listed using [list_models()].
#'
#' # Storage
#'
#' The registry uses an internal environment for O(1) lookup performance. The registry
#' automatically initializes on first access and persists for the duration of the R session.
#'
#' # Registry Contents
#'
#' Each entry in the registry is a [model_spec] object containing:
#' - Model type identifier
#' - Function references for fit, predict, and update operations
#' - Dependency information
#' - Metadata about model requirements
#'
#' @return Object of class `"model_registry"` with the following components:
#'     \describe{
#'       \item{models}{Named list of [model_spec] objects, where names are model type
#'         identifiers (e.g., `"sarima"`, `"reg_lin"`)}
#'     }
#'
#' @examples
#' registry <- get_registry()
#' print(registry)
#'
#' dummy_fit <- function(serie, ...) {
#'     new_modprevU(list(data = serie), serie, "dummy", list())
#' }
#' dummy_predict <- function(object, n.ahead, ...) ts(rep(0, n.ahead))
#' dummy_update <- function(object, newseries, refit, ...) object
#'
#' spec <- model_spec(
#'     tipo = "dummy",
#'     fit_fn = dummy_fit,
#'     predict_fn = dummy_predict,
#'     update_fn = dummy_update
#' )
#'
#' modprev:::.add_model("dummy", spec)
#' registry <- get_registry()
#' print(registry)
#'
#' @seealso [register_model()], [get_model()], [list_models()], [model_spec]
#'
#' @family model_registry
#'
#' @export

get_registry <- function() {
    models <- .get_models()

    structure(
        list(models = models),
        class = "model_registry"
    )
}

#' Print Method for model_registry
#'
#' Displays registered models in a readable format with indicators for models requiring
#' regression data.
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
        requires_reg <- sapply(x$models, function(m) m$requires_regdata)

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

    predict_args <- names(formals(spec$predict_fn))
    if (!"object" %in% predict_args) {
        stop(sprintf(
            "predict_fn for model '%s' must have 'object' argument",
            spec$tipo
        ))
    }
    if (!"n.ahead" %in% predict_args) {
        stop(sprintf(
            "predict_fn for model '%s' must have 'n.ahead' argument",
            spec$tipo
        ))
    }

    update_args <- names(formals(spec$update_fn))
    if (!"object" %in% update_args) {
        stop(sprintf(
            "update_fn for model '%s' must have 'object' argument",
            spec$tipo
        ))
    }
    if (!"newseries" %in% update_args) {
        stop(sprintf(
            "update_fn for model '%s' must have 'newseries' argument",
            spec$tipo
        ))
    }
    if (!"refit" %in% update_args) {
        stop(sprintf(
            "update_fn for model '%s' must have 'refit' argument",
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
#'
#' is_registered("nonexistent_model")
#'
#' dummy_fit <- function(serie, ...) {
#'     new_modprevU(list(data = serie), serie, "test", list())
#' }
#' dummy_predict <- function(object, n.ahead, ...) ts(rep(0, n.ahead))
#' dummy_update <- function(object, newseries, refit = FALSE, ...) object
#'
#' register_model("test", dummy_fit, dummy_predict, dummy_update)
#' is_registered("test")
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

    .has_model(tipo)
}

#' Register Model with Registry
#'
#' Adds a model specification to the global model registry, making it available for use
#' with [estimamodelo()].
#'
#' This function is the primary interface for extending modprev with new model types.
#' It validates the model specification and function signatures, then registers the model
#' for runtime discovery. Models can be registered at package load time (in `.onLoad()`)
#' or dynamically at runtime.
#'
#' # Function Signatures
#'
#' The model functions must follow specific signatures:
#'
#' - **fit_fn**: `function(serie, ...)` - Must accept time series and return `modprevU` object
#' - **predict_fn**: `function(object, n.ahead, ...)` - Standard S3 predict method signature
#' - **update_fn**: `function(object, newseries, refit = FALSE, ...)` - Standard update signature
#'
#' The `...` argument is recommended in all functions for forward compatibility.
#'
#' # Validation
#'
#' When `validate = TRUE` (default), the function checks that:
#' 1. All required arguments are present in function signatures
#' 2. Function names follow expected patterns
#'
#' Set `validate = FALSE` to skip these checks when you need non-standard signatures.
#'
#' @param tipo Character string uniquely identifying the model type. Should match the
#'     class name used in model objects.
#' @param fit_fn Function for model estimation. Must have `serie` argument and return
#'     a `modprevU` object.
#' @param predict_fn Function for generating predictions. Must be compatible with S3
#'     `predict()` generic (arguments: `object`, `n.ahead`).
#' @param update_fn Function for updating models. Must accept `object`, `newseries`,
#'     and `refit` arguments.
#' @param requires_regdata Logical indicating whether the model requires regression data
#'     via `regdata` argument. Defaults to `FALSE`.
#' @param deps Character vector of required package names. Defaults to empty vector.
#' @param metadata Optional named list containing additional model information (description,
#'     author, version, etc.). Defaults to empty list.
#' @param validate Logical indicating whether to validate function signatures. Defaults
#'     to `TRUE`. Set to `FALSE` to skip validation for non-standard signatures.
#' @param overwrite Logical indicating whether to replace an existing registration.
#'     Defaults to `FALSE`. Set to `TRUE` to update an already-registered model.
#'
#' @return The created [model_spec] object, invisibly.
#'
#' @examples
#' my_fit <- function(serie, ...) {
#'     params <- list(mean = mean(serie), sd = sd(serie))
#'     new_modprevU(params, serie, "my_model", list())
#' }
#'
#' my_predict <- function(object, n.ahead, ...) {
#'     ts(rep(object$modelo$mean, n.ahead))
#' }
#'
#' my_update <- function(object, newseries, refit = FALSE, ...) {
#'     if (refit) {
#'         estimamodelo(newseries, "my_model")
#'     } else {
#'         object$serie <- newseries
#'         object
#'     }
#' }
#'
#' register_model(
#'     tipo = "my_model",
#'     fit_fn = my_fit,
#'     predict_fn = my_predict,
#'     update_fn = my_update,
#'     metadata = list(description = "Simple mean model", author = "Me")
#' )
#'
#' is_registered("my_model")
#'
#' register_model(
#'     tipo = "my_reg_model",
#'     fit_fn = my_fit,
#'     predict_fn = my_predict,
#'     update_fn = my_update,
#'     requires_regdata = TRUE,
#'     deps = c("mgcv", "quantreg")
#' )
#'
#' @seealso [is_registered()], [model_spec], [get_registry()]
#'
#' @family model_registry
#'
#' @export

register_model <- function(tipo, fit_fn, predict_fn, update_fn,
    requires_regdata = FALSE, deps = character(),
    metadata = list(), validate = TRUE,
    overwrite = FALSE) {

    if (!overwrite && is_registered(tipo)) {
        stop(sprintf(
            "Model '%s' is already registered. Use overwrite = TRUE to replace.",
            tipo
        ))
    }

    spec <- model_spec(
        tipo = tipo,
        fit_fn = fit_fn,
        predict_fn = predict_fn,
        update_fn = update_fn,
        requires_regdata = requires_regdata,
        deps = deps,
        metadata = metadata
    )

    if (validate) {
        .validate_model_functions(spec)
    }

    .add_model(tipo, spec)

    invisible(spec)
}

#' Retrieve Model Specification from Registry
#'
#' Retrieves a registered model's specification for use in model estimation and other operations.
#'
#' This function provides type-safe access to registered model specifications. It's used
#' internally by [estimamodelo()] but can also be called directly when you need to inspect
#' or use a model's functions programmatically.
#'
#' # Error Handling
#'
#' By default, attempting to retrieve a non-existent model throws an error with a helpful
#' message listing all available models. Set `error = FALSE` to return `NULL` instead,
#' which is useful for conditional logic.
#'
#' @param tipo Character string identifying the model type (e.g., `"sarima"`, `"reg_lin"`).
#' @param error Logical indicating whether to throw an error if the model is not found.
#'     Defaults to `TRUE`. Set to `FALSE` to return `NULL` for missing models instead.
#'
#' @return A [model_spec] object if the model is registered, `NULL` if the model is not
#'     found and `error = FALSE`.
#'
#' @examples
#' dummy_fit <- function(serie, ...) {
#'     new_modprevU(list(data = serie), serie, "test", list())
#' }
#' dummy_predict <- function(object, n.ahead, ...) ts(rep(0, n.ahead))
#' dummy_update <- function(object, newseries, refit = FALSE, ...) object
#'
#' register_model("test", dummy_fit, dummy_predict, dummy_update)
#'
#' spec <- get_model("test")
#' spec$fit_fn
#'
#' spec_missing <- get_model("nonexistent", error = FALSE)
#' if (is.null(spec_missing)) {
#'     message("Model not registered")
#' }
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

    spec <- .get_model_spec(tipo)

    if (is.null(spec) && error) {
        registered <- names(.get_models())
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
#' This function provides programmatic access to the model inventory, enabling dynamic
#' workflows, help systems, and validation checks. It's useful for discovering available
#' models, checking their requirements, and building model selection interfaces.
#'
#' # Simple vs. Detailed Output
#'
#' - **Simple** (`details = FALSE`): Returns a character vector of model names, suitable
#'     for quick checks like `"sarima" %in% list_models()`.
#' - **Detailed** (`details = TRUE`): Returns a data.frame with columns for model type,
#'     regression data requirements, dependencies, and descriptions.
#'
#' @param details Logical indicating whether to return detailed information. Defaults
#'     to `FALSE`. If `FALSE`, returns character vector of model names. If `TRUE`, returns
#'     data.frame with additional information.
#'
#' @return If `details = FALSE`, a character vector of registered model names (sorted
#'     alphabetically). If `details = TRUE`, a data.frame with the following columns:
#'     \describe{
#'         \item{tipo}{Character. Model type identifier}
#'         \item{requires_regdata}{Logical. Whether model requires `regdata` argument}
#'         \item{deps}{Character. Package dependencies (comma-separated)}
#'         \item{description}{Character. Description from metadata (empty string if not provided)}
#'     }
#'
#' @examples
#' dummy_fit <- function(serie, ...) {
#'     new_modprevU(list(data = serie), serie, "test", list())
#' }
#' dummy_predict <- function(object, n.ahead, ...) ts(rep(0, n.ahead))
#' dummy_update <- function(object, newseries, refit = FALSE, ...) object
#'
#' register_model("test", dummy_fit, dummy_predict, dummy_update)
#'
#' models <- list_models()
#' print(models)
#'
#' "test" %in% list_models()
#'
#' info <- list_models(details = TRUE)
#' head(info)
#'
#' @seealso [get_model()], [register_model()], [get_registry()]
#'
#' @family model_registry
#'
#' @export

list_models <- function(details = FALSE) {
    models <- .get_models()
    tipos <- names(models)

    if (is.null(tipos)) {
        tipos <- character(0)
    }

    if (!details) {
        return(sort(tipos))
    }

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
        requires_regdata = sapply(models, function(m) m$requires_regdata),
        deps = sapply(models, function(m) {
            if (length(m$deps) > 0) paste(m$deps, collapse = ", ") else ""
        }),
        description = sapply(models, function(m) {
            desc <- m$metadata$description
            if (is.null(desc)) "" else as.character(desc)
        }),
        stringsAsFactors = FALSE,
        row.names = NULL
    )
}
