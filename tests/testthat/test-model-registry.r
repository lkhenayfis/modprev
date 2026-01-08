test_that(".init_registry", {
    f <- modprev:::.init_registry
    expect_true(is.function(f))

    test_that(".init_registry creates empty models list", {
        if (exists("models", envir = modprev:::.model_registry)) {
            rm("models", envir = modprev:::.model_registry)
        }
        if (exists("initialized", envir = modprev:::.model_registry)) {
            rm("initialized", envir = modprev:::.model_registry)
        }

        f()

        expect_true(exists("models", envir = modprev:::.model_registry))
        expect_true(exists("initialized", envir = modprev:::.model_registry))

        models <- get("models", envir = modprev:::.model_registry)
        expect_type(models, "list")
        expect_length(models, 0)
    })

    test_that(".init_registry is idempotent", {
        f()
        f()
        f()

        models <- get("models", envir = modprev:::.model_registry)
        expect_type(models, "list")
    })
})

test_that(".is_registry_initialized", {
    f <- modprev:::.is_registry_initialized
    expect_true(is.function(f))

    test_that(".is_registry_initialized returns FALSE when not initialized", {
        if (exists("initialized", envir = modprev:::.model_registry)) {
            rm("initialized", envir = modprev:::.model_registry)
        }

        result <- f()

        expect_false(result)
    })

    test_that(".is_registry_initialized returns TRUE after initialization", {
        modprev:::.init_registry()

        result <- f()

        expect_true(result)
    })
})

test_that(".get_models", {
    f <- modprev:::.get_models
    expect_true(is.function(f))

    test_that(".get_models initializes registry if needed", {
        if (exists("models", envir = modprev:::.model_registry)) {
            rm("models", envir = modprev:::.model_registry)
        }
        if (exists("initialized", envir = modprev:::.model_registry)) {
            rm("initialized", envir = modprev:::.model_registry)
        }

        models <- f()

        expect_type(models, "list")
        expect_length(models, 0)
        expect_true(modprev:::.is_registry_initialized())
    })

    test_that(".get_models returns existing models", {
        modprev:::.init_registry()

        dummy_fn <- function(...) NULL
        spec <- model_spec("test", dummy_fn, dummy_fn, dummy_fn)
        assign("models", list(test = spec), envir = modprev:::.model_registry)

        models <- f()

        expect_length(models, 1)
        expect_true("test" %in% names(models))
    })
})

test_that(".set_models", {
    f <- modprev:::.set_models
    expect_true(is.function(f))

    test_that(".set_models stores models list", {
        modprev:::.init_registry()

        dummy_fn <- function(...) NULL
        spec <- model_spec("test", dummy_fn, dummy_fn, dummy_fn)
        new_models <- list(test = spec)

        result <- withVisible(f(new_models))

        expect_false(result$visible)
        expect_null(result$value)

        stored <- get("models", envir = modprev:::.model_registry)
        expect_length(stored, 1)
        expect_identical(stored$test, spec)
    })

    test_that(".set_models initializes if needed", {
        if (exists("initialized", envir = modprev:::.model_registry)) {
            rm("initialized", envir = modprev:::.model_registry)
        }

        f(list())

        expect_true(modprev:::.is_registry_initialized())
    })

    test_that(".set_models overwrites existing models", {
        modprev:::.init_registry()

        dummy_fn <- function(...) NULL
        spec1 <- model_spec("test1", dummy_fn, dummy_fn, dummy_fn)
        spec2 <- model_spec("test2", dummy_fn, dummy_fn, dummy_fn)

        f(list(test1 = spec1))
        f(list(test2 = spec2))

        stored <- get("models", envir = modprev:::.model_registry)
        expect_length(stored, 1)
        expect_true("test2" %in% names(stored))
        expect_false("test1" %in% names(stored))
    })
})

test_that(".add_model", {
    f <- modprev:::.add_model
    expect_true(is.function(f))

    test_that(".add_model adds new model", {
        modprev:::.init_registry()
        modprev:::.set_models(list())

        dummy_fn <- function(...) NULL
        spec <- model_spec("test", dummy_fn, dummy_fn, dummy_fn)

        result <- withVisible(f("test", spec))

        expect_false(result$visible)
        expect_null(result$value)

        models <- modprev:::.get_models()
        expect_length(models, 1)
        expect_identical(models$test, spec)
    })

    test_that(".add_model overwrites existing model with same tipo", {
        modprev:::.init_registry()
        modprev:::.set_models(list())

        dummy_fn <- function(...) NULL
        spec1 <- model_spec("test", dummy_fn, dummy_fn, dummy_fn)
        spec2 <- model_spec("test", dummy_fn, dummy_fn, dummy_fn, deps = "forecast")

        f("test", spec1)
        f("test", spec2)

        models <- modprev:::.get_models()
        expect_length(models, 1)
        expect_equal(models$test$deps, "forecast")
    })

    test_that(".add_model preserves other models", {
        modprev:::.init_registry()
        modprev:::.set_models(list())

        dummy_fn <- function(...) NULL
        spec1 <- model_spec("test1", dummy_fn, dummy_fn, dummy_fn)
        spec2 <- model_spec("test2", dummy_fn, dummy_fn, dummy_fn)

        f("test1", spec1)
        f("test2", spec2)

        models <- modprev:::.get_models()
        expect_length(models, 2)
        expect_true(all(c("test1", "test2") %in% names(models)))
    })
})

test_that(".remove_model", {
    f <- modprev:::.remove_model
    expect_true(is.function(f))

    test_that(".remove_model removes existing model", {
        modprev:::.init_registry()

        dummy_fn <- function(...) NULL
        spec <- model_spec("test", dummy_fn, dummy_fn, dummy_fn)
        modprev:::.set_models(list(test = spec))

        result <- withVisible(f("test"))

        expect_false(result$visible)
        expect_null(result$value)

        models <- modprev:::.get_models()
        expect_length(models, 0)
    })

    test_that(".remove_model is no-op for non-existent model", {
        modprev:::.init_registry()
        modprev:::.set_models(list())

        expect_no_error(f("nonexistent"))

        models <- modprev:::.get_models()
        expect_length(models, 0)
    })

    test_that(".remove_model preserves other models", {
        modprev:::.init_registry()

        dummy_fn <- function(...) NULL
        spec1 <- model_spec("test1", dummy_fn, dummy_fn, dummy_fn)
        spec2 <- model_spec("test2", dummy_fn, dummy_fn, dummy_fn)
        modprev:::.set_models(list(test1 = spec1, test2 = spec2))

        f("test1")

        models <- modprev:::.get_models()
        expect_length(models, 1)
        expect_true("test2" %in% names(models))
        expect_false("test1" %in% names(models))
    })
})

test_that(".has_model", {
    f <- modprev:::.has_model
    expect_true(is.function(f))

    test_that(".has_model returns TRUE for existing model", {
        modprev:::.init_registry()

        dummy_fn <- function(...) NULL
        spec <- model_spec("test", dummy_fn, dummy_fn, dummy_fn)
        modprev:::.set_models(list(test = spec))

        result <- f("test")

        expect_true(result)
    })

    test_that(".has_model returns FALSE for non-existent model", {
        modprev:::.init_registry()
        modprev:::.set_models(list())

        result <- f("nonexistent")

        expect_false(result)
    })
})

test_that(".get_model_spec", {
    f <- modprev:::.get_model_spec
    expect_true(is.function(f))

    test_that(".get_model_spec retrieves existing model", {
        modprev:::.init_registry()

        dummy_fn <- function(...) NULL
        spec <- model_spec("test", dummy_fn, dummy_fn, dummy_fn)
        modprev:::.set_models(list(test = spec))

        result <- f("test")

        expect_identical(result, spec)
    })

    test_that(".get_model_spec returns NULL for non-existent model", {
        modprev:::.init_registry()
        modprev:::.set_models(list())

        result <- f("nonexistent")

        expect_null(result)
    })
})

test_that(".clear_registry", {
    f <- modprev:::.clear_registry
    expect_true(is.function(f))

    test_that(".clear_registry removes all models", {
        modprev:::.init_registry()

        dummy_fn <- function(...) NULL
        spec1 <- model_spec("test1", dummy_fn, dummy_fn, dummy_fn)
        spec2 <- model_spec("test2", dummy_fn, dummy_fn, dummy_fn)
        modprev:::.set_models(list(test1 = spec1, test2 = spec2))

        result <- withVisible(f())

        expect_false(result$visible)
        expect_null(result$value)

        models <- modprev:::.get_models()
        expect_length(models, 0)
    })

    test_that(".clear_registry is safe when not initialized", {
        if (exists("initialized", envir = modprev:::.model_registry)) {
            rm("initialized", envir = modprev:::.model_registry)
        }

        expect_no_error(f())
    })
})

test_that("get_registry", {
    f <- get_registry
    expect_true(is.function(f))

    test_that("get_registry returns model_registry object", {
        modprev:::.init_registry()
        modprev:::.set_models(list())

        result <- f()

        expect_s3_class(result, "model_registry")
        expect_type(result$models, "list")
    })

    test_that("get_registry initializes on first call", {
        if (exists("models", envir = modprev:::.model_registry)) {
            rm("models", envir = modprev:::.model_registry)
        }
        if (exists("initialized", envir = modprev:::.model_registry)) {
            rm("initialized", envir = modprev:::.model_registry)
        }

        result <- f()

        expect_s3_class(result, "model_registry")
        expect_length(result$models, 0)
    })

    test_that("get_registry returns snapshot of models", {
        modprev:::.init_registry()

        dummy_fn <- function(...) NULL
        spec <- model_spec("test", dummy_fn, dummy_fn, dummy_fn)
        modprev:::.set_models(list(test = spec))

        result1 <- f()
        result2 <- f()

        expect_equal(result1$models, result2$models)

        modprev:::.clear_registry()

        expect_length(result1$models, 1)
    })

    test_that("get_registry includes all registered models", {
        modprev:::.init_registry()
        modprev:::.set_models(list())

        dummy_fn <- function(...) NULL
        spec1 <- model_spec("test1", dummy_fn, dummy_fn, dummy_fn)
        spec2 <- model_spec("test2", dummy_fn, dummy_fn, dummy_fn)

        modprev:::.add_model("test1", spec1)
        modprev:::.add_model("test2", spec2)

        result <- f()

        expect_length(result$models, 2)
        expect_true(all(c("test1", "test2") %in% names(result$models)))
    })
})

test_that("print.model_registry", {
    f <- print.model_registry
    expect_true(is.function(f))

    test_that("print.model_registry displays empty registry", {
        modprev:::.init_registry()
        modprev:::.set_models(list())

        registry <- get_registry()
        output <- capture.output(f(registry))

        expect_true(any(grepl("Model Registry", output)))
        expect_true(any(grepl("Registered models: 0", output)))
    })

    test_that("print.model_registry displays registered models", {
        modprev:::.init_registry()
        modprev:::.set_models(list())

        dummy_fn <- function(...) NULL
        spec1 <- model_spec("test1", dummy_fn, dummy_fn, dummy_fn)
        spec2 <- model_spec("test2", dummy_fn, dummy_fn, dummy_fn,
                           requires_regdata = TRUE)

        modprev:::.add_model("test1", spec1)
        modprev:::.add_model("test2", spec2)

        registry <- get_registry()
        output <- capture.output(f(registry))

        expect_true(any(grepl("Registered models: 2", output)))
        expect_true(any(grepl("test1", output)))
        expect_true(any(grepl("test2", output)))
    })

    test_that("print.model_registry marks models requiring regdata", {
        modprev:::.init_registry()
        modprev:::.set_models(list())

        dummy_fn <- function(...) NULL
        spec <- model_spec("test", dummy_fn, dummy_fn, dummy_fn,
                          requires_regdata = TRUE)

        modprev:::.add_model("test", spec)

        registry <- get_registry()
        output <- capture.output(f(registry))

        expect_true(any(grepl("\\* test", output)))
        expect_true(any(grepl("\\* Requires regdata", output)))
    })

    test_that("print.model_registry returns invisibly", {
        modprev:::.init_registry()
        modprev:::.set_models(list())

        registry <- get_registry()
        result <- withVisible(f(registry))

        expect_false(result$visible)
        expect_identical(result$value, registry)
    })
})

test_that("is_registered", {
    f <- is_registered
    expect_true(is.function(f))

    test_that("is_registered returns FALSE for non-existent model", {
        modprev:::.init_registry()
        modprev:::.set_models(list())

        result <- f("nonexistent_model")

        expect_false(result)
    })

    test_that("is_registered returns TRUE for existing model", {
        modprev:::.init_registry()
        modprev:::.set_models(list())

        dummy_fn <- function(...) NULL
        spec <- model_spec("test", dummy_fn, dummy_fn, dummy_fn)
        modprev:::.add_model("test", spec)

        result <- f("test")

        expect_true(result)
    })

    test_that("is_registered validates tipo argument", {
        expect_error(f(123), "'tipo' must be a single character string")
        expect_error(f(c("a", "b")), "'tipo' must be a single character string")
        expect_error(f(character(0)), "'tipo' must be a single character string")
    })
})

test_that(".validate_model_functions", {
    f <- modprev:::.validate_model_functions
    expect_true(is.function(f))

    test_that(".validate_model_functions accepts valid specification", {
        fit_fn <- function(serie, ...) NULL
        pred_fn <- function(object, n.ahead, ...) NULL
        upd_fn <- function(object, newseries, refit = FALSE, ...) NULL

        spec <- model_spec("test", fit_fn, pred_fn, upd_fn)

        expect_no_error(f(spec))
    })

    test_that(".validate_model_functions rejects fit_fn without serie", {
        fit_fn <- function(...) NULL
        pred_fn <- function(object, n.ahead, ...) NULL
        upd_fn <- function(object, newseries, refit = FALSE, ...) NULL

        spec <- new_model_spec("test", fit_fn, pred_fn, upd_fn, FALSE, character(), list())

        expect_error(f(spec), "fit_fn for model 'test' must have 'serie' argument")
    })

    test_that(".validate_model_functions warns if fit_fn lacks ...", {
        fit_fn <- function(serie) NULL
        pred_fn <- function(object, n.ahead, ...) NULL
        upd_fn <- function(object, newseries, refit = FALSE, ...) NULL

        spec <- new_model_spec("test", fit_fn, pred_fn, upd_fn, FALSE, character(), list())

        expect_warning(
            f(spec),
            "fit_fn for model 'test' should have '...' argument for flexibility"
        )
    })

    test_that(".validate_model_functions rejects predict_fn without object", {
        fit_fn <- function(serie, ...) NULL
        pred_fn <- function(n.ahead, ...) NULL
        upd_fn <- function(object, newseries, refit = FALSE, ...) NULL

        spec <- new_model_spec("test", fit_fn, pred_fn, upd_fn, FALSE, character(), list())

        expect_error(f(spec), "predict_fn for model 'test' must have 'object' argument")
    })

    test_that(".validate_model_functions rejects predict_fn without n.ahead", {
        fit_fn <- function(serie, ...) NULL
        pred_fn <- function(object, ...) NULL
        upd_fn <- function(object, newseries, refit = FALSE, ...) NULL

        spec <- new_model_spec("test", fit_fn, pred_fn, upd_fn, FALSE, character(), list())

        expect_error(f(spec), "predict_fn for model 'test' must have 'n.ahead' argument")
    })

    test_that(".validate_model_functions rejects update_fn without object", {
        fit_fn <- function(serie, ...) NULL
        pred_fn <- function(object, n.ahead, ...) NULL
        upd_fn <- function(newseries, refit = FALSE, ...) NULL

        spec <- new_model_spec("test", fit_fn, pred_fn, upd_fn, FALSE, character(), list())

        expect_error(f(spec), "update_fn for model 'test' must have 'object' argument")
    })

    test_that(".validate_model_functions rejects update_fn without newseries", {
        fit_fn <- function(serie, ...) NULL
        pred_fn <- function(object, n.ahead, ...) NULL
        upd_fn <- function(object, refit = FALSE, ...) NULL

        spec <- new_model_spec("test", fit_fn, pred_fn, upd_fn, FALSE, character(), list())

        expect_error(f(spec), "update_fn for model 'test' must have 'newseries' argument")
    })

    test_that(".validate_model_functions rejects update_fn without refit", {
        fit_fn <- function(serie, ...) NULL
        pred_fn <- function(object, n.ahead, ...) NULL
        upd_fn <- function(object, newseries, ...) NULL

        spec <- new_model_spec("test", fit_fn, pred_fn, upd_fn, FALSE, character(), list())

        expect_error(f(spec), "update_fn for model 'test' must have 'refit' argument")
    })
})

test_that("register_model", {
    f <- register_model
    expect_true(is.function(f))

    test_that("register_model adds model to registry", {
        modprev:::.init_registry()
        modprev:::.set_models(list())

        fit_fn <- function(serie, ...) NULL
        pred_fn <- function(object, n.ahead, ...) NULL
        upd_fn <- function(object, newseries, refit = FALSE, ...) NULL

        result <- withVisible(f("test_reg", fit_fn, pred_fn, upd_fn))

        expect_false(result$visible)
        expect_s3_class(result$value, "model_spec")

        expect_true(is_registered("test_reg"))

        modprev:::.remove_model("test_reg")
    })

    test_that("register_model prevents duplicate registration by default", {
        modprev:::.init_registry()
        modprev:::.set_models(list())

        fit_fn <- function(serie, ...) NULL
        pred_fn <- function(object, n.ahead, ...) NULL
        upd_fn <- function(object, newseries, refit = FALSE, ...) NULL

        f("test_dup", fit_fn, pred_fn, upd_fn)

        expect_error(
            f("test_dup", fit_fn, pred_fn, upd_fn),
            "Model 'test_dup' is already registered. Use overwrite = TRUE to replace."
        )

        modprev:::.remove_model("test_dup")
    })

    test_that("register_model allows overwrite when specified", {
        modprev:::.init_registry()
        modprev:::.set_models(list())

        fit_fn <- function(serie, ...) NULL
        pred_fn <- function(object, n.ahead, ...) NULL
        upd_fn <- function(object, newseries, refit = FALSE, ...) NULL

        f("test_over", fit_fn, pred_fn, upd_fn)
        f("test_over", fit_fn, pred_fn, upd_fn, deps = "forecast", overwrite = TRUE)

        spec <- modprev:::.get_model_spec("test_over")
        expect_equal(spec$deps, "forecast")

        modprev:::.remove_model("test_over")
    })

    test_that("register_model validates functions by default", {
        modprev:::.init_registry()
        modprev:::.set_models(list())

        fit_fn <- function(...) NULL
        pred_fn <- function(object, n.ahead, ...) NULL
        upd_fn <- function(object, newseries, refit = FALSE, ...) NULL

        expect_error(
            f("test_val", fit_fn, pred_fn, upd_fn),
            "fit_fn for model 'test_val' must have 'serie' argument"
        )
    })

    test_that("register_model skips validation when validate = FALSE", {
        modprev:::.init_registry()
        modprev:::.set_models(list())

        fit_fn <- function(...) NULL
        pred_fn <- function(object, n.ahead, ...) NULL
        upd_fn <- function(object, newseries, refit = FALSE, ...) NULL

        expect_no_error(
            f("test_noval", fit_fn, pred_fn, upd_fn, validate = FALSE)
        )

        modprev:::.remove_model("test_noval")
    })

    test_that("register_model accepts all model_spec parameters", {
        modprev:::.init_registry()
        modprev:::.set_models(list())

        fit_fn <- function(serie, ...) NULL
        pred_fn <- function(object, n.ahead, ...) NULL
        upd_fn <- function(object, newseries, refit = FALSE, ...) NULL

        spec <- f(
            tipo = "test_full",
            fit_fn = fit_fn,
            predict_fn = pred_fn,
            update_fn = upd_fn,
            requires_regdata = TRUE,
            deps = c("forecast", "KFAS"),
            metadata = list(description = "Test model", author = "Test")
        )

        expect_true(spec$requires_regdata)
        expect_equal(spec$deps, c("forecast", "KFAS"))
        expect_equal(spec$metadata$description, "Test model")

        modprev:::.remove_model("test_full")
    })

    test_that("register_model returns model_spec invisibly", {
        modprev:::.init_registry()
        modprev:::.set_models(list())

        fit_fn <- function(serie, ...) NULL
        pred_fn <- function(object, n.ahead, ...) NULL
        upd_fn <- function(object, newseries, refit = FALSE, ...) NULL

        result <- withVisible(f("test_ret", fit_fn, pred_fn, upd_fn))

        expect_false(result$visible)
        expect_s3_class(result$value, "model_spec")
        expect_equal(result$value$tipo, "test_ret")

        modprev:::.remove_model("test_ret")
    })
})

test_that("get_model", {
    f <- get_model
    expect_true(is.function(f))

    test_that("get_model retrieves registered model", {
        modprev:::.init_registry()
        modprev:::.set_models(list())

        fit_fn <- function(serie, ...) NULL
        pred_fn <- function(object, n.ahead, ...) NULL
        upd_fn <- function(object, newseries, refit = FALSE, ...) NULL

        register_model("test_get", fit_fn, pred_fn, upd_fn)

        result <- f("test_get")

        expect_s3_class(result, "model_spec")
        expect_equal(result$tipo, "test_get")

        modprev:::.remove_model("test_get")
    })

    test_that("get_model errors for non-existent model by default", {
        modprev:::.init_registry()
        modprev:::.set_models(list())

        expect_error(
            f("nonexistent"),
            "Model 'nonexistent' is not registered"
        )
    })

    test_that("get_model error message lists available models", {
        modprev:::.init_registry()
        modprev:::.set_models(list())

        fit_fn <- function(serie, ...) NULL
        pred_fn <- function(object, n.ahead, ...) NULL
        upd_fn <- function(object, newseries, refit = FALSE, ...) NULL

        register_model("test1", fit_fn, pred_fn, upd_fn)
        register_model("test2", fit_fn, pred_fn, upd_fn)

        tryCatch(
            f("missing"),
            error = function(e) {
                expect_true(grepl("test1", e$message))
                expect_true(grepl("test2", e$message))
            }
        )

        modprev:::.remove_model("test1")
        modprev:::.remove_model("test2")
    })

    test_that("get_model returns NULL when error = FALSE", {
        modprev:::.init_registry()
        modprev:::.set_models(list())

        result <- f("nonexistent", error = FALSE)

        expect_null(result)
    })

    test_that("get_model validates tipo argument", {
        expect_error(f(123), "'tipo' must be a single character string")
        expect_error(f(c("a", "b")), "'tipo' must be a single character string")
        expect_error(f(character(0)), "'tipo' must be a single character string")
    })

    test_that("get_model returns correct spec for registered model", {
        modprev:::.init_registry()
        modprev:::.set_models(list())

        fit_fn <- function(serie, ...) NULL
        pred_fn <- function(object, n.ahead, ...) NULL
        upd_fn <- function(object, newseries, refit = FALSE, ...) NULL

        original_spec <- register_model(
            "test_spec",
            fit_fn,
            pred_fn,
            upd_fn,
            deps = "forecast"
        )

        retrieved_spec <- f("test_spec")

        expect_identical(retrieved_spec, original_spec)

        modprev:::.remove_model("test_spec")
    })
})

test_that("list_models", {
    f <- list_models
    expect_true(is.function(f))

    test_that("list_models returns empty vector for empty registry", {
        modprev:::.init_registry()
        modprev:::.set_models(list())

        result <- f()

        expect_type(result, "character")
        expect_length(result, 0)
    })

    test_that("list_models returns model names", {
        modprev:::.init_registry()
        modprev:::.set_models(list())

        fit_fn <- function(serie, ...) NULL
        pred_fn <- function(object, n.ahead, ...) NULL
        upd_fn <- function(object, newseries, refit = FALSE, ...) NULL

        register_model("test1", fit_fn, pred_fn, upd_fn)
        register_model("test2", fit_fn, pred_fn, upd_fn)

        result <- f()

        expect_type(result, "character")
        expect_length(result, 2)
        expect_true(all(c("test1", "test2") %in% result))

        modprev:::.remove_model("test1")
        modprev:::.remove_model("test2")
    })

    test_that("list_models returns sorted names", {
        modprev:::.init_registry()
        modprev:::.set_models(list())

        fit_fn <- function(serie, ...) NULL
        pred_fn <- function(object, n.ahead, ...) NULL
        upd_fn <- function(object, newseries, refit = FALSE, ...) NULL

        register_model("zebra", fit_fn, pred_fn, upd_fn)
        register_model("apple", fit_fn, pred_fn, upd_fn)
        register_model("mouse", fit_fn, pred_fn, upd_fn)

        result <- f()

        expect_equal(result, c("apple", "mouse", "zebra"))

        modprev:::.remove_model("zebra")
        modprev:::.remove_model("apple")
        modprev:::.remove_model("mouse")
    })

    test_that("list_models with details returns data.frame", {
        modprev:::.init_registry()
        modprev:::.set_models(list())

        fit_fn <- function(serie, ...) NULL
        pred_fn <- function(object, n.ahead, ...) NULL
        upd_fn <- function(object, newseries, refit = FALSE, ...) NULL

        register_model("test", fit_fn, pred_fn, upd_fn)

        result <- f(details = TRUE)

        expect_s3_class(result, "data.frame")
        expect_true(all(c("tipo", "requires_regdata", "deps", "description") %in% names(result)))

        modprev:::.remove_model("test")
    })

    test_that("list_models details shows correct information", {
        modprev:::.init_registry()
        modprev:::.set_models(list())

        fit_fn <- function(serie, ...) NULL
        pred_fn <- function(object, n.ahead, ...) NULL
        upd_fn <- function(object, newseries, refit = FALSE, ...) NULL

        register_model(
            "test_detail",
            fit_fn,
            pred_fn,
            upd_fn,
            requires_regdata = TRUE,
            deps = c("forecast", "KFAS"),
            metadata = list(description = "Test model")
        )

        result <- f(details = TRUE)

        expect_equal(nrow(result), 1)
        expect_equal(result$tipo, "test_detail")
        expect_true(result$requires_regdata)
        expect_equal(result$deps, "forecast, KFAS")
        expect_equal(result$description, "Test model")

        modprev:::.remove_model("test_detail")
    })

    test_that("list_models details handles missing description", {
        modprev:::.init_registry()
        modprev:::.set_models(list())

        fit_fn <- function(serie, ...) NULL
        pred_fn <- function(object, n.ahead, ...) NULL
        upd_fn <- function(object, newseries, refit = FALSE, ...) NULL

        register_model("test_nodesc", fit_fn, pred_fn, upd_fn)

        result <- f(details = TRUE)

        expect_equal(result$description, "")

        modprev:::.remove_model("test_nodesc")
    })

    test_that("list_models details handles empty deps", {
        modprev:::.init_registry()
        modprev:::.set_models(list())

        fit_fn <- function(serie, ...) NULL
        pred_fn <- function(object, n.ahead, ...) NULL
        upd_fn <- function(object, newseries, refit = FALSE, ...) NULL

        register_model("test_nodeps", fit_fn, pred_fn, upd_fn)

        result <- f(details = TRUE)

        expect_equal(result$deps, "")

        modprev:::.remove_model("test_nodeps")
    })

    test_that("list_models details returns empty data.frame for empty registry", {
        modprev:::.init_registry()
        modprev:::.set_models(list())

        result <- f(details = TRUE)

        expect_s3_class(result, "data.frame")
        expect_equal(nrow(result), 0)
        expect_true(all(c("tipo", "requires_regdata", "deps", "description") %in% names(result)))
    })
})
