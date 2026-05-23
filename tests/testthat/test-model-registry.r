local({
    saved_registry <- modprev:::.model_registry$models
    on.exit(assign("models", saved_registry, envir = modprev:::.model_registry), add = TRUE)

    test_that(".init_registry", {
    f <- modprev:::.init_registry
    expect_true(is.function(f))

    test_that(".init_registry creates empty models list", {
        f()
        expect_true(exists("models", envir = modprev:::.model_registry))
        expect_length(modprev:::.model_registry$models, 0)
    })

    test_that(".init_registry is idempotent", {
        f(); f(); expect_type(modprev:::.model_registry$models, "list")
    })
})

test_that(".clear_registry", {
    f <- modprev:::.clear_registry
    expect_true(is.function(f))

    test_that(".clear_registry removes all models", {
        assign("models", list(x = model_spec("x", function(serie, ...) NULL)),
            envir = modprev:::.model_registry)
        f()
        expect_length(modprev:::.model_registry$models, 0)
    })

    test_that(".clear_registry is safe on uninitialised registry", {
        rm("models", envir = modprev:::.model_registry)
        expect_no_error(f())
    })
})

test_that("get_registry", {
    f <- get_registry
    expect_true(is.function(f))

    test_that("get_registry returns model_registry object", {
        modprev:::.clear_registry()
        result <- f()
        expect_s3_class(result, "model_registry")
        expect_type(result$models, "list")
    })

    test_that("get_registry reflects registered models", {
        modprev:::.clear_registry()
        dummy_fn <- function(serie, ...) NULL
        register_model("test1", dummy_fn)
        register_model("test2", dummy_fn)
        expect_length(f()$models, 2)
    })
})

test_that("print.model_registry", {
    f <- print.model_registry
    expect_true(is.function(f))

    test_that("print.model_registry displays empty registry", {
        modprev:::.clear_registry()
        out <- capture.output(f(get_registry()))
        expect_true(any(grepl("Model Registry", out)))
        expect_true(any(grepl("Registered models: 0", out)))
    })

    test_that("print.model_registry displays registered models", {
        modprev:::.clear_registry()
        register_model("test1", function(serie, ...) NULL)
        register_model("test2", function(serie, ...) NULL, requires_regdata = TRUE)
        out <- capture.output(f(get_registry()))
        expect_true(any(grepl("Registered models: 2", out)))
        expect_true(any(grepl("test2", out)))
    })
})

test_that("is_registered", {
    f <- is_registered
    expect_true(is.function(f))

    test_that("is_registered returns TRUE for existing model", {
        modprev:::.onLoad(NULL, NULL); expect_true(f("sarima"))
    })

    test_that("is_registered returns FALSE for missing model", {
        modprev:::.clear_registry(); expect_false(f("nonexistent"))
    })

    test_that("is_registered errors on bad tipo argument", {
        expect_error(f(123), "'tipo' must be a single character string")
        expect_error(f(c("a", "b")), "'tipo' must be a single character string")
    })
})

test_that(".validate_model_functions", {
    f <- modprev:::.validate_model_functions
    expect_true(is.function(f))

    test_that(".validate_model_functions accepts valid fit_fn", {
        expect_no_error(f(model_spec("t", function(serie, ...) NULL)))
    })

    test_that(".validate_model_functions rejects fit_fn without serie", {
        expect_error(
            f(new_model_spec("t", function(...) NULL, FALSE, character(), list())),
            "fit_fn for model 't' must have 'serie' argument"
        )
    })

    test_that(".validate_model_functions warns on missing ...", {
        expect_warning(
            f(new_model_spec("t", function(serie) NULL, FALSE, character(), list())),
            "fit_fn for model 't' should have '...' argument for flexibility"
        )
    })
})

test_that("register_model", {
    f <- register_model
    expect_true(is.function(f))

    test_that("register_model adds model to registry", {
        modprev:::.clear_registry()
        f("test_reg", function(serie, ...) NULL)
        expect_true(is_registered("test_reg"))
    })

    test_that("register_model prevents duplicate registration", {
        modprev:::.clear_registry()
        f("test_dup", function(serie, ...) NULL)
        expect_error(
            f("test_dup", function(serie, ...) NULL),
            "Model 'test_dup' is already registered. Use overwrite = TRUE to replace."
        )
    })

    test_that("register_model allows overwrite = TRUE", {
        modprev:::.clear_registry()
        f("test_over", function(serie, ...) NULL)
        f("test_over", function(serie, ...) NULL, deps = "forecast", overwrite = TRUE)
        expect_equal(modprev:::.lookup("test_over")$deps, "forecast")
    })

    test_that("register_model accepts requires_regdata, deps, and metadata", {
        modprev:::.clear_registry()
        spec <- f("test_full", function(serie, ...) NULL,
            requires_regdata = TRUE, deps = c("forecast", "KFAS"),
            metadata = list(description = "Test model"))
        expect_true(spec$requires_regdata)
        expect_equal(spec$deps, c("forecast", "KFAS"))
        expect_equal(spec$metadata$description, "Test model")
    })
})

test_that("get_model", {
    f <- get_model
    expect_true(is.function(f))

    test_that("get_model retrieves registered model", {
        modprev:::.onLoad(NULL, NULL)
        expect_s3_class(f("sarima"), "model_spec")
    })

    test_that("get_model errors for missing model", {
        modprev:::.clear_registry()
        expect_error(f("nonexistent"), "Model 'nonexistent' is not registered")
    })

    test_that("get_model returns NULL with error = FALSE", {
        modprev:::.clear_registry()
        expect_null(f("nonexistent", error = FALSE))
    })
})

test_that("list_models", {
    f <- list_models
    expect_true(is.function(f))

    test_that("list_models returns sorted names", {
        modprev:::.clear_registry()
        register_model("zebra", function(serie, ...) NULL)
        register_model("apple", function(serie, ...) NULL)
        expect_equal(f(), c("apple", "zebra"))
    })

    test_that("list_models returns empty for empty registry", {
        modprev:::.clear_registry()
        expect_length(f(), 0)
    })

    test_that("list_models details = TRUE returns data.frame with required columns", {
        result <- f(details = TRUE)
        expect_s3_class(result, "data.frame")
        expect_true(all(c("tipo", "requires_regdata", "deps", "description") %in% names(result)))
    })
})

})
