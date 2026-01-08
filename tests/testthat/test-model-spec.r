test_that("model_spec", {
    f <- model_spec
    expect_true(is.function(f))

    test_that("model_spec creates valid specification", {
        dummy_fit <- function(serie, ...) NULL
        dummy_predict <- function(object, n.ahead, ...) NULL
        dummy_update <- function(object, newseries, refit, ...) NULL

        spec <- f(
            tipo = "test_model",
            fit_fn = dummy_fit,
            predict_fn = dummy_predict,
            update_fn = dummy_update,
            requires_regdata = FALSE,
            deps = character()
        )

        expect_s3_class(spec, "model_spec")
        expect_equal(spec$tipo, "test_model")
        expect_identical(spec$fit_fn, dummy_fit)
        expect_identical(spec$predict_fn, dummy_predict)
        expect_identical(spec$update_fn, dummy_update)
        expect_false(spec$requires_regdata)
        expect_length(spec$deps, 0)
        expect_type(spec$metadata, "list")
        expect_length(spec$metadata, 0)
    })

    test_that("model_spec accepts requires_regdata TRUE", {
        dummy_fn <- function(...) NULL

        spec <- f(
            tipo = "reg_model",
            fit_fn = dummy_fn,
            predict_fn = dummy_fn,
            update_fn = dummy_fn,
            requires_regdata = TRUE
        )

        expect_true(spec$requires_regdata)
    })

    test_that("model_spec accepts dependencies", {
        dummy_fn <- function(...) NULL

        spec <- f(
            tipo = "test",
            fit_fn = dummy_fn,
            predict_fn = dummy_fn,
            update_fn = dummy_fn,
            deps = c("forecast", "KFAS")
        )

        expect_length(spec$deps, 2)
        expect_equal(spec$deps, c("forecast", "KFAS"))
    })

    test_that("model_spec accepts metadata", {
        dummy_fn <- function(...) NULL

        spec <- f(
            tipo = "test",
            fit_fn = dummy_fn,
            predict_fn = dummy_fn,
            update_fn = dummy_fn,
            metadata = list(description = "Test model", author = "Test Author")
        )

        expect_equal(spec$metadata$description, "Test model")
        expect_equal(spec$metadata$author, "Test Author")
    })

    test_that("model_spec validates tipo argument", {
        dummy_fn <- function(...) NULL

        expect_error(
            f("", dummy_fn, dummy_fn, dummy_fn),
            "nchar\\(spec\\$tipo\\) > 0"
        )

        expect_error(
            f(c("a", "b"), dummy_fn, dummy_fn, dummy_fn),
            "length\\(spec\\$tipo\\) == 1"
        )

        expect_error(
            f(123, dummy_fn, dummy_fn, dummy_fn),
            "is\\.character\\(spec\\$tipo\\)"
        )

        expect_error(
            f(NA_character_, dummy_fn, dummy_fn, dummy_fn),
            "nchar\\(spec\\$tipo\\) > 0"
        )
    })

    test_that("model_spec validates function arguments", {
        dummy_fn <- function(...) NULL

        expect_error(
            f("test", "not_a_function", dummy_fn, dummy_fn),
            "is\\.function\\(spec\\$fit_fn\\)"
        )

        expect_error(
            f("test", dummy_fn, "not_a_function", dummy_fn),
            "is\\.function\\(spec\\$predict_fn\\)"
        )

        expect_error(
            f("test", dummy_fn, dummy_fn, "not_a_function"),
            "is\\.function\\(spec\\$update_fn\\)"
        )

        expect_error(
            f("test", NULL, dummy_fn, dummy_fn),
            "is\\.function\\(spec\\$fit_fn\\)"
        )

        expect_error(
            f("test", dummy_fn, NULL, dummy_fn),
            "is\\.function\\(spec\\$predict_fn\\)"
        )

        expect_error(
            f("test", dummy_fn, dummy_fn, NULL),
            "is\\.function\\(spec\\$update_fn\\)"
        )
    })

    test_that("model_spec validates requires_regdata argument", {
        dummy_fn <- function(...) NULL

        expect_error(
            f("test", dummy_fn, dummy_fn, dummy_fn, requires_regdata = "yes"),
            "is\\.logical\\(spec\\$requires_regdata\\)"
        )

        expect_error(
            f("test", dummy_fn, dummy_fn, dummy_fn,
              requires_regdata = c(TRUE, FALSE)),
            "length\\(spec\\$requires_regdata\\) == 1"
        )



        expect_error(
            f("test", dummy_fn, dummy_fn, dummy_fn, requires_regdata = 1),
            "is\\.logical\\(spec\\$requires_regdata\\)"
        )
    })

    test_that("model_spec validates deps argument", {
        dummy_fn <- function(...) NULL

        expect_error(
            f("test", dummy_fn, dummy_fn, dummy_fn, deps = 123),
            "is\\.character\\(spec\\$deps\\)"
        )

        expect_error(
            f("test", dummy_fn, dummy_fn, dummy_fn, deps = list("forecast")),
            "is\\.character\\(spec\\$deps\\)"
        )

        spec <- f("test", dummy_fn, dummy_fn, dummy_fn, deps = character())
        expect_length(spec$deps, 0)

        spec <- f("test", dummy_fn, dummy_fn, dummy_fn,
                  deps = c("forecast", "KFAS"))
        expect_length(spec$deps, 2)
    })

    test_that("model_spec validates metadata argument", {
        dummy_fn <- function(...) NULL

        expect_error(
            f("test", dummy_fn, dummy_fn, dummy_fn, metadata = "not a list"),
            "is\\.list\\(spec\\$metadata\\)"
        )

        expect_error(
            f("test", dummy_fn, dummy_fn, dummy_fn, metadata = c(a = 1, b = 2)),
            "is\\.list\\(spec\\$metadata\\)"
        )

        spec <- f("test", dummy_fn, dummy_fn, dummy_fn, metadata = list())
        expect_length(spec$metadata, 0)
    })
})

test_that("new_model_spec", {
    f <- new_model_spec
    expect_true(is.function(f))

    test_that("new_model_spec creates structure without validation", {
        dummy_fn <- function(...) NULL

        spec <- f(
            tipo = "test",
            fit_fn = dummy_fn,
            predict_fn = dummy_fn,
            update_fn = dummy_fn,
            requires_regdata = FALSE,
            deps = character(),
            metadata = list()
        )

        expect_s3_class(spec, "model_spec")
        expect_equal(spec$tipo, "test")
        expect_identical(spec$fit_fn, dummy_fn)
    })

    test_that("new_model_spec accepts invalid inputs without error", {
        spec <- f(
            tipo = "",
            fit_fn = "not_a_function",
            predict_fn = NULL,
            update_fn = 123,
            requires_regdata = "invalid",
            deps = list(1, 2),
            metadata = "not_a_list"
        )

        expect_s3_class(spec, "model_spec")
    })
})

test_that("validate_model_spec", {
    f <- validate_model_spec
    expect_true(is.function(f))

    test_that("validate_model_spec accepts valid spec", {
        dummy_fn <- function(...) NULL

        spec <- new_model_spec(
            tipo = "test",
            fit_fn = dummy_fn,
            predict_fn = dummy_fn,
            update_fn = dummy_fn,
            requires_regdata = TRUE,
            deps = c("forecast"),
            metadata = list(author = "test")
        )

        result <- f(spec)
        expect_identical(result, spec)
    })

    test_that("validate_model_spec returns invisibly", {
        dummy_fn <- function(...) NULL

        spec <- new_model_spec(
            tipo = "test",
            fit_fn = dummy_fn,
            predict_fn = dummy_fn,
            update_fn = dummy_fn,
            requires_regdata = FALSE,
            deps = character(),
            metadata = list()
        )

        expect_invisible(f(spec))
    })

    test_that("validate_model_spec rejects invalid class", {
        obj <- list(tipo = "test")
        class(obj) <- "wrong_class"

        expect_error(f(obj), "inherits\\(spec, \"model_spec\"\\)")
    })

    test_that("validate_model_spec rejects invalid tipo", {
        dummy_fn <- function(...) NULL

        spec_empty <- new_model_spec("", dummy_fn, dummy_fn, dummy_fn,
                                     FALSE, character(), list())
        expect_error(f(spec_empty), "nchar\\(spec\\$tipo\\) > 0")

        spec_multiple <- new_model_spec(c("a", "b"), dummy_fn, dummy_fn, dummy_fn,
                                        FALSE, character(), list())
        expect_error(f(spec_multiple), "length\\(spec\\$tipo\\) == 1")

        spec_numeric <- new_model_spec(123, dummy_fn, dummy_fn, dummy_fn,
                                       FALSE, character(), list())
        expect_error(f(spec_numeric), "is\\.character\\(spec\\$tipo\\)")
    })

    test_that("validate_model_spec rejects invalid functions", {
        dummy_fn <- function(...) NULL

        spec_bad_fit <- new_model_spec("test", "not_fn", dummy_fn, dummy_fn,
                                       FALSE, character(), list())
        expect_error(f(spec_bad_fit), "is\\.function\\(spec\\$fit_fn\\)")

        spec_bad_predict <- new_model_spec("test", dummy_fn, NULL, dummy_fn,
                                           FALSE, character(), list())
        expect_error(f(spec_bad_predict), "is\\.function\\(spec\\$predict_fn\\)")

        spec_bad_update <- new_model_spec("test", dummy_fn, dummy_fn, 123,
                                          FALSE, character(), list())
        expect_error(f(spec_bad_update), "is\\.function\\(spec\\$update_fn\\)")
    })

    test_that("validate_model_spec rejects invalid requires_regdata", {
        dummy_fn <- function(...) NULL

        spec_str <- new_model_spec("test", dummy_fn, dummy_fn, dummy_fn,
                                   "yes", character(), list())
        expect_error(f(spec_str), "is\\.logical\\(spec\\$requires_regdata\\)")

        spec_multiple <- new_model_spec("test", dummy_fn, dummy_fn, dummy_fn,
                                        c(TRUE, FALSE), character(), list())
        expect_error(f(spec_multiple), "length\\(spec\\$requires_regdata\\) == 1")
    })

    test_that("validate_model_spec rejects invalid deps", {
        dummy_fn <- function(...) NULL

        spec_numeric <- new_model_spec("test", dummy_fn, dummy_fn, dummy_fn,
                                       FALSE, 123, list())
        expect_error(f(spec_numeric), "is\\.character\\(spec\\$deps\\)")

        spec_list <- new_model_spec("test", dummy_fn, dummy_fn, dummy_fn,
                                    FALSE, list("forecast"), list())
        expect_error(f(spec_list), "is\\.character\\(spec\\$deps\\)")
    })

    test_that("validate_model_spec rejects invalid metadata", {
        dummy_fn <- function(...) NULL

        spec_str <- new_model_spec("test", dummy_fn, dummy_fn, dummy_fn,
                                   FALSE, character(), "not_list")
        expect_error(f(spec_str), "is\\.list\\(spec\\$metadata\\)")
    })
})

test_that("print.model_spec", {
    f <- print.model_spec
    expect_true(is.function(f))

    test_that("print.model_spec displays basic information", {
        dummy_fn <- function(...) NULL

        spec <- model_spec(
            tipo = "test_model",
            fit_fn = dummy_fn,
            predict_fn = dummy_fn,
            update_fn = dummy_fn,
            requires_regdata = TRUE,
            deps = c("forecast", "KFAS")
        )

        output <- capture.output(f(spec))

        expect_true(any(grepl("Model Specification: test_model", output)))
        expect_true(any(grepl("Requires regdata: TRUE", output)))
        expect_true(any(grepl("forecast, KFAS", output)))
    })

    test_that("print.model_spec displays metadata", {
        dummy_fn <- function(...) NULL

        spec <- model_spec(
            tipo = "test",
            fit_fn = dummy_fn,
            predict_fn = dummy_fn,
            update_fn = dummy_fn,
            metadata = list(description = "Test model", version = "1.0")
        )

        output <- capture.output(f(spec))

        expect_true(any(grepl("Metadata:", output)))
        expect_true(any(grepl("description", output)))
        expect_true(any(grepl("Test model", output)))
        expect_true(any(grepl("version", output)))
        expect_true(any(grepl("1.0", output)))
    })

    test_that("print.model_spec handles no dependencies", {
        dummy_fn <- function(...) NULL

        spec <- model_spec(
            tipo = "minimal",
            fit_fn = dummy_fn,
            predict_fn = dummy_fn,
            update_fn = dummy_fn
        )

        output <- capture.output(f(spec))

        expect_true(any(grepl("Dependencies: none", output)))
    })

    test_that("print.model_spec handles no metadata", {
        dummy_fn <- function(...) NULL

        spec <- model_spec(
            tipo = "minimal",
            fit_fn = dummy_fn,
            predict_fn = dummy_fn,
            update_fn = dummy_fn
        )

        output <- capture.output(f(spec))

        expect_false(any(grepl("Metadata:", output)))
    })

    test_that("print.model_spec returns invisibly", {
        dummy_fn <- function(...) NULL

        spec <- model_spec(
            tipo = "test",
            fit_fn = dummy_fn,
            predict_fn = dummy_fn,
            update_fn = dummy_fn
        )

        result <- withVisible(f(spec))

        expect_false(result$visible)
        expect_identical(result$value, spec)
    })
})
