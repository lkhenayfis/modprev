####################################################################################################
# META-TESTS FOR INTEGRATION HELPERS
####################################################################################################

test_that("with_registered_models", {
    f <- with_registered_models
    expect_true(is.function(f))

    test_that("executes function for each model", {
        tested <- character(0)

        f(function(tipo) {
            tested <<- c(tested, tipo)
        })

        expect_true(length(tested) > 0)
        expect_true("sarima" %in% tested)
    })

    test_that("filters univariate only models", {
        tested <- character(0)

        f(
            function(tipo) tested <<- c(tested, tipo),
            univariate_only = TRUE
        )

        expect_true("sarima" %in% tested)
        expect_false("reg_lin" %in% tested)
    })

    test_that("filters regression only models", {
        tested <- character(0)

        f(
            function(tipo) tested <<- c(tested, tipo),
            regression_only = TRUE
        )

        expect_true("reg_lin" %in% tested)
        expect_false("sarima" %in% tested)
    })

    test_that("respects exclude parameter", {
        tested <- character(0)

        f(
            function(tipo) tested <<- c(tested, tipo),
            exclude = c("sarima", "ss_ar1_saz")
        )

        expect_false("sarima" %in% tested)
        expect_false("ss_ar1_saz" %in% tested)
    })

    test_that("returns results invisibly", {
        result <- f(function(tipo) tipo)

        expect_type(result, "list")
        expect_true(length(result) > 0)
    })
})

test_that("test_model_workflow", {
    f <- test_model_workflow
    expect_true(is.function(f))

    test_that("works with univariate model", {
        result <- f("sarima", test_update = TRUE)

        expect_type(result, "list")
        expect_named(result, c("fit", "pred", "updated"))
        expect_s3_class(result$fit, "sarima")
        expect_s3_class(result$pred, "ts")
        expect_s3_class(result$updated, "sarima")
    })

    test_that("works with regression model", {
        result <- f("reg_lin", test_update = TRUE)

        expect_type(result, "list")
        expect_named(result, c("fit", "pred", "updated"))
        expect_s3_class(result$fit, "reg_lin")
        expect_s3_class(result$pred, "ts")
        expect_s3_class(result$updated, "reg_lin")
    })

    test_that("accepts custom serie", {
        custom_serie <- ts(rnorm(100), frequency = 12)

        result <- f("sarima", serie = custom_serie)

        expect_s3_class(result$fit, "sarima")
    })

    test_that("accepts custom regdata", {
        data <- make_regression_data(n = 100, n_predictors = 3, seed = 123)

        result <- f("reg_lin", serie = data$serie, regdata = data$regdata)
        expect_s3_class(result$fit, "reg_lin")
    })

    test_that("validates model structure", {
        expect_error(f("sarima"), NA)
    })

    test_that("validates prediction format", {
        result <- f("sarima", n.ahead = 24)

        expect_equal(nrow(result$pred), 24)
    })
})

test_that("expect_janelamovel_compatible", {
    f <- expect_janelamovel_compatible
    expect_true(is.function(f))

    test_that("works with univariate models", {
        expect_error(f("sarima"), NA)
    })

    test_that("returns list of predictions", {
        result <- f("sarima")

        expect_type(result, "list")
        expect_true(length(result) > 0)
    })

    test_that("validates janelamovel structure", {
        jan <- f("sarima", config = jm_config(janela = 24))

        expect_true(length(jan) > 0)

        for (window_result in jan) {
            expect_s3_class(window_result, "ts")
        }
    })

    test_that("accepts custom serie", {
        custom_serie <- make_univariate_series(n = 150, seed = 456)

        expect_error(f("sarima", serie = custom_serie), NA)
    })

    test_that("skips regression models", {
        expect_error(
            f("reg_lin"),
            regexp = "requires regdata"
        )
    })
})

test_that("expect_periodic_compatible", {
    f <- expect_periodic_compatible
    expect_true(is.function(f))

    test_that("works with univariate models", {
        expect_error(f("sarima"), NA)
    })

    test_that("returns modprevP object", {
        result <- f("sarima")

        expect_s3_class(result, "modprevP")
        expect_s3_class(result, "modprev")
    })

    test_that("validates modprevP structure", {
        mod <- f("sarima")

        expect_s3_class(mod, "modprevP")
        expect_true("modprevP" %in% class(mod))
    })

    test_that("validates prediction works", {
        mod <- f("sarima")
        pred <- predict(mod, n.ahead = 12)

        expect_s3_class(pred, "ts")
        expect_equal(nrow(pred), 12)
    })

    test_that("accepts custom serie", {
        custom_serie <- make_univariate_series(n = 120, frequency = 12, seed = 789)

        expect_error(f("sarima", serie = custom_serie), NA)
    })
})
