test_that("model-behaviors", {

    test_that("all univariate models produce valid modprevU structure", {
        with_registered_models(univariate_only = TRUE, function(tipo) {
            serie <- make_univariate_series(n = 100, frequency = 12, seed = 123)
            mod <- estimamodelo(serie, tipo)

            expect_modprev_structure(mod, tipo)
        })
    })

    test_that("all regression models produce valid modprevU structure", {
        with_registered_models(regression_only = TRUE, function(tipo) {
            data <- make_regression_data(n = 100, n_predictors = 2, seed = 456)
            mod <- estimamodelo(data$serie, tipo, regdata = data$regdata)

            expect_modprev_structure(mod, tipo)
        })
    })

    test_that("all models produce valid prediction format", {
        with_registered_models(function(tipo) {
            result <- test_model_workflow(tipo, n.ahead = 12)

            testthat::expect_s3_class(result$pred, "ts")
            testthat::expect_equal(nrow(result$pred), 12)
            testthat::expect_equal(ncol(result$pred), 2)
            testthat::expect_equal(colnames(result$pred), c("prev", "sd"))
        })
    })

    test_that("predictions have compatible time series properties", {
        with_registered_models(univariate_only = TRUE, function(tipo) {
            serie <- make_univariate_series(n = 120, frequency = 12, seed = 222)
            mod <- estimamodelo(serie, tipo)
            pred <- predict(mod, n.ahead = 12)

            end_time <- end(serie)
            start_year <- end_time[1]
            start_period <- end_time[2] + 1

            if (start_period > frequency(serie)) {
                start_year <- start_year + 1
                start_period <- 1
            }

            expect_compatible_tsp(pred, expected_freq = frequency(serie))
            testthat::expect_equal(start(pred), c(start_year, start_period))
        })
    })

    test_that("all models support different n.ahead values", {
        with_registered_models(univariate_only = TRUE, function(tipo) {
            serie <- make_univariate_series(seed = 333)
            mod <- estimamodelo(serie, tipo)

            for (n in c(1, 6, 12, 24)) {
                pred <- predict(mod, n.ahead = n)
                expect_prediction_format(pred, n.ahead = n)
            }
        })
    })

    test_that("all models support update workflow", {
        with_registered_models(function(tipo) {
            result <- test_model_workflow(tipo, test_update = TRUE, n.ahead = 12)

            expect_update_preserves_class(result$updated, result$fit)
        })
    })

    test_that("update with new data produces valid predictions", {
        with_registered_models(univariate_only = TRUE, function(tipo) {
            serie1 <- make_univariate_series(n = 100, seed = 555)
            mod <- estimamodelo(serie1, tipo)

            serie2 <- make_univariate_series(n = 120, seed = 555)
            updated <- update(mod, newserie = serie2)

            pred <- predict(updated, n.ahead = 12)
            expect_prediction_format(pred, n.ahead = 12)
        })
    })

    test_that("all models inherit from modprevU and modprev", {
        with_registered_models(function(tipo) {
            result <- test_model_workflow(tipo, n.ahead = 12)

            classes <- class(result$fit)
            expect_true("modprevU" %in% classes)
            expect_true("modprev" %in% classes)
            expect_equal(classes[length(classes)], "modprev")
        })
    })

    test_that("model tipo appears in class hierarchy", {
        with_registered_models(function(tipo) {
            result <- test_model_workflow(tipo, n.ahead = 12)

            expect_true(tipo %in% class(result$fit))
        })
    })

    test_that("all models have required components", {
        with_registered_models(function(tipo) {
            result <- test_model_workflow(tipo, n.ahead = 12)

            required <- c("modelo", "serie")
            expect_true(all(required %in% names(result$fit)))
        })
    })

    test_that("all models store original series correctly", {
        with_registered_models(univariate_only = TRUE, function(tipo) {
            serie <- make_univariate_series(seed = 999)
            mod <- estimamodelo(serie, tipo)

            expect_equal(mod$serie, serie)
        })
    })
})
