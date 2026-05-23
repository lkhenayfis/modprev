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

    test_that("all models — pred shape, fit class hierarchy, and required components", {
        with_registered_models(function(tipo) {
            result <- test_model_workflow(tipo, n.ahead = 12)

            expect_s3_class(result$pred, "ts")
            expect_equal(nrow(result$pred), 12)
            expect_equal(ncol(result$pred), 2)
            expect_equal(colnames(result$pred), c("prev", "sd"))

            classes <- class(result$fit)
            expect_true("modprevU" %in% classes)
            expect_true("modprev" %in% classes)
            expect_equal(classes[length(classes)], "modprev")
            expect_true(tipo %in% classes)

            required <- c("modelo", "serie")
            expect_true(all(required %in% names(result$fit)))
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
            expect_equal(start(pred), c(start_year, start_period))
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

    test_that("all models store original series correctly", {
        with_registered_models(univariate_only = TRUE, function(tipo) {
            serie <- make_univariate_series(seed = 999)
            mod <- estimamodelo(serie, tipo)

            expect_equal(mod$serie, serie)
        })
    })

    test_that("multiple predictions without update are consistent", {
        with_registered_models(univariate_only = TRUE, function(tipo) {
            serie <- make_univariate_series(seed = 212)
            mod <- estimamodelo(serie, tipo)

            pred1 <- predict(mod, n.ahead = 12)
            pred2 <- predict(mod, n.ahead = 12)
            pred3 <- predict(mod, n.ahead = 12)

            expect_equal(pred1, pred2)
            expect_equal(pred2, pred3)
        })
    })

    test_that("update with shorter series works correctly", {
        with_registered_models(univariate_only = TRUE, function(tipo) {
            serie1 <- make_univariate_series(n = 120, seed = 210)
            mod <- estimamodelo(serie1, tipo)

            serie2 <- make_univariate_series(n = 100, seed = 211)
            updated <- update(mod, newserie = serie2)

            expect_equal(updated$serie, serie2)

            models_without_sd <- c("reg_quant", "BOOST", "LGBM")
            allow_na <- tipo %in% models_without_sd

            pred <- predict(updated, n.ahead = 12)
            expect_prediction_format(pred, n.ahead = 12, allow_na_sd = allow_na)
        })
    })

    test_that("all univariate models are janelamovel compatible", {
        serie <- make_univariate_series(seed = 444)
        with_registered_models(univariate_only = TRUE, function(tipo) {
            expect_janelamovel_compatible(tipo, serie = serie)
        })
    })

    test_that("all univariate models are periodic compatible", {
        serie <- make_univariate_series(seed = 444)
        with_registered_models(univariate_only = TRUE, function(tipo) {
            if (tipo == "ss_ar1_saz") {
                testthat::skip("ss_ar1_saz periodic adapter known incompatible")
            }
            expect_periodic_compatible(tipo, serie = serie)
        })
    })

})
