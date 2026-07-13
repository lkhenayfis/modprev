####################################################################################################
# MODELS WITHOUT EXPLANATORY VARIABLES — FIT + PREDICT BASICS
####################################################################################################

test_that("models without explanatory variables — fit + predict basics", {

    serie_std <- make_seasonal_series(n = 240, frequency = 12, seed = 123)

    cache <- with_registered_models(no_regdata_only = TRUE, function(tipo) {
        fit <- estimamodelo(serie_std, tipo)
        pred <- predict(fit, n.ahead = 12)
        list(fit = fit, pred = pred)
    })
    expect_true(length(cache) > 0)

    test_that("all models without explanatory variables produce valid modprevU structure", {
        with_registered_models(no_regdata_only = TRUE, function(tipo) {
            expect_modprev_structure(cache[[tipo]]$fit, tipo)
        })
    })

    test_that("all models without explanatory variables store original series correctly", {
        with_registered_models(no_regdata_only = TRUE, function(tipo) {
            expect_equal(cache[[tipo]]$fit$serie, serie_std)
        })
    })

    test_that("predictions have correct format", {
        with_registered_models(no_regdata_only = TRUE, function(tipo) {
            expect_prediction_format(cache[[tipo]]$pred, n.ahead = 12, allow_na_sd = TRUE)
        })
    })

    test_that("predictions have compatible time series properties", {
        with_registered_models(no_regdata_only = TRUE, function(tipo) {
            pred <- cache[[tipo]]$pred

            end_time <- end(serie_std)
            start_year <- end_time[1]
            start_period <- end_time[2] + 1

            if (start_period > frequency(serie_std)) {
                start_year <- start_year + 1
                start_period <- 1
            }

            expect_compatible_tsp(pred, expected_freq = frequency(serie_std))
            expect_equal(start(pred), c(start_year, start_period))
        })
    })

    test_that("multiple predictions without update are consistent", {
        with_registered_models(no_regdata_only = TRUE, function(tipo) {
            pred1 <- cache[[tipo]]$pred
            pred2 <- predict(cache[[tipo]]$fit, n.ahead = 12)
            pred3 <- predict(cache[[tipo]]$fit, n.ahead = 12)

            expect_equal(pred1, pred2)
            expect_equal(pred2, pred3)
        })
    })

})

####################################################################################################
# ALL MODELS — FULL WORKFLOW (FIT + PREDICT + UPDATE)
####################################################################################################

test_that("all models — full workflow", {

    cache <- with_registered_models(function(tipo) {
        test_model_workflow(tipo, test_update = TRUE, n.ahead = 12)
    })

    test_that("all models — pred shape, fit class hierarchy, and required components", {
        with_registered_models(function(tipo) {
            result <- cache[[tipo]]

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

    test_that("all models support update workflow", {
        with_registered_models(function(tipo) {
            result <- cache[[tipo]]
            expect_update_preserves_class(result$updated, result$fit)
        })
    })

})

####################################################################################################
# MODELS WITHOUT EXPLANATORY VARIABLES — UPDATE WITH NEW SERIES
####################################################################################################

test_that("models without explanatory variables — update with new series", {

    serie_base <- make_seasonal_series(n = 300, seed = 555)
    serie_longer <- make_seasonal_series(n = 360, seed = 555)
    serie_shorter <- make_seasonal_series(n = 240, seed = 211)

    cache <- with_registered_models(no_regdata_only = TRUE, function(tipo) {
        mod <- estimamodelo(serie_base, tipo)
        updated_longer <- update(mod, newserie = serie_longer)
        updated_shorter <- update(mod, newserie = serie_shorter)
        list(
            updated_longer = updated_longer,
            updated_shorter = updated_shorter,
            pred_longer = predict(updated_longer, n.ahead = 12),
            pred_shorter = predict(updated_shorter, n.ahead = 12)
        )
    })
    expect_true(length(cache) > 0)

    test_that("update with longer series produces valid predictions", {
        with_registered_models(no_regdata_only = TRUE, function(tipo) {
            expect_prediction_format(cache[[tipo]]$pred_longer, n.ahead = 12, allow_na_sd = TRUE)
        })
    })

    test_that("update with shorter series preserves new series and predicts", {
        with_registered_models(no_regdata_only = TRUE, function(tipo) {
            expect_equal(cache[[tipo]]$updated_shorter$serie, serie_shorter)

            expect_prediction_format(
                cache[[tipo]]$pred_shorter,
                n.ahead = 12,
                allow_na_sd = TRUE
            )
        })
    })

})

####################################################################################################
# REGRESSION MODELS — FIT STRUCTURE
####################################################################################################

test_that("all regression models produce valid modprevU structure", {
    data_std <- make_regression_data(n = 100, n_predictors = 2, seed = 456)

    with_registered_models(regression_only = TRUE, function(tipo) {
        mod <- estimamodelo(data_std$serie, tipo, regdata = data_std$regdata)
        expect_modprev_structure(mod, tipo)
    })
})

####################################################################################################
# MODELS WITHOUT EXPLANATORY VARIABLES — N.AHEAD VARIATIONS
####################################################################################################

test_that("all models without explanatory variables support different n.ahead values", {
    serie_std <- make_seasonal_series(240, seed = 333)
    n_ahead_values <- c(1, 6, 12, 24)

    with_registered_models(no_regdata_only = TRUE, function(tipo) {
        fit <- estimamodelo(serie_std, tipo)
        for (n in n_ahead_values) {
            pred <- predict(fit, n.ahead = n)
            expect_prediction_format(pred, n.ahead = n, allow_na_sd = TRUE)
        }
    })
})

####################################################################################################
# MODELS WITHOUT EXPLANATORY VARIABLES — JANELAMOVEL COMPATIBILITY
####################################################################################################

test_that("all models without explanatory variables are janelamovel compatible", {
    serie <- make_seasonal_series(242, seed = 123)
    with_registered_models(no_regdata_only = TRUE, function(tipo) {
        expect_janelamovel_compatible(tipo, serie = serie, config = jm_config(janela = 240))
    })
})

####################################################################################################
# MODELS WITHOUT EXPLANATORY VARIABLES — PERIODIC COMPATIBILITY
####################################################################################################

test_that("all models without explanatory variables are periodic compatible", {
    serie <- make_seasonal_series(240, seed = 444)
    with_registered_models(no_regdata_only = TRUE, function(tipo) {
        if (tipo %in% c("ss_ar1_saz", "PAR", "PAR_A")) {
            testthat::skip("ss_ar1_saz|PAR|PAR_A periodic adapter known incompatible")
        }
        expect_periodic_compatible(tipo, serie = serie)
    })
})
