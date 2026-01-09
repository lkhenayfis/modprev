# ==============================================================================
# Workflow Integration Tests
#
# Purpose:
#   Validate complete end-to-end workflows (janelamovel, modprevP, 
#   predict-update cycles) work with all registered models.
#
# Note: Detailed workflow logic is tested in dedicated files:
#   - test-janelamovel.r: Detailed janelamovel functionality
#   - test-modprevP.r: Detailed modprevP functionality
#
# This file focuses on ensuring all models are compatible with these workflows.
# ==============================================================================

test_that("janelamovel works with all univariate models", {
    with_registered_models(univariate_only = TRUE, function(tipo) {
        serie <- make_univariate_series(n = 120, frequency = 12, seed = 201)
        expect_janelamovel_compatible(tipo, serie = serie)
    })
})

test_that("modprevP wrapping works with all univariate models", {
    with_registered_models(univariate_only = TRUE, function(tipo) {
        if (tipo == "ss_ar1_saz") {
            testthat::skip("ss_ar1_saz requires special periodic data structure")
        }

        serie <- make_univariate_series(n = 120, frequency = 12, seed = 205)
        expect_periodic_compatible(tipo, serie = serie)
    })
})

test_that("predict-update-predict cycles work for all models", {
    with_registered_models(univariate_only = TRUE, function(tipo) {
        serie1 <- make_univariate_series(n = 100, seed = 209)
        mod1 <- estimamodelo(serie1, tipo)
        pred1 <- predict(mod1, n.ahead = 12)

        models_without_sd <- c("reg_quant", "BOOST", "LGBM")
        allow_na <- tipo %in% models_without_sd

        expect_prediction_format(pred1, n.ahead = 12, allow_na_sd = allow_na)

        serie2 <- make_univariate_series(n = 120, seed = 209)
        mod2 <- update(mod1, newserie = serie2)
        pred2 <- predict(mod2, n.ahead = 12)

        expect_prediction_format(pred2, n.ahead = 12, allow_na_sd = allow_na)

        serie3 <- make_univariate_series(n = 140, seed = 209)
        mod3 <- update(mod2, newserie = serie3)
        pred3 <- predict(mod3, n.ahead = 12)

        expect_prediction_format(pred3, n.ahead = 12, allow_na_sd = allow_na)

        testthat::expect_equal(ncol(pred1), ncol(pred2))
        testthat::expect_equal(ncol(pred2), ncol(pred3))
        testthat::expect_equal(colnames(pred1), colnames(pred2))
    })
})

test_that("update with shorter series works correctly", {
    with_registered_models(univariate_only = TRUE, function(tipo) {
        serie1 <- make_univariate_series(n = 120, seed = 210)
        mod <- estimamodelo(serie1, tipo)

        serie2 <- make_univariate_series(n = 100, seed = 211)
        updated <- update(mod, newserie = serie2)

        testthat::expect_equal(updated$serie, serie2)

        models_without_sd <- c("reg_quant", "BOOST", "LGBM")
        allow_na <- tipo %in% models_without_sd

        pred <- predict(updated, n.ahead = 12)
        expect_prediction_format(pred, n.ahead = 12, allow_na_sd = allow_na)
    })
})

test_that("multiple predictions without update are consistent", {
    with_registered_models(univariate_only = TRUE, function(tipo) {
        serie <- make_univariate_series(seed = 212)
        mod <- estimamodelo(serie, tipo)

        pred1 <- predict(mod, n.ahead = 12)
        pred2 <- predict(mod, n.ahead = 12)
        pred3 <- predict(mod, n.ahead = 12)

        testthat::expect_equal(pred1, pred2)
        testthat::expect_equal(pred2, pred3)
    })
})
