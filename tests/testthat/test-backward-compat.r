test_that("all models registered", {
    test_that("list_models returns all 9 models", {
        models <- list_models()

        expected_models <- c(
            "sarima", "ss_ar1_saz",
            "sarimax", "reg_lin", "reg_quant", "ss_reg_din",
            "GAM", "BOOST", "LGBM"
        )

        for (model in expected_models) {
            expect_true(model %in% models,
                info = paste("Model", model, "should be registered"))
        }

        expect_equal(length(models), 9)
    })

    test_that("all models have correct requires_regdata flag", {
        details <- list_models(details = TRUE)

        univariate <- c("sarima", "ss_ar1_saz")
        regression <- c("sarimax", "reg_lin", "reg_quant", "ss_reg_din")
        ml <- c("GAM", "BOOST", "LGBM")

        for (model in univariate) {
            row <- details[details$tipo == model, ]
            expect_false(row$requires_regdata,
                info = paste("Univariate model", model, "should not require regdata"))
        }

        for (model in c(regression, ml)) {
            row <- details[details$tipo == model, ]
            expect_true(row$requires_regdata,
                info = paste("Model", model, "should require regdata"))
        }
    })
})

test_that("backward compatibility with estimamodelo", {
    test_that("univariate models work via estimamodelo", {
        mod_sarima <- estimamodelo(AirPassengers, "sarima")
        expect_s3_class(mod_sarima, "sarima")
        expect_s3_class(mod_sarima, "modprevU")

        mod_ss <- estimamodelo(AirPassengers, "ss_ar1_saz")
        expect_s3_class(mod_ss, "ss_ar1_saz")
        expect_s3_class(mod_ss, "modprevU")
    })

    test_that("prediction returns expected format", {
        mod <- estimamodelo(AirPassengers, "sarima")
        pred <- predict(mod, n.ahead = 12)

        expect_s3_class(pred, "ts")
        expect_equal(nrow(pred), 12)
        expect_equal(colnames(pred), c("prev", "sd"))
    })
})

test_that("periodic models work with registered models", {
    test_that("modprevP with sarima produces expected output", {
        mod <- estimamodelo(AirPassengers, "sarima", periodico = TRUE)

        expect_s3_class(mod, "modprevP")
        expect_s3_class(mod, "modprev")

        pred <- predict(mod, n.ahead = 12)
        expect_s3_class(pred, "ts")
        expect_equal(nrow(pred), 12)
    })
})
