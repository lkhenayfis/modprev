test_that(".onLoad", {
    f <- modprev:::.onLoad
    expect_true(is.function(f))

    test_that(".onLoad initializes registry", {
        modprev:::.clear_registry()

        modprev:::.onLoad(NULL, NULL)

        expect_true(modprev:::.is_registry_initialized())
    })

    test_that(".onLoad calls .register_builtin_models", {
        modprev:::.clear_registry()

        modprev:::.onLoad(NULL, NULL)

        registry <- get_registry()
        expect_s3_class(registry, "model_registry")
    })

    test_that(".onLoad survives multiple calls", {
        modprev:::.onLoad(NULL, NULL)
        modprev:::.onLoad(NULL, NULL)
        modprev:::.onLoad(NULL, NULL)

        expect_true(modprev:::.is_registry_initialized())
    })

    test_that(".onLoad returns invisibly", {
        result <- withVisible(modprev:::.onLoad(NULL, NULL))

        expect_false(result$visible)
        expect_null(result$value)
    })
})

test_that(".onUnload", {
    f <- modprev:::.onUnload
    expect_true(is.function(f))

    test_that(".onUnload clears registry", {
        modprev:::.onLoad(NULL, NULL)

        modprev:::.onUnload(NULL)

        registry <- get_registry()
        expect_equal(length(registry$models), 0)
    })

    test_that(".onUnload maintains registry initialization", {
        modprev:::.onLoad(NULL, NULL)

        modprev:::.onUnload(NULL)

        expect_true(modprev:::.is_registry_initialized())
    })

    test_that(".onUnload returns invisibly", {
        result <- withVisible(modprev:::.onUnload(NULL))

        expect_false(result$visible)
        expect_null(result$value)
    })
})

test_that(".register_builtin_models", {
    f <- modprev:::.register_builtin_models
    expect_true(is.function(f))

    test_that(".register_builtin_models runs without error on clean registry", {
        modprev:::.clear_registry()
        modprev:::.init_registry()

        expect_error(modprev:::.register_builtin_models(), NA)
    })

    test_that(".register_builtin_models returns invisibly", {
        modprev:::.clear_registry()
        modprev:::.init_registry()

        result <- withVisible(modprev:::.register_builtin_models())

        expect_false(result$visible)
        expect_null(result$value)
    })
})

test_that("package load integration", {
    test_that("registry is available after package load", {
        registry <- get_registry()

        expect_s3_class(registry, "model_registry")
    })

    test_that("registry survives load/unload cycle", {
        modprev:::.onLoad(NULL, NULL)
        expect_true(modprev:::.is_registry_initialized())

        modprev:::.onUnload(NULL)
        expect_true(modprev:::.is_registry_initialized())

        modprev:::.onLoad(NULL, NULL)
        expect_true(modprev:::.is_registry_initialized())
    })
})

test_that("sarima model registration", {
    test_that("sarima is registered on package load", {
        expect_true(is_registered("sarima"))
    })

    test_that("sarima has correct metadata", {
        spec <- get_model("sarima")

        expect_s3_class(spec, "model_spec")
        expect_equal(spec$tipo, "sarima")
        expect_false(spec$requires_regdata)
        expect_true("forecast" %in% spec$deps)
    })

    test_that("sarima works through registry", {
        mod <- estimamodelo(AirPassengers, "sarima")

        expect_s3_class(mod, "sarima")
        expect_s3_class(mod, "modprevU")
    })

    test_that("sarima predict works through registry", {
        mod <- estimamodelo(AirPassengers, "sarima")
        pred <- predict(mod, n.ahead = 12)

        expect_equal(nrow(pred), 12)
        expect_equal(colnames(pred), c("prev", "sd"))
    })

    test_that("sarima appears in list_models", {
        models <- list_models()

        expect_true("sarima" %in% models)
    })

    test_that("sarima has correct details in list_models", {
        details <- list_models(details = TRUE)
        sarima_row <- details[details$tipo == "sarima", ]

        expect_false(sarima_row$requires_regdata)
    })
})

test_that("ss_ar1_saz model registration", {
    test_that("ss_ar1_saz is registered on package load", {
        expect_true(is_registered("ss_ar1_saz"))
    })

    test_that("ss_ar1_saz has correct metadata", {
        spec <- get_model("ss_ar1_saz")

        expect_s3_class(spec, "model_spec")
        expect_equal(spec$tipo, "ss_ar1_saz")
        expect_false(spec$requires_regdata)
        expect_true("KFAS" %in% spec$deps)
    })

    test_that("ss_ar1_saz works through registry", {
        mod <- estimamodelo(AirPassengers, "ss_ar1_saz")

        expect_s3_class(mod, "ss_ar1_saz")
        expect_s3_class(mod, "modprevU")
    })

    test_that("ss_ar1_saz predict works through registry", {
        mod <- estimamodelo(AirPassengers, "ss_ar1_saz")
        pred <- predict(mod, n.ahead = 12)

        expect_equal(nrow(pred), 12)
        expect_equal(colnames(pred), c("prev", "sd"))
    })

    test_that("ss_ar1_saz appears in list_models", {
        models <- list_models()

        expect_true("ss_ar1_saz" %in% models)
    })

    test_that("ss_ar1_saz has correct details in list_models", {
        details <- list_models(details = TRUE)
        ss_row <- details[details$tipo == "ss_ar1_saz", ]

        expect_false(ss_row$requires_regdata)
    })
})

test_that("sarimax model registration", {
    test_that("sarimax is registered on package load", {
        expect_true(is_registered("sarimax"))
    })

    test_that("sarimax has correct metadata", {
        spec <- get_model("sarimax")

        expect_s3_class(spec, "model_spec")
        expect_equal(spec$tipo, "sarimax")
        expect_true(spec$requires_regdata)
        expect_true("forecast" %in% spec$deps)
    })

    test_that("sarimax appears in list_models", {
        models <- list_models()

        expect_true("sarimax" %in% models)
    })

    test_that("sarimax has correct details in list_models", {
        details <- list_models(details = TRUE)
        row <- details[details$tipo == "sarimax", ]

        expect_true(row$requires_regdata)
    })
})

test_that("reg_lin model registration", {
    test_that("reg_lin is registered on package load", {
        expect_true(is_registered("reg_lin"))
    })

    test_that("reg_lin has correct metadata", {
        spec <- get_model("reg_lin")

        expect_s3_class(spec, "model_spec")
        expect_equal(spec$tipo, "reg_lin")
        expect_true(spec$requires_regdata)
        expect_true("stats" %in% spec$deps)
    })

    test_that("reg_lin appears in list_models", {
        models <- list_models()

        expect_true("reg_lin" %in% models)
    })

    test_that("reg_lin has correct details in list_models", {
        details <- list_models(details = TRUE)
        row <- details[details$tipo == "reg_lin", ]

        expect_true(row$requires_regdata)
    })
})

test_that("reg_quant model registration", {
    test_that("reg_quant is registered on package load", {
        expect_true(is_registered("reg_quant"))
    })

    test_that("reg_quant has correct metadata", {
        spec <- get_model("reg_quant")

        expect_s3_class(spec, "model_spec")
        expect_equal(spec$tipo, "reg_quant")
        expect_true(spec$requires_regdata)
        expect_true("quantreg" %in% spec$deps)
    })

    test_that("reg_quant appears in list_models", {
        models <- list_models()

        expect_true("reg_quant" %in% models)
    })

    test_that("reg_quant has correct details in list_models", {
        details <- list_models(details = TRUE)
        row <- details[details$tipo == "reg_quant", ]

        expect_true(row$requires_regdata)
    })
})

test_that("ss_reg_din model registration", {
    test_that("ss_reg_din is registered on package load", {
        expect_true(is_registered("ss_reg_din"))
    })

    test_that("ss_reg_din has correct metadata", {
        spec <- get_model("ss_reg_din")

        expect_s3_class(spec, "model_spec")
        expect_equal(spec$tipo, "ss_reg_din")
        expect_true(spec$requires_regdata)
        expect_true("KFAS" %in% spec$deps)
    })

    test_that("ss_reg_din appears in list_models", {
        models <- list_models()

        expect_true("ss_reg_din" %in% models)
    })

    test_that("ss_reg_din has correct details in list_models", {
        details <- list_models(details = TRUE)
        row <- details[details$tipo == "ss_reg_din", ]

        expect_true(row$requires_regdata)
    })
})

test_that("GAM model registration", {
    test_that("GAM is registered on package load", {
        expect_true(is_registered("GAM"))
    })

    test_that("GAM has correct metadata", {
        spec <- get_model("GAM")

        expect_s3_class(spec, "model_spec")
        expect_equal(spec$tipo, "GAM")
        expect_true(spec$requires_regdata)
        expect_true("mgcv" %in% spec$deps)
    })

    test_that("GAM appears in list_models", {
        models <- list_models()

        expect_true("GAM" %in% models)
    })

    test_that("GAM has correct details in list_models", {
        details <- list_models(details = TRUE)
        row <- details[details$tipo == "GAM", ]

        expect_true(row$requires_regdata)
    })
})

test_that("BOOST model registration", {
    test_that("BOOST is registered on package load", {
        expect_true(is_registered("BOOST"))
    })

    test_that("BOOST has correct metadata", {
        spec <- get_model("BOOST")

        expect_s3_class(spec, "model_spec")
        expect_equal(spec$tipo, "BOOST")
        expect_true(spec$requires_regdata)
        expect_true("mboost" %in% spec$deps)
    })

    test_that("BOOST appears in list_models", {
        models <- list_models()

        expect_true("BOOST" %in% models)
    })

    test_that("BOOST has correct details in list_models", {
        details <- list_models(details = TRUE)
        row <- details[details$tipo == "BOOST", ]

        expect_true(row$requires_regdata)
    })
})

test_that("LGBM model registration", {
    test_that("LGBM is registered on package load", {
        expect_true(is_registered("LGBM"))
    })

    test_that("LGBM has correct metadata", {
        spec <- get_model("LGBM")

        expect_s3_class(spec, "model_spec")
        expect_equal(spec$tipo, "LGBM")
        expect_true(spec$requires_regdata)
        expect_true("lightgbm" %in% spec$deps)
    })

    test_that("LGBM appears in list_models", {
        models <- list_models()

        expect_true("LGBM" %in% models)
    })

    test_that("LGBM has correct details in list_models", {
        details <- list_models(details = TRUE)
        row <- details[details$tipo == "LGBM", ]

        expect_true(row$requires_regdata)
    })
})
