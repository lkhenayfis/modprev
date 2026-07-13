test_that(".onLoad", {
    f <- modprev:::.onLoad
    expect_true(is.function(f))

    test_that(".onLoad initialises registry", {
        modprev:::.clear_registry()
        f(NULL, NULL)
        expect_true(exists("models", envir = modprev:::.model_registry))
    })

    test_that(".onLoad is idempotent on repeated calls", {
        f(NULL, NULL)
        f(NULL, NULL)
        expect_true(exists("models", envir = modprev:::.model_registry))
    })
})

test_that(".onUnload", {
    f <- modprev:::.onUnload
    expect_true(is.function(f))

    test_that(".onUnload clears registry", {
        modprev:::.onLoad(NULL, NULL)
        f(NULL)
        expect_length(get_registry()$models, 0)
    })
})

test_that(".register_builtin_models", {
    f <- modprev:::.register_builtin_models
    expect_true(is.function(f))

    expected_tipos <- c("sarima", "ss_ar1_saz", "sarimax", "reg_lin", "reg_quant",
        "ss_reg_din", "GAM", "BOOST", "LGBM", "PAR", "PAR_A")

    test_that(".register_builtin_models registers all 9 expected tipos", {
        modprev:::.onLoad(NULL, NULL)
        registered <- list_models()
        expect_true(all(expected_tipos %in% registered))
        expect_length(registered, length(expected_tipos))
    })

    test_that(".register_builtin_models sets correct requires_regdata flags", {
        modprev:::.onLoad(NULL, NULL)
        no_regdata_tipos <- c("sarima", "ss_ar1_saz", "PAR", "PAR_A")
        for (tipo in no_regdata_tipos) {
            expect_false(get_model(tipo)$requires_regdata, label = tipo)
        }
        regdata_tipos <- setdiff(expected_tipos, no_regdata_tipos)
        for (tipo in regdata_tipos) {
            expect_true(get_model(tipo)$requires_regdata, label = tipo)
        }
    })

    test_that(".register_builtin_models populates expected deps per tipo", {
        modprev:::.onLoad(NULL, NULL)
        expect_true("forecast" %in% get_model("sarima")$deps)
        expect_true("KFAS" %in% get_model("ss_ar1_saz")$deps)
        expect_true("parmodels" %in% get_model("PAR")$deps)
        expect_true("parmodels" %in% get_model("PAR_A")$deps)
        expect_true("forecast" %in% get_model("sarimax")$deps)
        expect_true("stats" %in% get_model("reg_lin")$deps)
        expect_true("quantreg" %in% get_model("reg_quant")$deps)
        expect_true("KFAS" %in% get_model("ss_reg_din")$deps)
        expect_true("mgcv" %in% get_model("GAM")$deps)
        expect_true("mboost" %in% get_model("BOOST")$deps)
        expect_true("lightgbm" %in% get_model("LGBM")$deps)
    })
})
