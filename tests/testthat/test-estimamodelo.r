test_that("dispatch with registered models", {
    f <- estimamodelo_U

    saved_registry <- modprev:::.get_models()
    on.exit(modprev:::.set_models(saved_registry), add = TRUE)

    modprev:::.init_registry()
    modprev:::.clear_registry()

    dummy_fit <- function(serie, ...) {
        new_modprevU(list(params = mean(serie)), serie, "test_dispatch", list())
    }
    dummy_predict <- function(object, n.ahead, ...) ts(rep(0, n.ahead))
    dummy_update <- function(object, newseries, refit = FALSE, ...) object

    register_model("test_dispatch", dummy_fit, dummy_predict, dummy_update)

    serie <- ts(1:10)
    result <- f(serie, "test_dispatch")

    expect_s3_class(result, "modprevU")
    expect_equal(result$modelo$params, mean(serie))
})

test_that("dispatch falls back to legacy for unregistered models", {
    f <- estimamodelo_U

    saved_registry <- modprev:::.get_models()
    on.exit(modprev:::.set_models(saved_registry), add = TRUE)

    modprev:::.init_registry()
    modprev:::.clear_registry()

    serie <- AirPassengers
    result <- f(serie, "sarima")

    expect_s3_class(result, "modprevU")
    expect_s3_class(result, "sarima")
})

test_that("dispatch passes ... arguments correctly via registry", {
    f <- estimamodelo_U

    saved_registry <- modprev:::.get_models()
    on.exit(modprev:::.set_models(saved_registry), add = TRUE)

    modprev:::.init_registry()
    modprev:::.clear_registry()

    capture_args_fit <- function(serie, custom_arg = NULL, ...) {
        new_modprevU(
            list(custom = custom_arg),
            serie,
            "test_args",
            list(args = list(...))
        )
    }
    dummy_predict <- function(object, n.ahead, ...) ts(rep(0, n.ahead))
    dummy_update <- function(object, newseries, refit = FALSE, ...) object

    register_model("test_args", capture_args_fit, dummy_predict, dummy_update)

    serie <- ts(1:10)
    result <- f(serie, "test_args", custom_arg = "test_value", extra = 123)

    expect_equal(result$modelo$custom, "test_value")
    expect_equal(attr(result, "mod_atrs")$args$extra, 123)
})

test_that("dispatch passes ... arguments correctly via legacy", {
    f <- estimamodelo_U

    serie <- AirPassengers
    result <- f(serie, "sarima", max.d = 0)

    expect_s3_class(result, "sarima")
    expect_equal(result$modelo$arma[6], 0)
})

test_that("legacy dispatch behavior", {
    f <- estimamodelo_U
    expect_true(is.function(f))

    saved_registry <- modprev:::.get_models()
    on.exit(modprev:::.set_models(saved_registry), add = TRUE)

    modprev:::.init_registry()
    modprev:::.clear_registry()

    test_that("falls back to legacy for sarima", {
        serie <- AirPassengers
        result <- f(serie, "sarima")

        expect_s3_class(result, "modprevU")
        expect_s3_class(result, "sarima")
        expect_true(!is.null(result$modelo))
    })

    test_that("falls back to legacy for reg_lin", {
        serie <- ts(1:100)
        regdata <- data.frame(x = 1:100)

        result <- suppressWarnings(f(serie, "reg_lin", regdata = regdata))

        expect_s3_class(result, "modprevU")
        expect_s3_class(result, "reg_lin")
    })

    test_that("preserves error messages for nonexistent models", {
        serie <- ts(1:10)

        expect_error(
            f(serie, "nonexistent_model"),
            "not found"
        )
    })

    test_that("passes ... arguments to legacy models", {
        serie <- AirPassengers

        result <- f(serie, "sarima", max.d = 0, max.p = 2)

        expect_s3_class(result, "sarima")
        expect_equal(result$modelo$arma[6], 0)
    })
})

test_that("estimamodelo_U integration", {
    f <- estimamodelo_U
    expect_true(is.function(f))

    test_that("estimamodelo_U works with unregistered models (legacy path)", {
        serie <- AirPassengers
        result <- f(serie, "sarima")

        expect_s3_class(result, "modprevU")
        expect_s3_class(result, "sarima")
    })

    test_that("estimamodelo_U works with registered models (registry path)", {
        saved_registry <- modprev:::.get_models()
        on.exit(modprev:::.set_models(saved_registry), add = TRUE)

        modprev:::.init_registry()
        modprev:::.clear_registry()

        dummy_fit <- function(serie, ...) {
            new_modprevU(list(mean = mean(serie)), serie, "test_integrated", list())
        }
        dummy_predict <- function(object, n.ahead, ...) ts(rep(0, n.ahead))
        dummy_update <- function(object, newseries, refit = FALSE, ...) object

        register_model("test_integrated", dummy_fit, dummy_predict, dummy_update)

        serie <- ts(1:100)
        result <- f(serie, "test_integrated")

        expect_s3_class(result, "modprevU")
        expect_equal(result$modelo$mean, mean(serie))
    })

    test_that("estimamodelo_U maintains backward compatibility", {
        serie <- AirPassengers

        result_old <- f(serie, "sarima")

        expect_s3_class(result_old, "modprevU")
        expect_s3_class(result_old, "sarima")
        expect_true(!is.null(result_old$modelo))
    })

    test_that("estimamodelo_U passes arguments to both paths", {
        serie <- AirPassengers

        result_legacy <- f(serie, "sarima", max.d = 0)
        expect_equal(result_legacy$modelo$arma[6], 0)

        saved_registry <- modprev:::.get_models()
        on.exit(modprev:::.set_models(saved_registry), add = TRUE)

        modprev:::.init_registry()
        modprev:::.clear_registry()

        test_fit <- function(serie, test_param = NULL, ...) {
            new_modprevU(list(param = test_param), serie, "test_params", list())
        }
        dummy_predict <- function(object, n.ahead, ...) ts(rep(0, n.ahead))
        dummy_update <- function(object, newseries, refit = FALSE, ...) object

        register_model("test_params", test_fit, dummy_predict, dummy_update)

        serie_test <- ts(1:10)
        result_registry <- f(serie_test, "test_params", test_param = "value")
        expect_equal(result_registry$modelo$param, "value")
    })

    test_that("estimamodelo_U works with reg_lin and regdata", {
        serie <- ts(1:100)
        regdata <- data.frame(x = 1:100)

        expect_warning(result <- f(serie, "reg_lin", regdata = regdata))

        expect_s3_class(result, "modprevU")
        expect_s3_class(result, "reg_lin")
    })
})

test_that("integration with existing tests", {
    expect_true(TRUE) # dummy pra nao ter aviso de teste vazio
    test_that("sarima still works exactly as before", {
        compmod <- forecast::auto.arima(AirPassengers)
        mod <- estimamodelo(AirPassengers, tipo = "sarima")

        expect_equal("sarima", class(mod)[1])
        expect_equal(coef(compmod), coef(mod$modelo))
    })

    test_that("reg_lin still works exactly as before", {
        serie <- ts(1:100)
        regdata <- data.frame(x = 1:100)

        expect_warning(mod <- estimamodelo(serie, "reg_lin", regdata = regdata))

        expect_s3_class(mod, "reg_lin")
        expect_s3_class(mod, "modprevU")
    })
})
