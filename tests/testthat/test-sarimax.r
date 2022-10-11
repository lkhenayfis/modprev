
test_that("Estimacao de modelo SARIMAX", {
    serie <- datregdin$obs
    regdata <- datregdin$varex

    compmod <- forecast::auto.arima(serie, xreg = data.matrix(regdata))
    mod     <- estimamodelo(serie, tipo = "sarima", regdata = regdata, formula = ~ V1 + V2 + V3)

    expect_equal("sarimax", class(mod)[1])
    expect_equal(coef(compmod), coef(mod$model))
    expect_equal(attr(mod, "mod_atrs")$formula, ~ V1 + V2 + V3)

    # Sem passar a formula
    expect_warning(mod2 <- estimamodelo(serie, "sarima", regdata = regdata))
    expect_equal(unname(mod2$model$coefficients), unname(mod$model$coefficients))

    # Sem passar regdata
    expect_error(mod2 <- estimamodelo(serie, "sarimax"))
})
