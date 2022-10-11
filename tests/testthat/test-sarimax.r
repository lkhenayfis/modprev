
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

    # Quando a serie tem sazonalidade
    serie <- window(co2, end = 1963.9167)
    regdata <- head(datregdin$varex, 60)
    mod <- estimamodelo(serie, "sarima", regdata = regdata, formula = ~ V1 + V2 + V3)

    expect_equal(mod$modelo$arma, c(1, 1, 1, 0, 12, 0, 1))
})

test_that("Previsao de modelo SARIMAX", {
    yy <- window(datregdin$obs, 1, 150)
    xx <- head(datregdin$varex, 150)

    compmod <- forecast::auto.arima(yy, xreg = data.matrix(xx))
    mod <- estimamodelo(yy, "sarima", regdata = xx, formula = ~ V1 + V2 + V3)

    prevcomp <- forecast(compmod, xreg = data.matrix(datregdin$varex[151:160, ]), level = .95)
    sdcomp   <- with(prevcomp, upper - mean) / qnorm(.975)
    prev     <- predict(mod, newdata = datregdin$varex[151:160, ])

    expect_equal(prev[, 1], unname(prevcomp$mean))
    expect_equal(as.numeric(prev[, 2]), unname(as.numeric(sdcomp)))

    # Erro quando nao passa newdata

    expect_error(prev_erro <- predict(mod))

    # Manutencao de serie temporal sem sazo

    expect_equal(start(prev), c(151, 1))
    expect_equal(end(prev), c(160, 1))

    # Passando n.ahead tambem

    prev <- predict(mod, newdata = datregdin$varex[151:160, ], n.ahead = 5)
    expect_equal(nrow(prev), 5)

    # Com sazonalidade

    serie <- window(co2, end = 1963.9167)
    regdata <- head(datregdin$varex, 60)
    mod  <- estimamodelo(serie, "sarima", regdata = regdata, formula = ~ V1 + V2 + V3)
    prev <- predict(mod, newdata = datregdin$varex[151:160, ])

    expect_equal(start(prev), c(1964, 1))
    expect_equal(end(prev), c(1964, 10))
})

test_that("Atualizacao de modelo SARIMAX", {
    yy <- window(datregdin$obs, 1, 150)
    xx <- head(datregdin$varex, 150)
    mod <- estimamodelo(yy, "sarima", regdata = xx, formula = ~ V1 + V2 + V3)

    yy2 <- window(datregdin$obs, 151, 200)
    xx2 <- tail(datregdin$varex, 50)

    mod_upd <- update(mod, yy2, xx2)
    expect_equal(class(mod)[1], "sarimax")
    expect_equal(coef(mod$modelo), coef(mod_upd$modelo))
    expect_equal(mod_upd$serie, yy2)
    expect_equal(attr(mod, "mod_atrs"), attr(mod_upd, "mod_atrs"))

    # Erro quando nao passa newseries ou newregdata

    expect_error(mod_erro <- update(mod))
    expect_error(mod_erro <- update(mod, newseries = yy2))
    expect_error(mod_erro <- update(mod, newregdata = xx2))

    # Com refit

    mod_refit <- update(mod, yy2, xx2, refit = TRUE)
    expect_equal(mod_refit$serie, yy2)
    expect_equal(class(mod_refit)[1], "sarimax")
    expect_equal(attr(mod, "mod_atrs"), attr(mod_refit, "mod_atrs"))

    expect_snapshot_value(coef(mod_refit$modelo), style = "deparse")
})