
test_that("Estimacao de modelo SARIMA", {
    compmod <- forecast::auto.arima(AirPassengers)
    mod     <- estimamodelo(AirPassengers, tipo = "sarima")

    expect_equal("sarima", attr(mod, "tipo"))
    expect_equal(coef(compmod), coef(mod$model))
})

test_that("Previsao de modelo SARIMA", {
    compmod <- forecast::auto.arima(AirPassengers)
    mod     <- estimamodelo(AirPassengers, tipo = "sarima")

    prevcomp <- predict(compmod, n.ahead = 24, plot = FALSE)
    prev     <- predict(mod, n.ahead = 24, plot = FALSE)

    expect_true(all(dim(prev) == c(24, 2)))
    expect_equal(c("prev", "sd"), colnames(prev))

    expect_true(all(prev[, 1] == prevcomp[[1]]))
    expect_true(all(prev[, 2] == prevcomp[[2]]))
})

test_that("Atualizacao de modelo SARIMA", {
    serie1 <- window(datregdin[[1]], 1, 300)
    serie2 <- window(datregdin[[1]], 501, 900)

    mod <- estimamodelo(serie1, tipo = "sarima")

    mod_upd <- update(mod, serie2)
    expect_equal(coef(mod$modelo), coef(mod_upd$modelo))
    expect_equal(mod_upd$modelo$x, serie2)

    mod_refit <- update(mod, serie2, refit = TRUE)
    expect_equal(mod_refit$modelo$x, serie2)
    expect_snapshot_value(round(coef(mod_refit$modelo), 10), style = "deparse")
})
