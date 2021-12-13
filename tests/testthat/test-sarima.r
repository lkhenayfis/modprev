
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