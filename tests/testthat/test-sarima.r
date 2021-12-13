
test_that("Estimacao de modelo ARIMA", {
    compmod <- forecast::auto.arima(AirPassengers)
    mod     <- estimamodelo(AirPassengers, tipo = "sarima")

    expect_equal("sarima", attr(mod, "tipo"))
    expect_equal(coef(compmod), coef(mod$model))
})
