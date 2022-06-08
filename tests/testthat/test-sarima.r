
digits <- switch(Sys.info()[["sysname"]],
    "Windows" = 5,
    "Linux" = 5,
    "Darwin" = 2
)

test_that("Estimacao de modelo SARIMA", {
    compmod <- forecast::auto.arima(AirPassengers)
    mod     <- estimamodelo(AirPassengers, tipo = "sarima")

    expect_equal("sarima", class(mod)[1])
    expect_equal(coef(compmod), coef(mod$model))

    serie <- c(AirPassengers)
    mod   <- estimamodelo(serie, "sarima")
})

test_that("Previsao de modelo SARIMA", {
    compmod <- forecast::auto.arima(AirPassengers)
    mod     <- estimamodelo(AirPassengers, tipo = "sarima")

    prevcomp <- predict(compmod, n.ahead = 24)
    prev     <- predict(mod, n.ahead = 24)

    expect_true(all(dim(prev) == c(24, 2)))
    expect_equal(c("prev", "sd"), colnames(prev))

    expect_true(all(prev[, 1] == prevcomp[[1]]))
    expect_true(all(prev[, 2] == prevcomp[[2]]))
})

test_that("Atualizacao de modelo SARIMA", {
    serie1 <- window(AirPassengers, c(1949, 1), c(1954, 12))
    serie2 <- window(AirPassengers, c(1955, 1), c(1960, 12))

    mod <- estimamodelo(serie1, tipo = "sarima")

    mod_upd <- update(mod, serie2)
    expect_equal(coef(mod$modelo), coef(mod_upd$modelo))
    expect_equal(mod_upd$modelo$x, serie2)

    mod_refit <- update(mod, serie2, refit = TRUE)
    expect_equal(mod_refit$modelo$x, serie2)
    expect_snapshot_value(round(coef(mod_refit$modelo), digits), style = "deparse")
})
