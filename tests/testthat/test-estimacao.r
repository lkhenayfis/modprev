
test_that("Estimacao de modelo ARIMA", {
    compmod <- forecast::auto.arima(AirPassengers)
    mod     <- estimamodelo(AirPassengers, tipo = "sarima")

    expect_equal("sarima", attr(mod, "tipo"))
    expect_equal(coef(compmod), coef(mod$model))
})

test_that("Estimacao de modelo S.S. AR1+Saz", {
    mod <- estimamodelo(AirPassengers, tipo = "ss_ar1_saz")

    expect_equal("ss_ar1_saz", attr(mod, "tipo"))
    expect_equal(mod$modelo["Q"][1, 1, 1], 17.183724) # precalculados e testados para garantir
    expect_equal(mod$modelo["Q"][2, 2, 1], 172.71706)
    expect_equal(mod$modelo["T"]["custom2", "custom2", 1], 0.9957003)
})

test_that("Estimacao de modelo S.S. RegDin - regressao simples", {
    serie <- window(datregdin[[1]], 1, 200)
    varex <- data.frame(venprev = window(datregdin[[2]], 1, 200))
    mod   <- estimamodelo(serie, regdata = varex, tipo = "ss_reg")

    expect_equal("ss_reg_din", attr(mod, "tipo"))
    expect_equal(mod$modelo["Q"][1, 1, 1], 0.059735078) # precalculados e testados para garantir
    expect_equal(mod$modelo["H"][1, 1, 1], 0.67102834)

    expect_error(estimamodelo(serie, tipo = "ss_reg"))

    # testa aviso quando tem NA na variavel explicativa
    serie <- datregdin[[1]]
    varex <- data.frame(venprev = datregdin[[2]])
    expect_warning(estimamodelo(serie, regdata = varex, tipo = "ss_reg"))
})
