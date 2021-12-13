
test_that("Estimacao de modelo S.S. AR1+Saz", {
    mod <- estimamodelo(AirPassengers, tipo = "ss_ar1_saz")

    expect_equal("ss_ar1_saz", attr(mod, "tipo"))
    expect_equal(mod$modelo["Q"][1, 1, 1], 17.183724) # precalculados e testados para garantir
    expect_equal(mod$modelo["Q"][2, 2, 1], 172.71706)
    expect_equal(mod$modelo["T"]["custom2", "custom2", 1], 0.9957003)
})

test_that("Previsao de modelo S.S. AR1+Saz", {
    mod <- estimamodelo(AirPassengers, tipo = "ss_ar1_saz")

    prev <- predict(mod, n.ahead = 24, plot = FALSE)

    expect_true(all(dim(prev) == c(24, 2)))
    expect_equal(c("prev", "sd"), colnames(prev))

    expect_snapshot_value(round(c(prev), 10), style = "deparse")
})

test_that("Atualizacao de modelo S.S. AR1+Saz", {
    serie1 <- window(datregdin[[1]], 1, 300)
    serie2 <- window(datregdin[[1]], 501, 900)

    mod <- estimamodelo(serie1, tipo = "ss_ar1_saz")

    mod_upd <- update(mod, serie2)
    expect_equal(c(mod$modelo["Q"]), c(mod_upd$modelo["Q"]))
    expect_equal(c(mod$modelo["H"]), c(mod_upd$modelo["H"]))
    expect_equal(c(mod$modelo["Z"]), c(mod_upd$modelo["Z"]))
    expect_equal(c(mod$modelo["T"]), c(mod_upd$modelo["T"]))
    expect_equal(c(mod_upd$modelo$y), c(serie2))

    mod_refit <- update(mod, serie2, refit = TRUE)
    expect_equal(c(mod_refit$modelo$y), c(serie2))
    expect_snapshot_value(mod$modelo["Q"], style = "deparse")
    expect_snapshot_value(mod$modelo["H"], style = "deparse")
    expect_snapshot_value(mod$modelo["Z"], style = "deparse")
    expect_snapshot_value(mod$modelo["T"], style = "deparse")
})
