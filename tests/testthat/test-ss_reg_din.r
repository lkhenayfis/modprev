
test_that("Estimacao de modelo S.S. RegDin - regressao simples", {
    serie <- window(datregdin[[1]], 1, 200)
    varex <- data.frame(venprev = window(datregdin[[2]], 1, 200))
    mod   <- estimamodelo(serie, regdata = varex, tipo = "ss_reg")

    expect_equal("ss_reg_din", attr(mod, "tipo"))
    expect_equal(mod$modelo["Q"][1, 1, 1], 0.059735078) # precalculados e testados para garantir
    expect_equal(mod$modelo["H"][1, 1, 1], 0.67102834)

    serie <- c(serie)
    mod   <- estimamodelo(serie, regdata = varex, tipo = "ss_reg")

    expect_error(estimamodelo(serie, tipo = "ss_reg"))

    serie <- datregdin[[1]]
    varex <- data.frame(venprev = datregdin[[2]])
    expect_warning(estimamodelo(serie, regdata = varex, tipo = "ss_reg"))
})


test_that("Previsao de modelo S.S. RegDin - regressao simples", {
    serie <- window(datregdin[[1]], 1, 200)
    varex <- data.frame(venprev = window(datregdin[[2]], 1, 200))
    mod   <- estimamodelo(serie, regdata = varex, tipo = "ss_reg")

    newdata <- data.frame(venprev = window(datregdin[[2]], 201, 220))
    prev    <- predict(mod, newdata = newdata)

    expect_true(all(dim(prev) == c(20, 2)))
    expect_equal(c("prev", "sd"), colnames(prev))

    expect_snapshot_value(round(c(prev), 10), style = "deparse")

    prev <- predict(mod, newdata = newdata, n.ahead = 10)

    expect_true(all(dim(prev) == c(10, 2)))
    expect_equal(c("prev", "sd"), colnames(prev))

    expect_snapshot_value(round(c(prev), 10), style = "deparse")

    expect_error(predict(mod))
})

test_that("Atualizacao de modelo S.S. RegDin", {
    serie1 <- window(datregdin[[1]], 1, 300)
    varex1 <- data.frame(venprev = window(datregdin[[2]], 1, 300))
    serie2 <- window(datregdin[[1]], 501, 900)
    varex2 <- data.frame(venprev = window(datregdin[[2]], 501, 900))

    mod <- estimamodelo(serie1, tipo = "ss_reg_din", regdata = varex1)

    mod_upd <- update(mod, serie2, newregdata = varex2)
    expect_equal(c(mod$modelo["Q"]), c(mod_upd$modelo["Q"]))
    expect_equal(c(mod$modelo["H"]), c(mod_upd$modelo["H"]))
    expect_equal(c(mod$modelo["T"]), c(mod_upd$modelo["T"]))
    expect_equal(mod_upd$modelo["Z"][, 2, ], c(varex2[[1]]))
    expect_equal(c(mod_upd$modelo$y), c(serie2))

    mod_refit <- update(mod, serie2, newregdata = varex2, refit = TRUE)
    expect_equal(c(mod_refit$modelo$y), c(serie2))
    expect_snapshot_value(mod$modelo["Q"], style = "deparse")
    expect_snapshot_value(mod$modelo["H"], style = "deparse")
    expect_snapshot_value(mod$modelo["Z"], style = "deparse")
    expect_snapshot_value(mod$modelo["T"], style = "deparse")
})
