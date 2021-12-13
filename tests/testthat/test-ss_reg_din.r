
test_that("Estimacao de modelo S.S. RegDin - regressao simples", {
    serie <- window(datregdin[[1]], 1, 200)
    varex <- data.frame(venprev = window(datregdin[[2]], 1, 200))
    mod   <- estimamodelo(serie, regdata = varex, tipo = "ss_reg")

    expect_equal("ss_reg_din", attr(mod, "tipo"))
    expect_equal(mod$modelo["Q"][1, 1, 1], 0.059735078) # precalculados e testados para garantir
    expect_equal(mod$modelo["H"][1, 1, 1], 0.67102834)

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
    prev    <- predict(mod, newdata = newdata, plot = FALSE)

    expect_true(all(dim(prev) == c(20, 2)))
    expect_equal(c("prev", "sd"), colnames(prev))

    expect_snapshot_value(round(c(prev), 10), style = "deparse")

    prev <- predict(mod, newdata = newdata, n.ahead = 10, plot = FALSE)

    expect_true(all(dim(prev) == c(10, 2)))
    expect_equal(c("prev", "sd"), colnames(prev))

    expect_snapshot_value(round(c(prev), 10), style = "deparse")

    expect_error(predict(mod))
})