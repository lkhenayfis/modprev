
test_that("Estimacao de modelo DCS RegDin - regressao simples", {
    serie <- window(datregdin$obs, 1, 200)
    varex <- datregdin$varex[, "V1", drop = FALSE]
    mod   <- estimamodelo(serie, regdata = varex, tipo = "dcs_reg_din", formula = ~ V1)

    expect_equal("dcs_reg_din", class(mod)[1])
    expect_snapshot_value(round(coef(mod$modelo), 5), style = "deparse")

    serie <- c(serie)
    mod   <- estimamodelo(serie, regdata = varex, tipo = "dcs_reg_din", formula = ~ V1)

    # sem passar regdata
    expect_error(estimamodelo(serie, tipo = "dcs_reg_din"))

    # sem passar formula
    expect_warning(estimamodelo(serie, tipo = "dcs_reg_din", regdata = varex))

    # NAs nos regressores
    varex[c(1, 10, 20), ] <- NA_real_
    expect_warning(estimamodelo(serie, regdata = varex, tipo = "dcs_reg_din", formula = ~ V1))

    # Teste de argumentos extras que nao existe na funcao

    varex <- datregdin$varex[, "V1", drop = FALSE]
    mod2 <- estimamodelo(c(serie), regdata = varex, tipo = "dcs_reg_din", formula = ~ V1, erro = 1)
    expect_equal(coef(mod$modelo), coef(mod2$modelo))
})

test_that("Previsao de modelo DCS RegDin - regressao simples", {
    serie <- window(datregdin$obs, 1, 100)
    varex <- datregdin$varex[1:100, "V1", drop = FALSE]
    mod   <- estimamodelo(serie, regdata = varex, tipo = "dcs_reg_din", formula = ~ V1)

    newdata <- datregdin$varex[101:120, "V1", drop = FALSE]
    prev    <- predict(mod, newdata = newdata, S = 10)

    expect_true(all(dim(prev) == c(20, 2)))
    expect_equal(c("prev", "sd"), colnames(prev))

    expect_snapshot_value(round(prev[1, 1], 5), style = "deparse")

    prev <- predict(mod, newdata = newdata, n.ahead = 10, S = 10)

    expect_true(all(dim(prev) == c(10, 2)))
    expect_equal(c("prev", "sd"), colnames(prev))

    expect_snapshot_value(round(prev[1, 1], 5), style = "deparse")

    expect_error(predict(mod))
})

test_that("Atualizacao de modelo DCS RegDin - regressao simples", {
    serie1 <- window(datregdin$obs, 1, 100)
    varex1 <- datregdin$varex[1:100, "V1", drop = FALSE]
    serie2 <- window(datregdin$obs, 101, 200)
    varex2 <- datregdin$varex[101:200, "V1", drop = FALSE]

    mod <- estimamodelo(serie1, tipo = "dcs_reg_din", regdata = varex1, formula = ~ V1)

    mod_upd <- update(mod, serie2, newregdata = varex2)

    # somente os estados nao dinamicos devem ser iguais -- os dinamicos sao resetados para inicializacao
    expect_equal(coef(mod$modelo)[c(1, 2, 5, 6)], coef(mod_upd$modelo)[c(1, 2, 5, 6)])
    expect_equal(mod_upd$modelo["Z"]["mu", 2, ], c(varex2[[1]]))
    expect_true(all(mod_upd$serie - serie2 == 0))

    mod_atr     <- attr(mod, "mod_atrs")
    mod_atr_upd <- attr(mod_upd, "mod_atrs")

    expect_equal(mod_atr_upd$formula, mod_atr$formula)
    expect_equal(mod_atr_upd$vardin, mod_atr$vardin)

    mod_refit <- update(mod, serie2, newregdata = varex2, refit = TRUE)
    expect_equal(c(mod_refit$modelo$series), c(serie2))
    expect_snapshot_value(round(coef(mod_refit$modelo), 5), style = "deparse")
    expect_equal(mod_refit$modelo["Z"]["mu", 2, ], c(varex2[[1]]))

    expect_equal(mod_atr_upd$formula, mod_atr$formula)
    expect_equal(mod_atr_upd$vardin, mod_atr$vardin)
})