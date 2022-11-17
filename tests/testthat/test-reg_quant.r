
test_that("Estimacao de modelo Reg Quant", {
    compmod <- rq(as.numeric(datregdin$obs) ~ data.matrix(datregdin$varex))
    mod <- estimamodelo(datregdin$obs, "reg_quant", regdata = datregdin$varex, formula = ~ V1 + V2 + V3)

    expect_equal(class(mod)[1], "reg_quant")
    expect_equal(unname(compmod$coefficients), unname(mod$model$coefficients))
    expect_equal(attr(mod, "mod_atrs")$formula, Y ~ V1 + V2 + V3)

    # Sem passar a formula
    expect_warning(mod2 <- estimamodelo(datregdin$obs, "reg_quant", regdata = datregdin$varex))
    expect_equal(unname(mod2$model$coefficients), unname(mod$model$coefficients))

    # Sem passar regdata
    expect_error(mod2 <- estimamodelo(datregdin$obs, "reg_quant"))

    # Atributo tsp de saida ----------------------------------------------------

    expect_equal(attr(mod, "mod_atrs")$tsp, c(1, 200, 1))

    ss <- ts(datregdin$obs, frequency = 10)
    mod <- estimamodelo(ss, "reg_quant", regdata = datregdin$varex, formula = ~ V1 + V2 + V3)

    expect_equal(attr(mod, "mod_atrs")$tsp, c(1, 20.9, 10))
})

test_that("Previsao de modelo Reg Quant", {
    yy <- window(datregdin$obs, 1, 150)
    xx <- head(datregdin$varex, 150)

    compmod <- rq(Y ~ V1 + V2 + V3, data = cbind(Y = as.numeric(yy), xx))
    mod <- estimamodelo(yy, "reg_quant", regdata = xx, formula = ~ V1 + V2 + V3)

    prevcomp <- predict(compmod, newdata = datregdin$varex[151:160, ])
    prev     <- predict(mod, newdata = datregdin$varex[151:160, ])

    expect_equal(as.numeric(prev[, 1]), unname(prevcomp))
    expect_true(all(is.na(prev[, 2])))

    # Erro quando nao passa newdata

    expect_error(prev_erro <- predict(mod))

    # Manutencao de serie temporal sem sazo

    expect_equal(start(prev), c(151, 1))
    expect_equal(end(prev), c(160, 1))

    # Passando n.ahead tambem

    prev <- predict(mod, newdata = datregdin$varex[151:160, ], n.ahead = 5)
    expect_equal(nrow(prev), 5)

    # Com sazonalidade

    ss <- ts(yy, frequency = 10)
    mod  <- estimamodelo(ss, "reg_quant", regdata = xx, formula = ~ V1 + V2 + V3)
    prev <- predict(mod, newdata = datregdin$varex[151:160, ])

    expect_equal(start(prev), c(16, 1))
    expect_equal(end(prev), c(16, 10))
})

test_that("Atualizacao de modelo Reg Quant", {
    yy <- window(datregdin$obs, 1, 150)
    xx <- head(datregdin$varex, 150)
    mod <- estimamodelo(yy, "reg_quant", regdata = xx, formula = ~ V1 + V2 + V3)

    yy2 <- window(datregdin$obs, 151, 200)
    xx2 <- tail(datregdin$varex, 50)

    mod_upd <- update(mod, yy2, xx2)
    expect_equal(coef(mod$modelo), coef(mod_upd$modelo))
    expect_equal(mod_upd$serie, yy2)

    expect_equal(attr(mod_upd, "mod_atrs")$tsp, c(151, 200, 1))

    # Erro quando nao passa newseries ou newregdata

    expect_error(mod_erro <- update(mod))
    expect_error(mod_erro <- update(mod, newseries = yy2))
    expect_error(mod_erro <- update(mod, newregdata = xx2))

    # Com refit

    mod_refit <- update(mod, yy2, xx2, refit = TRUE)
    expect_equal(mod_refit$serie, yy2)

    expect_snapshot_value(coef(mod_refit$modelo), style = "deparse")

    expect_equal(attr(mod_refit, "mod_atrs")$tsp, c(151, 200, 1))
})