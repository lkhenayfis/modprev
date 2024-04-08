
test_that("Estimacao de modelo Reg Lin - simples", {
    compmod <- lm(as.numeric(datregdin$obs) ~ data.matrix(datregdin$varex))
    mod <- estimamodelo(datregdin$obs, "reg_lin", regdata = datregdin$varex, formula = ~ V1 + V2 + V3)

    expect_equal(class(mod)[1], "reg_lin")
    expect_equal(unname(compmod$coefficients), unname(mod$model$coefficients))

    # Sem passar a formula
    expect_warning(mod2 <- estimamodelo(datregdin$obs, "reg_lin", regdata = datregdin$varex))
    expect_equal(unname(mod2$model$coefficients), unname(mod$model$coefficients))

    # Sem passar regdata
    expect_error(mod2 <- estimamodelo(datregdin$obs, "reg_lin"))

    # Atributo tsp de saida ----------------------------------------------------

    expect_equal(attr(mod, "mod_atrs")$tsp, c(1, 200, 1))

    ss <- ts(datregdin$obs, frequency = 10)
    mod <- estimamodelo(ss, "reg_lin", regdata = datregdin$varex, formula = ~ V1 + V2 + V3)

    expect_equal(attr(mod, "mod_atrs")$tsp, c(1, 20.9, 10))
})

test_that("Estimacao de modelo Reg Lin - argumentos opcionais de lm", {

    # passando pesos
    set.seed(12)
    mod <- estimamodelo(datregdin$obs, "reg_lin", regdata = datregdin$varex, formula = ~ V1 + V2 + V3,
        weights = runif(200))

    set.seed(12)
    compmod <- lm(as.numeric(datregdin$obs) ~ data.matrix(datregdin$varex), weights = runif(200))

    expect_equal(unname(compmod$coefficients), unname(mod$model$coefficients))
    expect_equal(mod$model$weights, compmod$weights)
})

test_that("Previsao de modelo Reg Lin", {
    yy <- window(datregdin$obs, 1, 150)
    xx <- head(datregdin$varex, 150)

    compmod <- lm(Y ~ V1 + V2 + V3, cbind(Y = as.numeric(yy), xx))
    mod <- estimamodelo(yy, "reg_lin", regdata = xx, formula = ~ V1 + V2 + V3)

    prevcomp <- predict(compmod, newdata = datregdin$varex[151:160, ], se.fit = TRUE)
    prev     <- predict(mod, newdata = datregdin$varex[151:160, ])

    expect_equal(as.numeric(prev[, 1]), unname(prevcomp$fit))
    expect_equal(as.numeric(prev[, 2]), unname(prevcomp$se))

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
    mod  <- estimamodelo(ss, "reg_lin", regdata = xx, formula = ~ V1 + V2 + V3)
    prev <- predict(mod, newdata = datregdin$varex[151:160, ])

    expect_equal(start(prev), c(16, 1))
    expect_equal(end(prev), c(16, 10))
})

test_that("Atualizacao de modelo Reg Lin", {
    yy <- window(datregdin$obs, 1, 100)
    xx <- head(datregdin$varex, 100)
    pesos <- runif(100)
    mod <- estimamodelo(yy, "reg_lin", regdata = xx, formula = ~ V1 + V2 + V3, weights = pesos)

    yy2 <- window(datregdin$obs, 101, 200)
    xx2 <- tail(datregdin$varex, 100)

    mod_upd <- update(mod, yy2, xx2)
    expect_equal(coef(mod$modelo), coef(mod_upd$modelo))
    expect_equal(mod$model$weights, mod_upd$model$weights)
    expect_equal(mod_upd$serie, yy2)

    orig_call <- attr(mod, "mod_atrs")$call
    upd_call  <- attr(mod_upd, "mod_atrs")$call
    compare_calls(orig_call, upd_call, c("formula", "weights"))

    expect_equal(attr(mod_upd, "mod_atrs")$tsp, c(101, 200, 1))

    # Erro quando nao passa newseries ou newregdata

    expect_error(mod_erro <- update(mod))
    expect_error(mod_erro <- update(mod, newseries = yy2))
    expect_error(mod_erro <- update(mod, newregdata = xx2))

    # Com refit

    compmod <- estimamodelo(yy2, "reg_lin", regdata = xx2, formula = ~ V1 + V2 + V3, weights = pesos)
    mod_refit <- update(mod, yy2, xx2, refit = TRUE)
    expect_equal(unname(coef(mod_refit$modelo)), unname(coef(compmod$modelo)))
    expect_equal(mod$model$weights, mod_refit$model$weights)
    expect_equal(mod_refit$serie, yy2)

    refit_call  <- attr(mod_refit, "mod_atrs")$call
    compare_calls(orig_call, refit_call, c("formula", "weights"))

    expect_equal(attr(mod_refit, "mod_atrs")$tsp, c(101, 200, 1))
})