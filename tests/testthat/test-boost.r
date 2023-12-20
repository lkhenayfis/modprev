
test_that("Estimacao de BOOSTs - simples", {
    set.seed(12)
    mod <- estimamodelo(datregdin$obs, "BOOST", regdata = datregdin$varex, formula = ~ V1 + V2 + V3,
        cv_control = list(B = 1))

    set.seed(12)
    compmod <- mboost(obs ~ V1 + V2 + V3,
        cbind(obs = as.numeric(datregdin[[1]]), datregdin[[2]]))
    cv_comp <- cvrisk(compmod, cv(model.weights(compmod), B = 1))
    compmod <- compmod[mstop(cv_comp)]

    expect_equal(class(mod)[1], "BOOST")
    expect_equal(coef(compmod), coef(mod$model))
    expect_equal(compmod$mstop(), mod$model$mstop())
    expect_equal(compmod$xselect(), mod$model$xselect())

    # Sem passar regdata
    expect_error(mod2 <- estimamodelo(datregdin$obs, "BOOST"))

    # Atributo tsp de saida ----------------------------------------------------

    expect_equal(attr(mod, "mod_atrs")$tsp, c(1, 200, 1))

    ss <- ts(datregdin$obs, frequency = 10)
    mod <- estimamodelo(ss, "BOOST", regdata = datregdin$varex, formula = ~ V1 + V2 + V3,
        cv_control = list(B = 1))
    expect_equal(attr(mod, "mod_atrs")$tsp, c(1, 20.9, 10))
})

test_that("Estimacao de BOOSTs - argumentos opcionais de mboost", {

    # mudando baselearner
    set.seed(12)
    mod <- estimamodelo(datregdin$obs, "BOOST", regdata = datregdin$varex, formula = ~ V1 + V2 + V3,
        baselearner = "bols", cv_control = list(B = 1))

    set.seed(12)
    compmod <- mboost(obs ~ V1 + V2 + V3,
        cbind(obs = as.numeric(datregdin[[1]]), datregdin[[2]]), baselearner = "bols")
    cv_comp <- cvrisk(compmod, cv(model.weights(compmod), B = 1, type = "boot"))
    compmod <- compmod[mstop(cv_comp)]

    expect_equal(class(mod)[1], "BOOST")
    expect_equal(coef(compmod), coef(mod$model))
    expect_equal(compmod$mstop(), mod$model$mstop())
    expect_equal(compmod$xselect(), mod$model$xselect())

    # mudando funcao perda
    set.seed(12)
    mod <- estimamodelo(datregdin$obs, "BOOST", regdata = datregdin$varex, formula = ~ V1 + V2 + V3,
        family = "Laplace", baselearner = "bols", cv_control = list(B = 1))

    set.seed(12)
    compmod <- mboost(obs ~ V1 + V2 + V3,
        cbind(obs = as.numeric(datregdin[[1]]), datregdin[[2]]), family = Laplace(), baselearner = "bols")
    cv_comp <- cvrisk(compmod, cv(model.weights(compmod), B = 1, type = "boot"))
    compmod <- compmod[mstop(cv_comp)]

    expect_equal(class(mod)[1], "BOOST")
    expect_equal(coef(compmod), coef(mod$model))
    expect_equal(compmod$mstop(), mod$model$mstop())
    expect_equal(compmod$xselect(), mod$model$xselect())
})

test_that("Estimacao de BOOSTs - argumentos de validacao cruzada", {
    set.seed(12)
    mod <- estimamodelo(datregdin$obs, "BOOST", regdata = datregdin$varex, formula = ~ V1 + V2 + V3,
        cv_control = list(B = 4, type = "boot"))

    set.seed(12)
    compmod <- mboost(obs ~ V1 + V2 + V3,
        cbind(obs = as.numeric(datregdin[[1]]), datregdin[[2]]))
    cv_comp <- cvrisk(compmod, cv(model.weights(compmod), B = 4, type = "boot"))
    compmod <- compmod[mstop(cv_comp)]

    expect_equal(class(mod)[1], "BOOST")
    expect_equal(coef(compmod), coef(mod$model))
    expect_equal(compmod$mstop(), mod$model$mstop())
    expect_equal(compmod$xselect(), mod$model$xselect())
})

test_that("Previsao de BOOST", {
    yy <- window(datregdin$obs, 1, 150)
    xx <- head(datregdin$varex, 150)

    # sem cv mesmo pra ficar mais simples aqui
    compmod <- mboost(Y ~ V1 + V2 + V3, data = cbind(Y = as.numeric(yy), xx), baselearner = "bols")

    # essa seed em particular e so pra garantir que mod vai ate 100 iteracoes
    set.seed(1234)
    mod <- estimamodelo(yy, "BOOST", regdata = xx, formula = ~ V1 + V2 + V3,
        baselearner = "bols", cv_control = list(B = 1))

    prevcomp <- predict(compmod, newdata = datregdin$varex[151:160, ])
    prev     <- predict(mod, newdata = datregdin$varex[151:160, ])

    expect_equal(as.numeric(prev[, 1]), as.numeric(prevcomp[, 1]))
    expect_equal(as.numeric(prev[, 2]), rep(NA_real_, 10))

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
    mod <- estimamodelo(ss, "BOOST", regdata = xx, formula = ~ V1 + V2 + V3,
        baselearner = "bols", cv_control = list(B = 1))
    prev <- predict(mod, newdata = datregdin$varex[151:160, ])

    expect_equal(start(prev), c(16, 1))
    expect_equal(end(prev), c(16, 10))
})

test_that("Atualizacao de BOOST", {
    yy <- window(datregdin$obs, 1, 100)
    xx <- head(datregdin$varex, 100)

    set.seed(12)
    mod <- estimamodelo(yy, "BOOST", regdata = xx, formula = ~ V1 + V2 + V3,
        baselearner = "bols", cv_control = list(B = 1))

    yy2 <- window(datregdin$obs, 101, 200)
    xx2 <- tail(datregdin$varex, 100)

    mod_upd <- update(mod, yy2, xx2)
    expect_equal(coef(mod$modelo), coef(mod_upd$modelo))
    expect_equal(mod$modelo$mstop(), mod_upd$modelo$mstop())
    expect_equal(mod$modelo$xselect(), mod_upd$modelo$xselect())
    expect_equal(mod_upd$serie, yy2)

    expect_equal(attr(mod_upd, "mod_atrs")$tsp, c(101, 200, 1))

    # Erro quando nao passa newseries ou newregdata

    expect_error(mod_erro <- update(mod))
    expect_error(mod_erro <- update(mod, newregdata = xx2))

    # Com refit

    mod_refit <- update(mod, yy2, xx2, refit = TRUE)
    expect_equal(mod_refit$serie, yy2)

    expect_equal(attr(mod_refit, "mod_atrs")$tsp, c(101, 200, 1))
})