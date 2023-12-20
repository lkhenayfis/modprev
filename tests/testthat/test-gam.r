
test_that("Estimacao de modelo GAM - simples", {
    compmod <- with(datregdin,
        gam(as.numeric(obs) ~ s(varex$V1, varex$V2, varex$V3))
    )
    mod <- estimamodelo(datregdin$obs, "GAM", regdata = datregdin$varex, formula = ~ s(V1, V2, V3))

    expect_equal(class(mod)[1], "GAM")
    expect_equal(unname(compmod$coefficients), unname(mod$model$coefficients))
    expect_equal(attr(mod, "mod_atrs")$formula, Y ~ s(V1, V2, V3))

    # Sem passar a formula
    expect_warning(mod2 <- estimamodelo(datregdin$obs, "GAM", regdata = datregdin$varex))
    expect_equal(unname(mod2$model$coefficients), unname(mod$model$coefficients))

    # Formula com coisas mais complicadas
    SP <- 3
    K  <- 15
    mod <- estimamodelo(datregdin$obs, "GAM", regdata = datregdin$varex,
        formula = ~ s(V1, V2, V3, sp = SP, k = K))
    expect_equal(unname(mod$model$smooth[[1]]$sp), SP)
    expect_equal(unname(mod$model$smooth[[1]]$bs.dim), K)

    # Sem passar regdata
    expect_error(mod2 <- estimamodelo(datregdin$obs, "GAM"))

    # Atributo tsp de saida ----------------------------------------------------

    expect_equal(attr(mod, "mod_atrs")$tsp, c(1, 200, 1))

    ss <- ts(datregdin$obs, frequency = 10)
    mod <- estimamodelo(ss, "GAM", regdata = datregdin$varex, formula = ~ s(V1, V2, V3))

    expect_equal(attr(mod, "mod_atrs")$tsp, c(1, 20.9, 10))
})

test_that("Estimacao de modelo GAM - funcao 'bam'", {
    compmod <- with(datregdin,
        bam(as.numeric(obs) ~ s(varex$V1, varex$V2, varex$V3))
    )
    mod <- estimamodelo(datregdin$obs, "GAM", regdata = datregdin$varex, formula = ~ s(V1, V2, V3),
        fit_fun = "bam")

    expect_equal(class(mod)[1], "GAM")
    expect_equal(unname(compmod$coefficients), unname(mod$model$coefficients))
    expect_equal(attr(mod, "mod_atrs")$formula, Y ~ s(V1, V2, V3))

    # Sem passar a formula
    expect_warning(
        mod2 <- estimamodelo(datregdin$obs, "GAM", regdata = datregdin$varex, fit_fun = "bam")
    )
    expect_equal(unname(mod2$model$coefficients), unname(mod$model$coefficients))

    # Formula com coisas mais complicadas
    SP <- 3
    K  <- 15
    mod <- estimamodelo(datregdin$obs, "GAM", regdata = datregdin$varex,
        formula = ~ s(V1, V2, V3, sp = SP, k = K), fit_fun = "bam")
    expect_equal(unname(mod$model$smooth[[1]]$sp), SP)
    expect_equal(unname(mod$model$smooth[[1]]$bs.dim), K)

    # Sem passar regdata
    expect_error(mod2 <- estimamodelo(datregdin$obs, "GAM", fit_fun = "bam"))

    # Atributo tsp de saida ----------------------------------------------------

    expect_equal(attr(mod, "mod_atrs")$tsp, c(1, 200, 1))

    ss <- ts(datregdin$obs, frequency = 10)
    mod <- estimamodelo(ss, "GAM", regdata = datregdin$varex, formula = ~ s(V1, V2, V3),
        fit_fun = "bam")

    expect_equal(attr(mod, "mod_atrs")$tsp, c(1, 20.9, 10))
})

test_that("Estimacao de modelo GAM - argumentos opcionais de gam", {

    # passando pesos
    set.seed(12)
    mod <- estimamodelo(datregdin$obs, "GAM", regdata = datregdin$varex, formula = ~ s(V1, V2, V3),
        weights = runif(200))

    set.seed(12)
    compmod <- with(datregdin,
        gam(as.numeric(obs) ~ s(varex$V1, varex$V2, varex$V3), weights = runif(200))
    )

    expect_equal(unname(compmod$coefficients), unname(mod$model$coefficients))
    expect_equal(attr(mod, "mod_atrs")$formula, Y ~ s(V1, V2, V3))
    expect_equal(mod$model$weights, compmod$weights)
})

test_that("Previsao de modelo GAM", {
    yy <- window(datregdin$obs, 1, 150)
    xx <- head(datregdin$varex, 150)

    compmod <- gam(Y ~ s(V1, V2, V3), data = cbind(Y = as.numeric(yy), xx))
    mod <- estimamodelo(yy, "GAM", regdata = xx, formula = ~ s(V1, V2, V3))

    prevcomp <- predict(compmod, newdata = datregdin$varex[151:160, ], se.fit = TRUE)
    prev     <- predict(mod, newdata = datregdin$varex[151:160, ])

    expect_equal(as.numeric(prev[, 1]), as.numeric(unname(prevcomp$fit)))
    expect_equal(as.numeric(prev[, 2]), as.numeric(unname(prevcomp$se)))

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
    mod  <- estimamodelo(ss, "GAM", regdata = xx, formula = ~ s(V1, V2, V3))
    prev <- predict(mod, newdata = datregdin$varex[151:160, ])

    expect_equal(start(prev), c(16, 1))
    expect_equal(end(prev), c(16, 10))
})

test_that("Atualizacao de modelo GAM", {
    yy <- window(datregdin$obs, 1, 100)
    xx <- head(datregdin$varex, 100)
    mod <- estimamodelo(yy, "GAM", regdata = xx, formula = ~ s(V1, V2, V3, k = 15))

    yy2 <- window(datregdin$obs, 101, 200)
    xx2 <- tail(datregdin$varex, 100)

    mod_upd <- update(mod, yy2, xx2)
    expect_equal(coef(mod$modelo), coef(mod_upd$modelo))
    expect_equal(mod_upd$serie, yy2)

    expect_equal(attr(mod_upd, "mod_atrs")$tsp, c(101, 200, 1))

    # Erro quando nao passa newseries ou newregdata

    expect_error(mod_erro <- update(mod))
    # ver comentario em R/mod-gam.r a respeito do problema com NAs nos regressores
    #expect_error(mod_erro <- update(mod, newseries = yy2))
    expect_error(mod_erro <- update(mod, newregdata = xx2))

    # Com refit

    mod_refit <- update(mod, yy2, xx2, refit = TRUE)
    expect_equal(mod_refit$serie, yy2)

    expect_equal(attr(mod_refit, "mod_atrs")$tsp, c(101, 200, 1))
})