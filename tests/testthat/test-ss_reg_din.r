
test_that("Estimacao de modelo S.S. RegDin - regressao simples", {
    serie <- window(datregdin[[1]], 1, 200)
    varex <- data.frame(venprev = window(datregdin[[2]], 1, 200))
    mod   <- estimamodelo(serie, regdata = varex, tipo = "ss_reg")

    expect_equal("ss_reg_din", class(mod)[1])
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

test_that("Atualizacao de modelo S.S. RegDin - regressao simples", {
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
    expect_equal(round(mod_refit$modelo["Q"][, , 1], 8), 0.03101488)
    expect_equal(round(mod_refit$modelo["H"][, , 1], 8), 0.02001851)
    expect_equal(mod_refit$modelo["Z"][, 2, ], c(varex2[[1]]))
})

# helper para gerar dados de regressao multipla
geradado <- function(n = 200, seed = 1234) {
    set.seed(seed)
    X <- data.frame(V1 = rnorm(n), V2 = rnorm(n, sd = .5), V3 = rnorm(n, sd = 2))
    y <- ts(2 * X$V1 + .7 * X$V2 - 1.1 * X$V3 + .8 * X$V2 * X$V3 + rnorm(n, sd = .25))
    return(list(X, y))
}

test_that("Estimacao de modelo S.S. RegDin - regressao multipla", {

    dados <- geradado()
    serie <- window(dados[[2]], 1, 150)
    varex <- dados[[1]][1:150, ]

    mod <- estimamodelo(serie, "ss_reg", regdata = varex, formula = ~ V1 + V2 * V3)

    expect_equal("ss_reg_din", class(mod)[1])
    expect_snapshot_value(round(mod$modelo["Q"][, , 1], 10), style = "deparse")
    expect_snapshot_value(round(mod$modelo["H"][, , 1], 10), style = "deparse")

    serie <- c(serie)
    mod   <- estimamodelo(serie, formula = ~ V1 + V2 * V3, regdata = varex, tipo = "ss_reg")

    expect_error(estimamodelo(serie, formula = ~ V1 + V2 * V3, tipo = "ss_reg"))

    varex_cna <- varex
    varex_cna[c(10, 20, 30), ] <- NA_real_
    expect_warning(estimamodelo(serie, formula = ~ V1 + V2 * V3, regdata = varex_cna, tipo = "ss_reg"))
})

test_that("Previsao de modelo S.S. RegDin - regressao multipla", {

    dados <- geradado()
    serie <- window(dados[[2]], 1, 150)
    varex <- dados[[1]][1:150, ]

    mod <- estimamodelo(serie, formula = ~ V1 + V2 * V3, regdata = varex, tipo = "ss_reg")

    newdata <- dados[[1]][151:170, ]
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

test_that("Atualizacao de modelo S.S. RegDin - regressao multipla", {

    dados <- geradado()
    serie <- window(dados[[2]], 1, 150)
    varex <- dados[[1]][1:150, ]

    mod <- estimamodelo(serie, formula = ~ V1 + V2 * V3, regdata = varex, tipo = "ss_reg")

    newseries  <- window(dados[[2]], 151, 200)
    newregdata <- dados[[1]][151:200, ]
    mod_upd   <- update(mod, newseries = newseries, newregdata = newregdata)

    expect_equal(c(mod$modelo["Q"]), c(mod_upd$modelo["Q"]))
    expect_equal(c(mod$modelo["H"]), c(mod_upd$modelo["H"]))
    expect_equal(c(mod$modelo["T"]), c(mod_upd$modelo["T"]))
    expect_true(all(mod_upd$modelo["Z"][, 2:4, ] - t(data.matrix(newregdata)) == 0))
    expect_true(all(c(mod_upd$modelo$y) - newseries == 0))

    mod_refit <- update(mod, newseries = newseries, newregdata = newregdata, refit = TRUE)

    expect_true(all(mod_upd$modelo["Z"][, 2:4, ] - t(data.matrix(newregdata)) == 0))
    expect_true(all(c(mod_upd$modelo$y) - newseries == 0))
    expect_snapshot_value(round(mod$modelo["Q"][, , 1], 10), style = "deparse")
    expect_snapshot_value(round(mod$modelo["H"][, , 1], 10), style = "deparse")
})

test_that("Estimacao de modelo S.S. RegDin - regressao multipla heterocedastica", {

    dados <- geradado()

    # Serie temporal com sazonalidade especificada e vardin = TRUE

    serie <- window(dados[[2]], 1, 150)
    serie <- ts(c(serie), freq = 10)
    varex <- dados[[1]][1:150, ]

    mod1 <- estimamodelo(serie, "ss_reg", regdata = varex, formula = ~ V1 + V2 * V3, vardin = TRUE)

    expect_equal("ss_reg_din", class(mod1)[1])
    expect_equal(attr(mod1$model, "vardin"), 1)
    matH <- matrix(c(mod1$model["H"]), 10)
    expect_true(all(apply(matH, 1, function(v) all(v == v[1]))))
    expect_snapshot_value(round(mod1$modelo["Q"][, , 1], 10), style = "deparse")
    expect_snapshot_value(round(mod1$modelo["H"][1, 1, 1:10], 10), style = "deparse")

    # Serie temporal sem sazonalidade com vardin = 10

    serie <- c(serie)
    expect_warning(
        mod2 <- estimamodelo(serie, "ss_reg", regdata = varex, formula = ~ V1 + V2 * V3, vardin = 10)
    )

    expect_equal("ss_reg_din", class(mod2)[1])
    expect_equal(attr(mod2$model, "vardin"), 10)
    expect_equal(c(mod2$model["H"]), c(mod1$model["H"]))
    expect_equal(c(mod2$model["Q"]), c(mod1$model["Q"]))
})
