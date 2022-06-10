
digits <- switch(Sys.info()[["sysname"]],
    "Windows" = 5,
    "Linux" = 5,
    "Darwin" = 2
)

test_that("Estimacao de modelo S.S. RegDin - regressao simples", {
    serie <- window(datregdin$obs, 1, 200)
    varex <- datregdin$varex[, "V1", drop = FALSE]
    mod   <- estimamodelo(serie, regdata = varex, tipo = "ss_reg_din", formula = ~ V1)

    expect_equal("ss_reg_din", class(mod)[1])
    expect_snapshot_value(round(mod$modelo["Q"][1, 1, 1], digits), style = "deparse")
    expect_snapshot_value(round(mod$modelo["H"][1, 1, 1], digits), style = "deparse")

    serie <- c(serie)
    mod   <- estimamodelo(serie, regdata = varex, tipo = "ss_reg_din", formula = ~ V1)

    # sem passar regdata
    expect_error(estimamodelo(serie, tipo = "ss_reg_din"))

    # sem passar formula
    expect_warning(estimamodelo(serie, tipo = "ss_reg_din", regdata = varex))

    # NAs nos regressores
    varex[c(1, 10, 20), ] <- NA_real_
    expect_warning(estimamodelo(serie, regdata = varex, tipo = "ss_reg_din", formula = ~ V1))

    # Teste de argumentos extras que nao existe na funcao

    varex <- datregdin$varex[, "V1", drop = FALSE]
    mod2 <- estimamodelo(c(serie), regdata = varex, tipo = "ss_reg_din", formula = ~ V1, erro = 1)
    expect_equal(mod$modelo["H"], mod2$modelo["H"])
    expect_equal(mod$modelo["Q"], mod2$modelo["Q"])
    expect_equal(mod$modelo["T"], mod2$modelo["T"])
    expect_equal(mod$modelo["Z"], mod2$modelo["Z"])
})

test_that("Previsao de modelo S.S. RegDin - regressao simples", {
    serie <- window(datregdin$obs, 1, 100)
    varex <- datregdin$varex[1:100, "V1", drop = FALSE]
    mod   <- estimamodelo(serie, regdata = varex, tipo = "ss_reg_din", formula = ~ V1)

    newdata <- datregdin$varex[101:120, "V1", drop = FALSE]
    prev    <- predict(mod, newdata = newdata)

    expect_true(all(dim(prev) == c(20, 2)))
    expect_equal(c("prev", "sd"), colnames(prev))

    expect_snapshot_value(round(c(prev), digits), style = "deparse")

    prev <- predict(mod, newdata = newdata, n.ahead = 10)

    expect_true(all(dim(prev) == c(10, 2)))
    expect_equal(c("prev", "sd"), colnames(prev))

    expect_snapshot_value(round(c(prev), digits), style = "deparse")

    expect_error(predict(mod))
})

test_that("Atualizacao de modelo S.S. RegDin - regressao simples", {
    serie1 <- window(datregdin$obs, 1, 100)
    varex1 <- datregdin$varex[1:100, "V1", drop = FALSE]
    serie2 <- window(datregdin$obs, 101, 200)
    varex2 <- datregdin$varex[101:200, "V1", drop = FALSE]

    mod <- estimamodelo(serie1, tipo = "ss_reg_din", regdata = varex1, formula = ~ V1)

    mod_upd <- update(mod, serie2, newregdata = varex2)
    expect_equal(c(mod$modelo["Q"]), c(mod_upd$modelo["Q"]))
    expect_equal(c(mod$modelo["H"]), c(mod_upd$modelo["H"]))
    expect_equal(c(mod$modelo["T"]), c(mod_upd$modelo["T"]))
    expect_equal(mod_upd$modelo["Z"][, 2, ], c(varex2[[1]]))
    expect_true(all(mod_upd$serie - serie2 == 0))

    expect_equal(attr(mod_upd$model, "formula"), attr(mod$model, "formula"))
    expect_equal(attr(mod_upd$model, "vardin"), attr(mod$model, "vardin"))
    expect_equal(attr(mod_upd$model, "saz"), attr(mod$model, "saz"))

    mod_refit <- update(mod, serie2, newregdata = varex2, refit = TRUE)
    expect_equal(c(mod_refit$modelo$y), c(serie2))
    expect_snapshot_value(round(mod_refit$modelo["Q"][, , 1], digits), style = "deparse")
    expect_snapshot_value(round(mod_refit$modelo["H"][, , 1], digits), style = "deparse")
    expect_equal(mod_refit$modelo["Z"][, 2, ], c(varex2[[1]]))

    expect_equal(attr(mod_upd$model, "formula"), attr(mod$model, "formula"))
    expect_equal(attr(mod_upd$model, "vardin"), attr(mod$model, "vardin"))
    expect_equal(attr(mod_upd$model, "saz"), attr(mod$model, "saz"))
})

test_that("Estimacao de modelo S.S. RegDin - regressao multipla", {

    serie <- window(datregdin$obs, 1, 150)
    varex <- datregdin$varex[1:150, ]

    mod <- estimamodelo(serie, "ss_reg_din", regdata = varex, formula = ~ V1 + V2 * V3)

    expect_equal("ss_reg_din", class(mod)[1])
    expect_snapshot_value(round(mod$modelo["Q"][, , 1], digits), style = "deparse")
    expect_snapshot_value(round(mod$modelo["H"][, , 1], digits), style = "deparse")

    serie <- c(serie)
    mod   <- estimamodelo(serie, formula = ~ V1 + V2 * V3, regdata = varex, tipo = "ss_reg_din")

    expect_error(estimamodelo(serie, formula = ~ V1 + V2 * V3, tipo = "ss_reg_din"))

    varex_cna <- varex
    varex_cna[c(10, 20, 30), ] <- NA_real_
    expect_warning(estimamodelo(serie, formula = ~ V1 + V2 * V3, regdata = varex_cna, tipo = "ss_reg_din"))
})

test_that("Previsao de modelo S.S. RegDin - regressao multipla", {

    serie <- window(datregdin$obs, 1, 150)
    varex <- datregdin$varex[1:150, ]

    mod <- estimamodelo(serie, formula = ~ V1 + V2 * V3, regdata = varex, tipo = "ss_reg_din")

    newdata <- datregdin$varex[151:170, ]
    prev    <- predict(mod, newdata = newdata)

    expect_true(all(dim(prev) == c(20, 2)))
    expect_equal(c("prev", "sd"), colnames(prev))

    expect_snapshot_value(round(c(prev), digits), style = "deparse")

    prev <- predict(mod, newdata = newdata, n.ahead = 10)

    expect_true(all(dim(prev) == c(10, 2)))
    expect_equal(c("prev", "sd"), colnames(prev))

    expect_snapshot_value(round(c(prev), digits), style = "deparse")

    expect_error(predict(mod))
})

test_that("Atualizacao de modelo S.S. RegDin - regressao multipla", {

    serie <- window(datregdin$obs, 1, 150)
    varex <- datregdin$varex[1:150, ]

    mod <- estimamodelo(serie, formula = ~ V1 + V2 * V3, regdata = varex, tipo = "ss_reg_din")

    newseries  <- window(datregdin$obs, 151, 200)
    newregdata <- datregdin$varex[151:200, ]
    mod_upd   <- update(mod, newseries = newseries, newregdata = newregdata)

    expect_equal(c(mod$modelo["Q"]), c(mod_upd$modelo["Q"]))
    expect_equal(c(mod$modelo["H"]), c(mod_upd$modelo["H"]))
    expect_equal(c(mod$modelo["T"]), c(mod_upd$modelo["T"]))
    expect_true(all(mod_upd$modelo["Z"][, 2:4, ] - t(data.matrix(newregdata)) == 0))
    expect_true(all(mod_upd$serie - newseries == 0))

    expect_equal(attr(mod_upd$model, "formula"), attr(mod$model, "formula"))
    expect_equal(attr(mod_upd$model, "vardin"), attr(mod$model, "vardin"))
    expect_equal(attr(mod_upd$model, "saz"), attr(mod$model, "saz"))

    mod_refit <- update(mod, newseries = newseries, newregdata = newregdata, refit = TRUE)

    expect_true(all(mod_upd$modelo["Z"][, 2:4, ] - t(data.matrix(newregdata)) == 0))
    expect_true(all(mod_upd$serie - newseries == 0))
    expect_snapshot_value(round(mod$modelo["Q"][, , 1], digits), style = "deparse")
    expect_snapshot_value(round(mod$modelo["H"][, , 1], digits), style = "deparse")

    expect_equal(attr(mod_upd$model, "formula"), attr(mod$model, "formula"))
    expect_equal(attr(mod_upd$model, "vardin"), attr(mod$model, "vardin"))
    expect_equal(attr(mod_upd$model, "saz"), attr(mod$model, "saz"))
})

test_that("Estimacao de modelo S.S. RegDin - regressao multipla heterocedastica", {


    # Serie temporal com sazonalidade especificada e vardin = TRUE

    serie <- window(datregdin$obs, 1, 150)
    serie <- ts(c(serie), freq = 10)
    varex <- datregdin$varex[1:150, ]

    mod1 <- estimamodelo(serie, "ss_reg_din", regdata = varex, formula = ~ V1 + V2 * V3, vardin = TRUE)

    expect_equal("ss_reg_din", class(mod1)[1])
    expect_equal(attr(mod1$model, "vardin"), 1)
    matH <- matrix(c(mod1$model["H"]), 10)
    expect_true(all(apply(matH, 1, function(v) all(v == v[1]))))
    expect_snapshot_value(round(mod1$modelo["Q"][, , 1], digits), style = "deparse")
    expect_snapshot_value(round(mod1$modelo["H"][1, 1, 1:10], digits), style = "deparse")

    # Serie temporal sem sazonalidade com vardin = 10

    serie <- c(serie)
    expect_warning(
        mod2 <- estimamodelo(serie, "ss_reg_din", regdata = varex, formula = ~ V1 + V2 * V3, vardin = 10)
    )

    expect_equal("ss_reg_din", class(mod2)[1])
    expect_equal(attr(mod2$model, "vardin"), 10)
    expect_equal(c(mod2$model["H"]), c(mod1$model["H"]))
    expect_equal(c(mod2$model["Q"]), c(mod1$model["Q"]))
})

test_that("Previsao de modelo S.S. RegDin - regressao multipla - heterocedastica", {

    serie <- window(datregdin$obs, 1, 150)
    serie <- ts(serie, freq = 10)
    varex <- datregdin$varex[1:150, ]

    mod <- estimamodelo(serie, "ss_reg_din", formula = ~ V1 + V2 * V3, regdata = varex, vardin = TRUE)

    newdata <- datregdin$varex[151:170, ]
    prev    <- predict(mod, newdata = newdata)

    expect_true(all(dim(prev) == c(20, 2)))
    expect_equal(c("prev", "sd"), colnames(prev))

    expect_snapshot_value(round(c(prev), digits), style = "deparse")

    prev <- predict(mod, newdata = newdata, n.ahead = 10)

    expect_true(all(dim(prev) == c(10, 2)))
    expect_equal(c("prev", "sd"), colnames(prev))

    expect_snapshot_value(round(c(prev), digits), style = "deparse")

    expect_error(predict(mod))
})

test_that("Atualizacao de modelo S.S. RegDin - regressao multipla heterocedastica", {

    # os testes aqui se focam mais no encadeamento correto do array de variancias H
    # a determinacao do tipo de deslocamento, dependnendo dos atributos de serie e newseries, fica
    # concentrado nos testes de desloc espceficicamente

    serie <- window(datregdin$obs, 1, 100)
    serie <- ts(serie, start = c(1, 3), freq = 10)
    varex <- datregdin$varex[1:100, ]

    mod <- estimamodelo(serie, "ss_reg_din", formula = ~ V1 + V2 * V3, regdata = varex, vardin = TRUE)

    newregdata <- datregdin$varex[1:20, ]

    newseries  <- ts(seq(20), start = c(2, 4), freq = 10)
    mod_upd    <- update(mod, newseries = newseries, newregdata = newregdata)
    desloc     <- parsedesloc(serie, newseries, attr(mod_upd$modelo, "saz"))
    expect_equal(mod_upd$modelo["H"][1, 1, 1:10], shift(mod$modelo["H"][1, 1, 1:10], desloc))

    newseries  <- ts(seq(20), start = c(2, 10), freq = 10)
    mod_upd    <- update(mod, newseries = newseries, newregdata = newregdata)
    desloc     <- parsedesloc(serie, newseries, attr(mod_upd$modelo, "saz"))
    expect_equal(mod_upd$modelo["H"][1, 1, 1:10], shift(mod$modelo["H"][1, 1, 1:10], desloc))

    newseries  <- ts(seq(20), start = c(2, 2), freq = 10)
    mod_upd    <- update(mod, newseries = newseries, newregdata = newregdata)
    desloc     <- parsedesloc(serie, newseries, attr(mod_upd$modelo, "saz"))
    expect_equal(mod_upd$modelo["H"][1, 1, 1:10], shift(mod$modelo["H"][1, 1, 1:10], desloc))

    # Atualizacao quando a serie e sazonal mas vardin = FALSE

    mod <- estimamodelo(serie, "ss_reg_din", formula = ~ V1 + V2 * V3, regdata = varex, vardin = FALSE)

    newseries  <- ts(seq(20), start = c(2, 4), freq = 10)
    newregdata <- datregdin$varex[1:20, ]
    mod_upd    <- update(mod, newseries = newseries, newregdata = newregdata)
    desloc     <- parsedesloc(serie, newseries, attr(mod$model, "saz"))
    expect_equal(desloc, 0)
})

test_that("Identificacao de deslocamento de array", {

    # Quando ambas as series tem sazonalidade especificada

    s1 <- ts(seq(100), start = c(1, 4), freq = 10)

    s2 <- ts(seq(100), start = c(11, 7), freq = 10)
    expect_equal(parsedesloc(s1, s2, 10), -3)

    s3 <- ts(seq(100), start = c(11, 1), freq = 10)
    expect_equal(parsedesloc(s1, s3, 10), 3)

    s4 <- ts(seq(100), start = c(11, 5), freq = 10)
    expect_equal(parsedesloc(s1, s4, 10), -1)

    s5 <- ts(seq(100), start = c(11, 3), freq = 10)
    expect_equal(parsedesloc(s1, s5, 10), 1)

    # serie original sem sazonalidade especificada

    s1 <- seq(100)

    s2 <- ts(seq(100), start = c(11, 7), freq = 10)
    expect_warning(expect_equal(parsedesloc(s1, s2, 10), -6))

    s3 <- ts(seq(100), start = c(11, 1), freq = 10)
    expect_warning(expect_equal(parsedesloc(s1, s3, 10), 0))

    s4 <- ts(seq(100), start = c(11, 5), freq = 10)
    expect_warning(expect_equal(parsedesloc(s1, s4, 10), -4))

    s5 <- ts(seq(100), start = c(11, 3), freq = 10)
    expect_warning(expect_equal(parsedesloc(s1, s5, 10), -2))

    # newseries sem sazonalidade especificada

    s1 <- ts(seq(100), start = c(1, 4), freq = 10)
    s2 <- seq(100)
    expect_warning(expect_equal(parsedesloc(s1, s2, 10), 0))

    s1 <- ts(seq(101), start = c(1, 4), freq = 10)
    s2 <- seq(100)
    expect_warning(expect_equal(parsedesloc(s1, s2, 10), -1))

    s1 <- ts(seq(99), start = c(1, 4), freq = 10)
    s2 <- seq(100)
    expect_warning(expect_equal(parsedesloc(s1, s2, 10), 1))

    # nenhuma das duas tem sazonalidade especificada

    s2 <- seq(99)

    s1 <- seq(100)
    expect_warning(expect_warning(expect_equal(parsedesloc(s1, s2, 10), 0)))

    s1 <- seq(101)
    expect_warning(expect_warning(expect_equal(parsedesloc(s1, s2, 10), -1)))

    s1 <- seq(99)
    expect_warning(expect_warning(expect_equal(parsedesloc(s1, s2, 10), -9)))
})
