
# helper para gerar dados de regressao multipla
geradado_pm <- function(n = 250, f = 5, seed = 1234) {
    set.seed(seed)
    X <- data.frame(V1 = rnorm(n), V2 = rnorm(n, sd = .5), V3 = rnorm(n, sd = 2))
    y <- ts(2 * X$V1 + .7 * X$V2 - 1.1 * X$V3 + .8 * X$V2 * X$V3 + rnorm(n, sd = .25), frequency = f)
    return(list(X, y))
}

test_that("Estimacao de modelo S.S. RegDin Pseudo Multivar - regressao simples", {
    dados <- geradado_pm()

    varex <- dados[[1]]
    serie <- dados[[2]]
    mod   <- estimamodelo(serie, regdata = varex, tipo = "ss_reg_din_pm", formula = ~ V1)

    expect_equal("ss_reg_din_pm", class(mod)[1])
    expect_snapshot_value(round(mod$modelo["Q"][1, 1, 1], 5), style = "deparse")
    expect_snapshot_value(round(mod$modelo["H"][1, 1, 1], 5), style = "deparse")

    # sem passar regdata
    expect_error(estimamodelo(serie, tipo = "ss_reg_din_pm"))

    # sem passar formula
    expect_warning(estimamodelo(serie, tipo = "ss_reg_din_pm", regdata = varex))

    # NAs nos regressores
    varex2 <- dados[[1]]
    varex2[c(1, 10, 20), ] <- NA_real_
    expect_warning(estimamodelo(serie, regdata = varex2, tipo = "ss_reg_din_pm", formula = ~ V1))

    # serie sem sazonalidade
    expect_error(estimamodelo(c(serie), regdata = varex, tipo = "ss_reg_din_pm", formula = ~ V1))

    # serie sem numero inteiro de periodos
    expect_error(estimamodelo(window(serie, c(1, 1), c(4, 3)), "ss_reg_din_pm", regdata = varex, formula = ~ V1))

    # Teste de argumentos extras que nao existe na funcao

    varex <- varex[, "V1", drop = FALSE]
    mod2 <- estimamodelo(serie, regdata = varex, tipo = "ss_reg_din_pm", formula = ~ V1, erro = 1)
    expect_equal(mod$modelo["H"], mod2$modelo["H"])
    expect_equal(mod$modelo["Q"], mod2$modelo["Q"])
    expect_equal(mod$modelo["T"], mod2$modelo["T"])
    expect_equal(mod$modelo["Z"], mod2$modelo["Z"])
})

test_that("Previsao de modelo S.S. RegDin Pseudo Multivar - regressao simples", {
    dados <- geradado_pm()
    serie <- window(dados[[2]], c(1, 1), c(20, 5))
    varex <- dados[[1]][1:100, "V1", drop = FALSE]
    mod   <- estimamodelo(serie, regdata = varex, tipo = "ss_reg_din_pm", formula = ~ V1)

    newdata <- dados[[1]][101:120, "V1", drop = FALSE]
    prev    <- predict(mod, newdata = newdata)

    expect_true(all(dim(prev) == c(20, 2)))
    expect_equal(c("prev", "sd"), colnames(prev))
    expect_equal(frequency(prev), frequency(serie))
    expect_equal(tsp(prev)[1], tsp(serie)[2] + 1 / tsp(serie)[3])

    expect_snapshot_value(round(c(prev), 5), style = "deparse")

    # n.ahead maior que nrow(regdata)

    prev_maior <- predict(mod, newdata = newdata, n.ahead = 100)
    expect_equal(prev, prev_maior)

    # previsao "1 passo a frente" (no modo multivariado)

    prev <- predict(mod, newdata = newdata, n.ahead = 5)

    expect_true(all(dim(prev) == c(5, 2)))
    expect_equal(c("prev", "sd"), colnames(prev))
    expect_equal(frequency(prev), frequency(serie))
    expect_equal(tsp(prev)[1], tsp(serie)[2] + 1 / tsp(serie)[3])

    expect_snapshot_value(round(c(prev), 5), style = "deparse")

    # previsao de um horionte "quebrado" -- sem numero inteiro de periodos

    prev_qbr1 <- predict(mod, newdata = newdata, n.ahead = 13)
    expect_equal(dim(prev_qbr1), c(13, 2))
    expect_equal(c(prev_qbr1), c(prev[seq_len(13), ]))

    prev_qbr2 <- predict(mod, newdata = newdata[seq_len(13), , drop = FALSE])
    expect_equal(prev_qbr1, prev_qbr2)

    prev_qbr3 <- predict(mod, newdata = newdata, n.ahead = 3)
    expect_equal(dim(prev_qbr3), c(3, 2))
    expect_equal(c(prev_qbr3), c(prev[seq_len(3), ]))

    # sem passar regdata

    expect_error(predict(mod))
})

test_that("Atualizacao de modelo S.S. RegDin Pseudo Multivar - regressao simples", {

    dados <- geradado_pm()

    serie1 <- window(dados[[2]], c(1, 1), c(20, 5))
    varex1 <- dados[[1]][1:100, "V1", drop = FALSE]
    serie2 <- window(dados[[2]], c(21, 1), c(40, 5))
    varex2 <- dados[[1]][101:200, "V1", drop = FALSE]

    mod <- estimamodelo(serie1, tipo = "ss_reg_din_pm", regdata = varex1, formula = ~ V1)

    mod_upd <- update(mod, serie2, newregdata = varex2)
    expect_equal(c(mod$modelo["Q"]), c(mod_upd$modelo["Q"]))
    expect_equal(c(mod$modelo["H"]), c(mod_upd$modelo["H"]))
    expect_equal(c(mod$modelo["T"]), c(mod_upd$modelo["T"]))
    expect_equal(c(mod_upd$modelo["Z"][, 2, ]), c(varex2[[1]]))
    expect_true(all(mod_upd$serie - serie2 == 0))

    mod_atr     <- attr(mod, "mod_atrs")
    mod_atr_upd <- attr(mod_upd, "mod_atrs")

    expect_equal(mod_atr_upd$formula, mod_atr$formula)
    expect_equal(mod_atr_upd$vardin, mod_atr$vardin)

    # passando dados ruins
    expect_error(update(mod, c(serie2), newregdata = varex2))
    expect_error(update(mod, window(serie2, c(21, 1), c(40, 3)), newregdata = varex2))

    mod_refit <- update(mod, serie2, newregdata = varex2, refit = TRUE)
    expect_equal(c(t(mod_refit$modelo$y)), c(serie2))
    expect_snapshot_value(round(mod_refit$modelo["Q"][, , 1], 5), style = "deparse")
    expect_snapshot_value(round(mod_refit$modelo["H"][, , 1], 5), style = "deparse")
    expect_equal(c(mod_refit$modelo["Z"][, 2, ]), c(varex2[[1]]))

    expect_equal(mod_atr_upd$formula, mod_atr$formula)
    expect_equal(mod_atr_upd$vardin, mod_atr$vardin)
})

test_that("Estimacao de modelo S.S. RegDin Pseudo Multivar - regressao multipla", {

    dados <- geradado_pm()

    varex <- dados[[1]][1:150, ]
    serie <- window(dados[[2]], c(1, 1), c(30, 5))

    mod <- estimamodelo(serie, "ss_reg_din_pm", regdata = varex, formula = ~ V1 + V2 * V3)

    expect_equal("ss_reg_din_pm", class(mod)[1])
    expect_snapshot_value(round(mod$modelo["Q"][, , 1], 5), style = "deparse")
    expect_snapshot_value(round(mod$modelo["H"][, , 1], 5), style = "deparse")

    expect_error(estimamodelo(c(serie), formula = ~ V1 + V2 * V3, regdata = varex, tipo = "ss_reg_din_pm"))
    expect_error(estimamodelo(serie, formula = ~ V1 + V2 * V3, tipo = "ss_reg_din_pm"))

    varex_cna <- varex
    varex_cna[c(10, 20, 30), ] <- NA_real_
    expect_warning(estimamodelo(serie, formula = ~ V1 + V2 * V3, regdata = varex_cna, tipo = "ss_reg_din_pm"))
})

test_that("Previsao de modelo S.S. RegDin Pseudo Multivar - regressao multipla", {

    dados <- geradado_pm()

    varex <- dados[[1]][1:150, ]
    serie <- window(dados[[2]], c(1, 1), c(30, 5))

    mod <- estimamodelo(serie, formula = ~ V1 + V2 * V3, regdata = varex, tipo = "ss_reg_din_pm")

    newdata <- datregdin$varex[151:170, ]
    prev    <- predict(mod, newdata = newdata)

    expect_true(all(dim(prev) == c(20, 2)))
    expect_equal(c("prev", "sd"), colnames(prev))

    expect_snapshot_value(round(c(prev), 5), style = "deparse")

    prev <- predict(mod, newdata = newdata, n.ahead = 10)

    expect_true(all(dim(prev) == c(10, 2)))
    expect_equal(c("prev", "sd"), colnames(prev))

    expect_snapshot_value(round(c(prev), 5), style = "deparse")

    expect_error(predict(mod))
})

test_that("Atualizacao de modelo S.S. RegDin Pseudo Multivar - regressao multipla", {

    dados <- geradado_pm()

    varex <- dados[[1]][1:150, ]
    serie <- window(dados[[2]], c(1, 1), c(30, 5))

    mod <- estimamodelo(serie, formula = ~ V1 + V2 * V3, regdata = varex, tipo = "ss_reg_din_pm")

    newseries  <- window(dados[[2]], c(31, 1), c(40, 5))
    newregdata <- datregdin$varex[151:200, ]
    mod_upd   <- update(mod, newseries = newseries, newregdata = newregdata)

    expect_equal(c(mod$modelo["Q"]), c(mod_upd$modelo["Q"]))
    expect_equal(c(mod$modelo["H"]), c(mod_upd$modelo["H"]))
    expect_equal(c(mod$modelo["T"][, , 1]), c(mod_upd$modelo["T"][, , 1]))
    expect_equal(c(mod_upd$modelo["Z"][, 2:4, 2]), c(data.matrix(newregdata)[6:10, ]))
    expect_equal(mod_upd$serie, newseries)

    mod_atr     <- attr(mod, "mod_atrs")
    mod_atr_upd <- attr(mod_upd, "mod_atrs")

    expect_equal(mod_atr_upd$formula, mod_atr$formula)
    expect_equal(mod_atr_upd$vardin, mod_atr$vardin)

    mod_refit <- update(mod, newseries = newseries, newregdata = newregdata, refit = TRUE)

    expect_equal(c(mod_refit$modelo["Z"][, 2:4, 2]), c(data.matrix(newregdata)[6:10, ]))
    expect_true(all(mod_refit$serie - newseries == 0))
    expect_snapshot_value(round(mod$modelo["Q"][, , 1], 5), style = "deparse")
    expect_snapshot_value(round(mod$modelo["H"][, , 1], 5), style = "deparse")

    mod_atr_refit <- attr(mod_upd, "mod_atrs")

    expect_equal(mod_atr_refit$formula, mod_atr$formula)
    expect_equal(mod_atr_refit$vardin, mod_atr$vardin)
})

test_that("Estimacao de modelo S.S. RegDin - regressao multipla heterocedastica", {

    # Serie temporal com sazonalidade especificada e vardin = TRUE

    dados <- geradado_pm(f = 10)

    varex <- dados[[1]][1:150, ]
    serie <- window(dados[[2]], c(1, 1), c(15, 10))

    mod1 <- estimamodelo(serie, "ss_reg_din_pm", regdata = varex, formula = ~ V1 + V2 * V3, vardin = TRUE)

    expect_equal("ss_reg_din_pm", class(mod1)[1])
    expect_equal(attr(mod1, "mod_atrs")$vardin, TRUE)
    expect_snapshot_value(round(mod1$modelo["Q"][, , 1], 5), style = "deparse")
    expect_snapshot_value(round(mod1$modelo["H"][, , 1], 5), style = "deparse")
})

test_that("Previsao de modelo S.S. RegDin - regressao multipla - heterocedastica", {

    dados <- geradado_pm(f = 10)

    varex <- dados[[1]][1:150, ]
    serie <- window(dados[[2]], c(1, 1), c(15, 10))

    mod <- estimamodelo(serie, "ss_reg_din_pm", formula = ~ V1 + V2 * V3, regdata = varex, vardin = TRUE)

    newdata <- datregdin$varex[151:170, ]
    prev    <- predict(mod, newdata = newdata)

    expect_true(all(dim(prev) == c(20, 2)))
    expect_equal(c("prev", "sd"), colnames(prev))

    expect_snapshot_value(round(c(prev), 5), style = "deparse")

    prev <- predict(mod, newdata = newdata, n.ahead = 10)

    expect_true(all(dim(prev) == c(10, 2)))
    expect_equal(c("prev", "sd"), colnames(prev))

    expect_snapshot_value(round(c(prev), 5), style = "deparse")

    expect_error(predict(mod))
})

test_that("Atualizacao de modelo S.S. RegDin - regressao multipla heterocedastica", {

    dados <- geradado_pm()

    serie1 <- window(dados[[2]], c(1, 1), c(20, 5))
    varex1 <- dados[[1]][1:100, ]
    serie2 <- window(dados[[2]], c(21, 1), c(40, 5))
    varex2 <- dados[[1]][101:200, ]

    mod <- estimamodelo(serie1, "ss_reg_din_pm", formula = ~ V1 + V2 * V3, regdata = varex1, vardin = TRUE)
    mod_refit <- update(mod, serie2, newregdata = varex2, refit = TRUE)

    expect_true(!any(duplicated(diag(mod_refit$modelo["H"][, , 1]))))
})

test_that("Geracao de matrizes do sistema", {

    dados <- geradado_pm()
    varex <- dados[[1]]
    formula <- ~ V1 + V2 + V3
    serie_m <- ts(matrix(dados[[2]], 50, 5))

    mats <- expande_sist_mats(serie_m, varex, formula)

    # matriz Z
    expect_equal(dim(mats$Z), c(5, 4, 50))
    expect_true(all((data.matrix(cbind(1, head(varex, 5))) - mats$Z[, , 1]) == 0))

    # matriz T
    expect_equal(dim(mats$T), c(4, 4, 50))
    alldiag <- lapply(seq_len(dim(mats$T)[3]), function(k) mats$T[, , k] == diag(4))
    expect_true(all(Reduce("&", alldiag)))

    # matriz R
    expect_equal(dim(mats$R), c(4, 3, 1))
    expect_equal(mats$R[1, , 1], rep(0, 3))
    expect_equal(mats$R[-1, , 1], diag(3))

    # matriz Q
    expect_equal(dim(mats$Q), c(3, 3, 1))
    expect_equal(mats$Q[, , 1], diag(NA_real_, 3))

    # matriz H
    expect_equal(dim(mats$H), c(5, 5, 1))
    expect_equal(mats$H[, , 1], diag(NA_real_, 5))
})

test_that("Conversao univar <-> multivar", {

    # iniciando em c(1, 1)

    s1 <- ts(seq_len(50), frequency = 5)

    s1_m <- univar2multivar(s1)
    expect_equal(dim(s1_m), c(10, 5))
    expect_equal(frequency(s1_m), 1)
    expect_equal(start(s1_m), c(1, 1))
    expect_equal(end(s1_m), c(10, 1))

    s1_u <- multivar2univar(s1_m)
    expect_equal(s1_u, s1)

    # iniciando em c(4, 1)

    s2 <- ts(seq_len(50), frequency = 5, start = c(4, 1))

    s2_m <- univar2multivar(s2)
    expect_equal(dim(s2_m), c(10, 5))
    expect_equal(frequency(s2_m), 1)
    expect_equal(start(s2_m), c(4, 1))
    expect_equal(end(s2_m), c(13, 1))

    s2_u <- multivar2univar(s2_m)
    expect_equal(s2_u, s2)
})