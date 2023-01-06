
# helper para gerar dados de regressao multipla
geradado <- function(n = 250, f = 5, seed = 1234) {
    set.seed(seed)
    X <- data.frame(V1 = rnorm(n), V2 = rnorm(n, sd = .5), V3 = rnorm(n, sd = 2))
    y <- ts(2 * X$V1 + .7 * X$V2 - 1.1 * X$V3 + .8 * X$V2 * X$V3 + rnorm(n, sd = .25), frequency = f)
    return(list(X, y))
}

test_that("Estimacao de modelo S.S. RegDin Pseudo Multivar - regressao simples", {
    dados <- geradado()

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

test_that("Geracao de matrizes do sistema", {

    dados <- geradado()
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