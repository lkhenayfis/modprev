test_that("Expansoes de janelas", {

    ss <- ts(seq(40), freq = 10)

    # JANELAS MOVEIS ------------------------------------------------

    testa_janelamovel <- function(janela, passo) {

        jj <- expandejanelas(ss, janela, passo)

        sj <- lapply(jj, function(j) window(ss, j[[1]], j[[2]]))
        expect_true(all(sapply(sj, length) == janela))

        diffs <- diff(sapply(sj[-1], "[[", 1))
        expect_true(all(diffs == passo))
    }

    testa_janelamovel(20, 1)
    testa_janelamovel(20, 2)
    testa_janelamovel(20, 10)

    # teste de passos tal que nem todas as janelas adjacentes tem o mesmo deslocamento

    jj <- expandejanelas(ss, 13, 7)
    sj <- lapply(jj, function(j) window(ss, j[[1]], j[[2]]))
    expect_true(all(sapply(sj, length) == 13))

    diffs <- diff(sapply(sj[-1], "[[", 1))
    expect_true(all(diffs[-length(diffs)] == diffs[1]))
    expect_true(tail(diffs, 1) != diffs[1])

    # JANELAS EXPANSIVEIS -------------------------------------------

    testa_janelaexpand <- function(janela, passo) {

        jj <- expandejanelas(ss, janela, passo)

        sj <- lapply(jj, function(j) window(ss, j[[1]], j[[2]]))
        expect_true(length(sj[[1]]) == janela[2])
        expect_true(all(diff(sapply(sj, length)) == passo))

        diffs <- diff(sapply(sj[-1], "[[", 1))
        expect_true(all(diffs == 0))

        expect_true(tail(tail(sj, 1)[[1]], 1) == tail(ss, 1))
    }

    testa_janelaexpand(c(1, 20), 1)
    testa_janelaexpand(c(1, 20), 2)
    testa_janelaexpand(c(1, 20), 10)

    testa_janelaexpand(c(1, 30), 1)
    testa_janelaexpand(c(1, 30), 2)
    testa_janelaexpand(c(1, 30), 10)

    # teste de passos tal que nem todas as janelas adjacentes tem o mesmo deslocamento

    testa_janelaexpand(c(2, 20), 1)

    jj <- expandejanelas(ss, c(4, 20), 2)
    sj <- lapply(jj, function(j) window(ss, j[[1]], j[[2]]))

    difflen <- diff(sapply(sj, length))
    expect_true(all(difflen[-length(difflen)] == difflen[1]))
    expect_true(tail(difflen, 1) != difflen[1])

    diffs <- diff(sapply(sj[-1], "[[", 1))
    expect_true(all(diffs == 0))

    expect_true(tail(tail(sj, 1)[[1]], 1) == tail(ss, 1))
})

test_that("Vetor de reajustes", {

    ss <- ts(seq(40), freq = 10)

    jj <- expandejanelas(ss, 20, 1)

    rr <- expanderefit(jj, 1)
    expect_equal(length(rr), length(jj))
    expect_true(!rr[1])
    expect_true(all(rr[-1]))

    rr <- expanderefit(jj, NA)
    expect_equal(length(rr), length(jj))
    expect_true(all(!rr))

    expect_error(rr <- expanderefit(jj, c(1, 4, 6)))
})

geraserie <- function(n, f, seed = 1234) {
    set.seed(seed)
    ar1 <- arima.sim(list(ar = .8), n)
    saz <- sin(seq(0, f - 1) * 2 * pi / f)

    ts(ar1 + 2.5 * saz, freq = f)
}

# helper para gerar dados de regressao multipla
geradado <- function(n = 200, seed = 1234) {
    set.seed(seed)
    X <- data.frame(V1 = rnorm(n), V2 = rnorm(n, sd = .5), V3 = rnorm(n, sd = 2))
    y <- ts(2 * X$V1 + .7 * X$V2 - 1.1 * X$V3 + .8 * X$V2 * X$V3 + rnorm(n, sd = .25))
    return(list(X, y))
}

test_that("Testes de previsao em janela", {

    # Modelo sem variavel explicativa -------------------------------

    serie <- geraserie(100, 4)

    jm <- janelamovel(serie, "sarima", 48, 12, 6)

    expect_equal(length(jm), 6)
    expect_true(all(sapply(jm, function(m) all(dim(m) == c(6, 2)))))

    # checa se as janelas comecam onde deveriam
    serie1 <- window(serie, c(1, 1), c(12, 4))
    for(i in seq(jm)[-length(jm)]) {
        expect_equal(start(jm[[i]]), deltats(end(serie1), 12 * (i - 1) + 1, 4))
    }
    expect_equal(start(tail(jm, 1)[[1]]), deltats(end(serie), 1, 4))

    # teste simples com ss_ar1_saz, nao precisa repetir os inicios de janela
    jm <- janelamovel(serie, "ss_ar1_saz", 48, 12, 6)

    expect_equal(length(jm), 6)
    expect_true(all(sapply(jm, function(m) all(dim(m) == c(6, 2)))))

    # Modelo regressao dinamica -------------------------------------

    dados <- geradado()
    ss <- window(dados[[2]], 1, 195)
    regd <- dados[[1]]

    jm <- janelamovel(ss, "ss_reg_din", 150, passo = 5, n.ahead = 5, regdata = regd,
        formula = ~ V1)

    expect_equal(length(jm), 10)
    expect_true(all(sapply(jm, function(m) all(dim(m) == c(5, 2)))))

    # Checa se as previsoes estao usando variaveis explicativas corretamente
    mod_ref <- estimamodelo(window(dados[[2]], 1, 150), "ss_reg_din", regdata = regd[1:150, ],
        formula = ~ V1)

    prev_ref1 <- predict(mod_ref, newdata = regd[151:155, ])
    expect_equal(prev_ref1, jm[[1]])

    mod_ref2 <- update(mod_ref, newseries = window(dados[[2]], 6, 155), newregdata = regd[6:155, ])
    prev_ref2 <- predict(mod_ref2, newdata = regd[156:160, ])
    expect_equal(prev_ref2, jm[[2]])

    mod_ref3 <- update(mod_ref, newseries = window(dados[[2]], 11, 160), newregdata = regd[11:160, ])
    prev_ref3 <- predict(mod_ref3, newdata = regd[161:165, ])
    expect_equal(prev_ref3, jm[[3]])

    # Argumento deprecado -------------------------------------------

    serie <- geraserie(100, 4)
    jm    <- janelamovel(serie, "ss_ar1_saz", 48, 12, 6)
    expect_warning(jm2 <- janelamovel(serie, "ss_ar1_saz", largura = 48, passo = 12, n.ahead = 6))
    expect_true(identical(jm, jm2))

    # Full Output ---------------------------------------------------

    # Caso com variavel explicativa
    jm <- janelamovel(ss, "ss_reg_din", 150, passo = 5, n.ahead = 5, regdata = regd, formula = ~ V1,
        full.output = TRUE)
    outs <- sapply(jm, function(l) {
        length(l) &
            ("ts" %in% class(l[[1]])) & ("modprev" %in% class(l[[2]])) & ("data.frame" %in% class(l[[3]]))
    })
    expect_true(all(outs))

    # Caso sem variavel explicativa
    jm <- janelamovel(serie, "ss_ar1_saz", 48, 12, 6, full.output = TRUE)
    outs <- sapply(jm, function(l) {
        length(l) &
            ("ts" %in% class(l[[1]])) & ("modprev" %in% class(l[[2]])) & (is.null(l[[3]]))
    })
    expect_true(all(outs))
})
