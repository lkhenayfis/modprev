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

    # Argumento deprecado -------------------------------------------

    serie <- geraserie(100, 4)
    jm    <- janelamovel(serie, "ss_ar1_saz", 48, 12, 6)
    expect_warning(jm2 <- janelamovel(serie, "ss_ar1_saz", largura = 48, passo = 12, n.ahead = 6))
    expect_true(identical(jm, jm2))
})
