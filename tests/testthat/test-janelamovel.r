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