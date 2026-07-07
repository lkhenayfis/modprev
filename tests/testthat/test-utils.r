test_that("Shift de vetores", {
    vv <- seq(10)
    expect_equal(shift(vv, 0), vv)
    expect_equal(shift(vv, 1), c(10, 1:9))
    expect_equal(shift(vv, 2), c(9:10, 1:8))
    expect_equal(shift(vv, -1), c(2, 3:10, 1))
    expect_equal(shift(vv, -2), c(3, 4:10, 1:2))
})

test_that("Intervalos de tempo em sistema ts", {

    tt <- c(3, 2)

    expect_equal(deltats(tt, 0, 12), tt)

    expect_equal(deltats(tt, 1, 12), c(3, 3))
    expect_equal(deltats(tt, 2, 12), c(3, 4))
    expect_equal(deltats(tt, 10, 12), c(3, 12))

    expect_equal(deltats(tt, 11, 12), c(4, 1))
    expect_equal(deltats(tt, 12, 12), c(4, 2))
    expect_equal(deltats(tt, 20, 12), c(4, 10))

    expect_equal(deltats(tt, 30, 12), c(5, 8))

    expect_equal(deltats(tt, -1, 12), c(3, 1))
    expect_equal(deltats(tt, -2, 12), c(2, 12))
    expect_equal(deltats(tt, -10, 12), c(2, 4))

    expect_equal(deltats(tt, -20, 12), c(1, 6))
})

test_that("Expansao de formula", {
    dd1 <- data.frame(a = 1, b = 2, c = 3)
    ff1 <- expandeformula(dd1)
    expect_equal(as.character(ff1), c("~", "a + b + c"))

    dd1 <- data.frame(a = 1, b = 2, c = 3)
    expect_warning(ff1 <- expandeformula(dd1, warn = TRUE))
    expect_equal(as.character(ff1), c("~", "a + b + c"))

    dd2 <- data.frame(V = 1, V4 = 2, BB = 3, CAD = 0, f1j = 10)
    ff2 <- expandeformula(dd2)
    expect_equal(as.character(ff2), c("~", "V + V4 + BB + CAD + f1j"))
})

test_that("Split sazonal -- estrutura e nomes", {
    serie <- ts(seq_len(20), frequency = 4)
    result <- split_seasonal(serie)

    expect_equal(names(result), c("series", "seasons"))

    expect_equal(length(result$series), 4)
    expect_equal(names(result$series), c("1", "2", "3", "4"))
    expect_true(all(vapply(result$series, length, integer(1)) == 5))

    expect_true(is.factor(result$seasons))
    expect_equal(levels(result$seasons), c("1", "2", "3", "4"))
})

test_that("Split sazonal -- invariante tsp das parcelas", {
    serie <- ts(seq_len(20), frequency = 4)
    result <- split_seasonal(serie)

    expect_equal(tsp(result$series[["1"]]), c(1, 5, 1))
    expect_equal(tsp(result$series[["2"]]), c(1.25, 5.25, 1))
    expect_equal(tsp(result$series[["3"]]), c(1.5, 5.5, 1))
    expect_equal(tsp(result$series[["4"]]), c(1.75, 5.75, 1))

    vals_by_season <- lapply(result$series, as.numeric)
    idx_by_season <- split(seq_along(serie), result$seasons)
    reconstructed <- numeric(length(serie))
    for (s in names(idx_by_season)) reconstructed[idx_by_season[[s]]] <- vals_by_season[[s]]
    expect_equal(reconstructed, as.numeric(serie))
})

test_that("Split sazonal -- inicio em estacao > 1 preserva ordem", {
    serie <- window(ts(seq_len(20), frequency = 4), start = c(1, 3))
    result <- split_seasonal(serie)

    expect_equal(names(result$series), c("3", "4", "1", "2"))
    expect_equal(levels(result$seasons), c("3", "4", "1", "2"))
})

test_that("Split sazonal regdata -- shape e nomes", {
    serie <- ts(seq_len(20), frequency = 4)
    seasons <- factor(as.numeric(cycle(serie)), unique(as.numeric(cycle(serie))))
    rd <- data.frame(a = seq_len(20), b = seq(21, 40))

    result <- split_seasonal_regdata(rd, seasons)

    expect_equal(length(result), 4)
    expect_equal(names(result), c("1", "2", "3", "4"))
    expect_true(all(vapply(result, nrow, integer(1)) == 5))
})

test_that("sim_ts shapes simulation output", {
    serie <- ts(seq_len(24), frequency = 12)
    mm <- matrix(seq_len(15), nrow = 5, ncol = 3)

    result <- sim_ts(mm, serie)

    expect_equal(dim(result), c(5, 3))
    expect_equal(colnames(result), c("sim_1", "sim_2", "sim_3"))
    expect_equal(start(result), c(3, 1))
    expect_equal(frequency(result), 12)

    vv <- seq_len(5)
    result_vec <- sim_ts(vv, serie)

    expect_equal(ncol(result_vec), 1)
    expect_equal(colnames(result_vec), "sim_1")
})