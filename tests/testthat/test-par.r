
test_that("simulate.PAR returns n.ahead x nsim ts", {
    serie <- ts(10 + 5 * sin(2 * pi * (1:240) / 12) + rnorm(240), frequency = 12, start = c(2000, 1))
    mod <- estimamodelo(serie, tipo = "PAR")

    sims <- simulate(mod, nsim = 20, n.ahead = 12)

    expect_equal(dim(sims), c(12, 20))
    expect_equal(colnames(sims), paste0("sim_", 1:20))

    expect_equal(start(sims), c(2020, 1))
    expect_equal(frequency(sims), 12)

    sims1 <- simulate(mod, nsim = 5, n.ahead = 6, seed = 42)
    sims2 <- simulate(mod, nsim = 5, n.ahead = 6, seed = 42)
    expect_equal(sims1, sims2)

    sims_distinct <- simulate(mod, nsim = 4, n.ahead = 6, seed = 1)
    expect_false(identical(sims_distinct[, 1], sims_distinct[, 2]))
    expect_equal(length(unique(apply(sims_distinct, 2, paste, collapse = ","))), 4)

    sims_onehead <- simulate(mod, nsim = 3, n.ahead = 1, seed = 1)
    expect_equal(dim(sims_onehead), c(1, 3))
})

test_that("simulate.PAR_A returns n.ahead x nsim ts", {
    serie <- ts(10 + 5 * sin(2 * pi * (1:240) / 12) + rnorm(240), frequency = 12, start = c(2000, 1))
    mod <- estimamodelo(serie, tipo = "PAR_A")

    sims <- simulate(mod, nsim = 20, n.ahead = 12)

    expect_equal(dim(sims), c(12, 20))
    expect_equal(colnames(sims), paste0("sim_", 1:20))

    expect_equal(start(sims), c(2020, 1))
    expect_equal(frequency(sims), 12)

    sims1 <- simulate(mod, nsim = 5, n.ahead = 6, seed = 42)
    sims2 <- simulate(mod, nsim = 5, n.ahead = 6, seed = 42)
    expect_equal(sims1, sims2)

    sims_distinct <- simulate(mod, nsim = 4, n.ahead = 6, seed = 1)
    expect_false(identical(sims_distinct[, 1], sims_distinct[, 2]))
    expect_equal(length(unique(apply(sims_distinct, 2, paste, collapse = ","))), 4)

    sims_onehead <- simulate(mod, nsim = 3, n.ahead = 1, seed = 1)
    expect_equal(dim(sims_onehead), c(1, 3))
})
