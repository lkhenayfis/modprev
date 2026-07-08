test_that("simulate.modprevU errors for unsupported model", {
    serie <- ts(1:100)
    regdata <- data.frame(x = 1:100)

    mod <- estimamodelo(serie, "reg_lin", regdata = regdata)

    expect_error(
        simulate(mod, nsim = 10, n.ahead = 5),
        "reg_lin nao possui metodo 'simulate'"
    )
})

test_that("simulate.modprevS rejects simulation", {
    mod <- structure(
        list(modelo = NULL, serie = NULL),
        class = c("modprevS", "sarima", "modprevU", "modprev")
    )

    expect_error(
        simulate(mod, nsim = 10, n.ahead = 5),
        "modprevS nao possui metodo 'simulate'"
    )
})
