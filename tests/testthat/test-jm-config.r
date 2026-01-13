test_that("jm_config", {
    f <- jm_config
    expect_true(is.function(f))

    test_that("jm_config creates valid configuration with minimal arguments", {
        config <- f(janela = 60)

        expect_s3_class(config, "jm_config")
        expect_equal(config$janela, 60)
        expect_equal(config$passo, 1L)
        expect_equal(config$n.ahead, 1L)
        expect_true(is.na(config$refit.cada))
        expect_equal(config$verbose, 0L)
        expect_false(config$full.output)
    })

    test_that("jm_config creates valid configuration with all arguments", {
        config <- f(janela = c(1, 50), passo = 6, n.ahead = 12, refit.cada = 6, verbose = 2,
            full.output = TRUE)

        expect_s3_class(config, "jm_config")
        expect_equal(config$janela, c(1, 50))
        expect_equal(config$passo, 6L)
        expect_equal(config$n.ahead, 12L)
        expect_equal(config$refit.cada, 6L)
        expect_equal(config$verbose, 2L)
        expect_true(config$full.output)
    })

    test_that("jm_config handles rolling window", {
        config <- f(janela = 100)

        expect_equal(length(config$janela), 1)
        expect_equal(config$janela, 100)
    })

    test_that("jm_config handles expanding window", {
        config <- f(janela = c(1, 60))

        expect_equal(length(config$janela), 2)
        expect_equal(config$janela[1], 1)
        expect_equal(config$janela[2], 60)
    })

    test_that("jm_config coerces numeric to integer", {
        config <- f(janela = 60, passo = 5.0, n.ahead = 12.0, refit.cada = 3.0, verbose = 1.0)

        expect_type(config$passo, "integer")
        expect_type(config$n.ahead, "integer")
        expect_type(config$refit.cada, "integer")
        expect_type(config$verbose, "integer")
    })

    test_that("jm_config validates janela is positive", {
        expect_error(f(janela = 0), "'janela' must be positive")
        expect_error(f(janela = -10), "'janela' must be positive")
        expect_error(f(janela = c(1, -5)), "'janela' must be positive")
    })

    test_that("jm_config validates janela length", {
        expect_error(f(janela = c(1, 2, 3)), "'janela' must be scalar or length-2")
    })

    test_that("jm_config validates passo is positive integer", {
        expect_error(f(janela = 60, passo = 0), "'passo' must be a positive integer")
        expect_error(f(janela = 60, passo = -5), "'passo' must be a positive integer")
        expect_error(f(janela = 60, passo = c(1, 2)), "'passo' must be a positive integer")
    })

    test_that("jm_config validates n.ahead is positive integer", {
        expect_error(f(janela = 60, n.ahead = 0), "'n.ahead' must be a positive integer")
        expect_error(f(janela = 60, n.ahead = -5), "'n.ahead' must be a positive integer")
        expect_error(f(janela = 60, n.ahead = c(1, 2)), "'n.ahead' must be a positive integer")
    })

    test_that("jm_config validates refit.cada is NA or positive integer", {
        config <- f(janela = 60, refit.cada = NA)
        expect_true(is.na(config$refit.cada))

        config <- f(janela = 60, refit.cada = 5)
        expect_equal(config$refit.cada, 5L)

        expect_error(f(janela = 60, refit.cada = 0), "'refit.cada' must be NA or a positive")
        expect_error(f(janela = 60, refit.cada = -5), "'refit.cada' must be NA or a positive")
        expect_error(f(janela = 60, refit.cada = c(1, 2)),
            "'refit.cada' must be NA or a positive")
    })

    test_that("jm_config validates verbose is 0, 1, or 2", {
        expect_silent(f(janela = 60, verbose = 0))
        expect_silent(f(janela = 60, verbose = 1))
        expect_silent(f(janela = 60, verbose = 2))

        expect_error(f(janela = 60, verbose = 3), "'verbose' must be 0, 1, or 2")
        expect_error(f(janela = 60, verbose = -1), "'verbose' must be 0, 1, or 2")
        expect_error(f(janela = 60, verbose = c(0, 1)), "'verbose' must be 0, 1, or 2")
    })

    test_that("jm_config validates full.output is logical", {
        config <- f(janela = 60, full.output = TRUE)
        expect_true(config$full.output)

        config <- f(janela = 60, full.output = FALSE)
        expect_false(config$full.output)

        expect_error(f(janela = 60, full.output = 1), "'full.output' must be a logical")
        expect_error(f(janela = 60, full.output = "TRUE"), "'full.output' must be a logical")
        expect_error(f(janela = 60, full.output = c(TRUE, FALSE)),
            "'full.output' must be a logical")
    })
})

test_that("new_jm_config", {
    f <- modprev:::new_jm_config
    expect_true(is.function(f))

    test_that("new_jm_config creates unvalidated object", {
        config <- f(janela = 60, passo = 1L, n.ahead = 1L, refit.cada = NA, verbose = 0L,
            full.output = FALSE)

        expect_s3_class(config, "jm_config")
        expect_equal(config$janela, 60)
        expect_equal(config$passo, 1L)
    })

    test_that("new_jm_config accepts any values", {
        config <- f(janela = -10, passo = 0L, n.ahead = 0L, refit.cada = NA, verbose = 5L,
            full.output = "not logical")

        expect_s3_class(config, "jm_config")
    })
})

test_that("validate_jm_config", {
    f <- modprev:::validate_jm_config
    expect_true(is.function(f))

    test_that("validate_jm_config rejects non-jm_config objects", {
        expect_error(f(list()), "'config' must be a jm_config object")
        expect_error(f(data.frame()), "'config' must be a jm_config object")
    })

    test_that("validate_jm_config accepts valid config", {
        config <- jm_config(janela = 60)

        expect_silent(f(config))
        result <- f(config)
        expect_identical(result, config)
    })

    test_that("validate_jm_config rejects invalid janela", {
        config <- modprev:::new_jm_config(janela = 0, passo = 1L, n.ahead = 1L, refit.cada = NA,
            verbose = 0L, full.output = FALSE)
        expect_error(f(config), "'janela' must be positive")

        config <- modprev:::new_jm_config(janela = c(1, 2, 3), passo = 1L, n.ahead = 1L,
            refit.cada = NA, verbose = 0L, full.output = FALSE)
        expect_error(f(config), "'janela' must be scalar or length-2")
    })

    test_that("validate_jm_config rejects invalid passo", {
        config <- modprev:::new_jm_config(janela = 60, passo = 0L, n.ahead = 1L, refit.cada = NA,
            verbose = 0L, full.output = FALSE)
        expect_error(f(config), "'passo' must be a positive integer")
    })

    test_that("validate_jm_config rejects invalid n.ahead", {
        config <- modprev:::new_jm_config(janela = 60, passo = 1L, n.ahead = -1L, refit.cada = NA,
            verbose = 0L, full.output = FALSE)
        expect_error(f(config), "'n.ahead' must be a positive integer")
    })

    test_that("validate_jm_config rejects invalid verbose", {
        config <- modprev:::new_jm_config(janela = 60, passo = 1L, n.ahead = 1L, refit.cada = NA,
            verbose = 5L, full.output = FALSE)
        expect_error(f(config), "'verbose' must be 0, 1, or 2")
    })

    test_that("validate_jm_config rejects invalid full.output", {
        config <- modprev:::new_jm_config(janela = 60, passo = 1L, n.ahead = 1L, refit.cada = NA,
            verbose = 0L, full.output = "not logical")
        expect_error(f(config), "'full.output' must be a logical")
    })
})

test_that("print.jm_config", {
    f <- print.jm_config
    expect_true(is.function(f))

    test_that("print.jm_config returns invisibly", {
        config <- jm_config(janela = 60)

        result <- withVisible(f(config))

        expect_false(result$visible)
        expect_identical(result$value, config)
    })

    test_that("print.jm_config displays rolling window configuration", {
        config <- jm_config(janela = 60, passo = 6, n.ahead = 12)

        output <- capture.output(f(config))

        expect_true(any(grepl("janelamovel Configuration", output)))
        expect_true(any(grepl("Rolling", output)))
        expect_true(any(grepl("Window Size.*60", output)))
        expect_true(any(grepl("Step Size.*6", output)))
        expect_true(any(grepl("Forecast Horizon.*12", output)))
    })

    test_that("print.jm_config displays expanding window configuration", {
        config <- jm_config(janela = c(1, 50), passo = 6, n.ahead = 12)

        output <- capture.output(f(config))

        expect_true(any(grepl("Expanding", output)))
        expect_true(any(grepl("Start Position.*1", output)))
        expect_true(any(grepl("Initial Width.*50", output)))
    })

    test_that("print.jm_config displays refit strategy", {
        config1 <- jm_config(janela = 60, refit.cada = NA)
        output1 <- capture.output(f(config1))
        expect_true(any(grepl("Fit once, update thereafter", output1)))

        config2 <- jm_config(janela = 60, refit.cada = 6)
        output2 <- capture.output(f(config2))
        expect_true(any(grepl("Refit Interval.*6", output2)))
    })

    test_that("print.jm_config displays verbosity levels", {
        config0 <- jm_config(janela = 60, verbose = 0)
        output0 <- capture.output(f(config0))
        expect_true(any(grepl("Silent", output0)))

        config1 <- jm_config(janela = 60, verbose = 1)
        output1 <- capture.output(f(config1))
        expect_true(any(grepl("Refit events only", output1)))

        config2 <- jm_config(janela = 60, verbose = 2)
        output2 <- capture.output(f(config2))
        expect_true(any(grepl("All windows", output2)))
    })

    test_that("print.jm_config displays full.output flag", {
        config_false <- jm_config(janela = 60, full.output = FALSE)
        output_false <- capture.output(f(config_false))
        expect_true(any(grepl("Full Output.*FALSE", output_false)))

        config_true <- jm_config(janela = 60, full.output = TRUE)
        output_true <- capture.output(f(config_true))
        expect_true(any(grepl("Full Output.*TRUE", output_true)))
    })
})
