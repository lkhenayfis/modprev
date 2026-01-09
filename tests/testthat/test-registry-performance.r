skip_on_cran()

measure_time <- function(expr, n_reps = 50) {
    for (i in 1:5) eval(expr)

    times <- replicate(n_reps, {
        system.time(eval(expr))[["elapsed"]]
    })

    list(
        median = median(times),
        mean = mean(times),
        max = max(times),
        min = min(times)
    )
}

test_that("get_model performance", {
    test_that("get_model lookup is fast", {
        if (!is_registered("sarima")) {
            modprev:::.onLoad(NULL, NULL)
        }

        timing <- measure_time(quote(get_model("sarima")))

        expect_lt(timing$median, 0.001,
            label = sprintf("Median: %.3f ms", timing$median * 1000))
    })
})

test_that("is_registered performance", {
    test_that("is_registered lookup is fast", {
        if (!is_registered("sarima")) {
            modprev:::.onLoad(NULL, NULL)
        }

        timing <- measure_time(quote(is_registered("sarima")))

        expect_lt(timing$median, 0.0005,
            label = sprintf("Median: %.3f ms", timing$median * 1000))
    })
})

test_that("list_models performance", {
    test_that("list_models is fast", {
        if (!is_registered("sarima")) {
            modprev:::.onLoad(NULL, NULL)
        }

        timing <- measure_time(quote(list_models()))

        expect_lt(timing$median, 0.005,
            label = sprintf("Median: %.3f ms", timing$median * 1000))
    })

    test_that("list_models with details is acceptably fast", {
        if (!is_registered("sarima")) {
            modprev:::.onLoad(NULL, NULL)
        }

        timing <- measure_time(quote(list_models(details = TRUE)))

        expect_lt(timing$median, 0.020,
            label = sprintf("Median: %.3f ms", timing$median * 1000))
    })
})

test_that("registry overhead on estimamodelo", {
    test_that("registry lookup adds minimal overhead", {
        if (!is_registered("sarima")) {
            modprev:::.onLoad(NULL, NULL)
        }

        timing_with_registry <- measure_time(quote({
            spec <- get_model("sarima")
            spec$tipo
        }), n_reps = 100)

        timing_direct <- measure_time(quote({
            "sarima"
        }), n_reps = 100)

        overhead <- timing_with_registry$median - timing_direct$median

        expect_lt(overhead, 0.001,
            label = sprintf("Overhead: %.3f ms", overhead * 1000))
    })
})
