####################################################################################################
# META-TESTS FOR FIXTURE GENERATORS
####################################################################################################

test_that("make_univariate_series", {
    f <- make_univariate_series
    expect_true(is.function(f))

    test_that("creates valid time series", {
        serie <- f(n = 100, frequency = 12, seed = 123)

        expect_s3_class(serie, "ts")
        expect_equal(length(serie), 100)
        expect_equal(frequency(serie), 12)
    })

    test_that("is deterministic with seed", {
        s1 <- f(n = 50, seed = 42)
        s2 <- f(n = 50, seed = 42)

        expect_equal(s1, s2)
    })

    test_that("trend parameter works", {
        s_trend <- f(n = 100, trend = TRUE, seasonal = FALSE, noise_sd = 0, seed = 1)
        s_no_trend <- f(n = 100, trend = FALSE, seasonal = FALSE, noise_sd = 0, seed = 1)

        expect_true(s_trend[100] > s_trend[1])
        expect_equal(sd(s_no_trend), 0)
    })

    test_that("seasonal parameter works", {
        s_seasonal <- f(n = 100, trend = FALSE, seasonal = TRUE, noise_sd = 0, seed = 1)
        s_no_seasonal <- f(n = 100, trend = FALSE, seasonal = FALSE, noise_sd = 0, seed = 1)

        expect_true(sd(s_seasonal) > 0)
        expect_equal(sd(s_no_seasonal), 0)
    })

    test_that("respects frequency parameter", {
        s12 <- f(frequency = 12, seed = 123)
        s4 <- f(frequency = 4, seed = 456)

        expect_equal(frequency(s12), 12)
        expect_equal(frequency(s4), 4)
    })

    test_that("works with real models", {
        serie <- f(seed = 123)

        expect_error(estimamodelo(serie, "sarima"), NA)
    })
})

test_that("make_seasonal_series", {
    f <- make_seasonal_series
    expect_true(is.function(f))

    test_that("creates valid time series", {
        serie <- f(n = 144, frequency = 12, seed = 123)

        expect_s3_class(serie, "ts")
        expect_equal(length(serie), 144)
        expect_equal(frequency(serie), 12)
    })

    test_that("is deterministic with seed", {
        s1 <- f(seed = 42)
        s2 <- f(seed = 42)

        expect_equal(s1, s2)
    })

    test_that("has strong seasonality", {
        serie <- f(amplitude = 20, noise_sd = 1, seed = 123)

        decomp <- decompose(serie)
        seasonal_var <- var(decomp$seasonal, na.rm = TRUE)
        noise_var <- var(decomp$random, na.rm = TRUE)

        expect_true(seasonal_var > noise_var)
    })

    test_that("works with seasonal models", {
        serie <- f(seed = 123)

        expect_error(estimamodelo(serie, "sarima"), NA)
    })
})

test_that("make_regression_data", {
    f <- make_regression_data
    expect_true(is.function(f))

    test_that("creates valid output structure", {
        data <- f(n = 100, n_predictors = 3, seed = 456)

        expect_type(data, "list")
        expect_named(data, c("serie", "regdata", "coefs"))
    })

    test_that("serie is time series", {
        data <- f(seed = 123)

        expect_s3_class(data$serie, "ts")
        expect_equal(length(data$serie), 100)
    })

    test_that("regdata has correct dimensions", {
        data <- f(n = 100, n_predictors = 3, seed = 123)

        expect_s3_class(data$regdata, "data.frame")
        expect_equal(nrow(data$regdata), 100)
        expect_equal(ncol(data$regdata), 3)
    })

    test_that("regdata has correct column names", {
        data <- f(n = 50, n_predictors = 2, seed = 123)

        expect_equal(names(data$regdata), c("X1", "X2"))
    })

    test_that("coefs have correct length", {
        data <- f(n_predictors = 5, seed = 123)

        expect_length(data$coefs, 5)
    })

    test_that("is deterministic with seed", {
        d1 <- f(seed = 42)
        d2 <- f(seed = 42)

        expect_equal(d1$serie, d2$serie)
        expect_equal(d1$regdata, d2$regdata)
        expect_equal(d1$coefs, d2$coefs)
    })

    test_that("linear relationship is correct with no noise", {
        data <- f(n = 100, n_predictors = 2, coefs = c(1, 2), noise_sd = 0, seed = 789)

        lm_fit <- lm(as.numeric(data$serie) ~ ., data = data$regdata)

        expect_equal(as.numeric(coef(lm_fit)[-1]), data$coefs, tolerance = 1e-10)
    })

    test_that("respects provided coefficients", {
        custom_coefs <- c(1.5, -0.5, 2.0)
        data <- f(n_predictors = 3, coefs = custom_coefs, seed = 123)

        expect_equal(data$coefs, custom_coefs)
    })

    test_that("works with regression models", {
        data <- f(seed = 123)

        expect_warning(
            estimamodelo(data$serie, "reg_lin", regdata = data$regdata),
            "formula"
        )
    })
})

test_that("make_periodic_data", {
    f <- make_periodic_data
    expect_true(is.function(f))

    test_that("creates correct structure", {
        periods <- f(n_periods = 12, len_period = 10, seed = 111)

        expect_type(periods, "list")
        expect_length(periods, 12)
    })

    test_that("each period is time series", {
        periods <- f(n_periods = 5, len_period = 20, seed = 123)

        for (i in seq_along(periods)) {
            expect_s3_class(periods[[i]], "ts")
            expect_equal(length(periods[[i]]), 20)
        }
    })

    test_that("is deterministic with seed", {
        p1 <- f(seed = 42)
        p2 <- f(seed = 42)

        expect_equal(p1, p2)
    })

    test_that("respects base pattern when provided", {
        custom_pattern <- rep(c(10, 20), 5)
        periods <- f(len_period = 10, base_pattern = custom_pattern, noise_sd = 0, seed = 123)

        for (period in periods) {
            expect_equal(mean(abs(period - mean(period))), mean(abs(custom_pattern - mean(custom_pattern))), tolerance = 0.1)
        }
    })
})

test_that("make_missing_data_series", {
    f <- make_missing_data_series
    expect_true(is.function(f))

    test_that("creates time series", {
        serie <- f(n = 100, prop_missing = 0.2, seed = 222)

        expect_s3_class(serie, "ts")
        expect_equal(length(serie), 100)
    })

    test_that("creates NAs in random pattern", {
        serie <- f(n = 100, prop_missing = 0.2, pattern = "random", seed = 222)

        n_missing <- sum(is.na(serie))

        expect_true(n_missing >= 15 && n_missing <= 25)
    })

    test_that("creates contiguous NAs in block pattern", {
        serie <- f(n = 100, prop_missing = 0.1, pattern = "block", seed = 333)

        na_idx <- which(is.na(serie))

        expect_equal(length(na_idx), max(na_idx) - min(na_idx) + 1)
    })

    test_that("creates NAs at end in end pattern", {
        serie <- f(n = 100, prop_missing = 0.1, pattern = "end", seed = 444)

        expect_true(all(is.na(serie[91:100])))
        expect_false(any(is.na(serie[1:90])))
    })

    test_that("returns series without NAs when prop_missing = 0", {
        serie <- f(n = 100, prop_missing = 0, seed = 555)

        expect_false(any(is.na(serie)))
    })

    test_that("is deterministic with seed", {
        s1 <- f(seed = 42)
        s2 <- f(seed = 42)

        expect_equal(is.na(s1), is.na(s2))
        expect_equal(s1[!is.na(s1)], s2[!is.na(s2)])
    })
})

test_that("make_newdata", {
    f <- make_newdata
    expect_true(is.function(f))

    test_that("matches regdata structure", {
        original_data <- make_regression_data(n = 100, n_predictors = 4, seed = 444)
        newdata <- f(n = 12, regdata = original_data$regdata, seed = 555)

        expect_s3_class(newdata, "data.frame")
        expect_equal(nrow(newdata), 12)
        expect_equal(ncol(newdata), ncol(original_data$regdata))
        expect_equal(names(newdata), names(original_data$regdata))
    })

    test_that("is deterministic with seed", {
        regdata <- data.frame(x1 = rnorm(50), x2 = rnorm(50))
        n1 <- f(n = 10, regdata = regdata, seed = 42)
        n2 <- f(n = 10, regdata = regdata, seed = 42)

        expect_equal(n1, n2)
    })

    test_that("handles numeric columns", {
        regdata <- data.frame(x1 = rnorm(100), x2 = rnorm(100))
        newdata <- f(n = 20, regdata = regdata, seed = 123)

        expect_true(is.numeric(newdata$x1))
        expect_true(is.numeric(newdata$x2))
        expect_equal(nrow(newdata), 20)
    })
})
