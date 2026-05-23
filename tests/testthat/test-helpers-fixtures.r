####################################################################################################
# META-TESTS FOR FIXTURE GENERATORS
####################################################################################################

test_that("make_univariate_series creates valid ts", {
    serie <- make_univariate_series(n = 100, frequency = 12, seed = 123)

    expect_s3_class(serie, "ts")
    expect_equal(length(serie), 100)
    expect_equal(frequency(serie), 12)
})

test_that("make_seasonal_series creates valid ts", {
    serie <- make_seasonal_series(n = 144, frequency = 12, seed = 123)

    expect_s3_class(serie, "ts")
    expect_equal(length(serie), 144)
    expect_equal(frequency(serie), 12)
})

test_that("make_regression_data creates valid output structure", {
    data <- make_regression_data(n = 100, n_predictors = 3, seed = 456)

    expect_type(data, "list")
    expect_named(data, c("serie", "regdata", "coefs"))
    expect_s3_class(data$serie, "ts")
    expect_equal(nrow(data$regdata), 100)
    expect_equal(ncol(data$regdata), 3)
})

test_that("make_periodic_data creates correct structure", {
    periods <- make_periodic_data(n_periods = 12, len_period = 10, seed = 111)

    expect_type(periods, "list")
    expect_length(periods, 12)
    expect_s3_class(periods[[1]], "ts")
})

test_that("make_missing_data_series creates valid ts", {
    serie <- make_missing_data_series(n = 100, prop_missing = 0.2, seed = 222)

    expect_s3_class(serie, "ts")
    expect_equal(length(serie), 100)
})

test_that("make_newdata matches regdata structure", {
    original_data <- make_regression_data(n = 100, n_predictors = 4, seed = 444)
    newdata <- make_newdata(n = 12, regdata = original_data$regdata, seed = 555)

    expect_s3_class(newdata, "data.frame")
    expect_equal(nrow(newdata), 12)
    expect_equal(names(newdata), names(original_data$regdata))
})
