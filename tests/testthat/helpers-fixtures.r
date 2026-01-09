####################################################################################################
# FIXTURE GENERATORS FOR MODPREV TESTING
####################################################################################################

#' Generate Univariate Time Series for Testing
#'
#' Creates a deterministic time series with optional trend, seasonality,
#' and noise for testing univariate models.
#'
#' @param n Number of observations
#' @param frequency Time series frequency
#' @param trend Logical; add linear trend?
#' @param seasonal Logical; add seasonal component?
#' @param noise_sd Standard deviation of random noise
#' @param seed Random seed for reproducibility (NULL = no seed)
#' @param start Start time (passed to ts())
#'
#' @return Time series object
#'
#' @keywords internal

make_univariate_series <- function(
    n = 100,
    frequency = 12,
    trend = TRUE,
    seasonal = TRUE,
    noise_sd = 1,
    seed = NULL,
    start = c(2000, 1)
) {
    if (!is.null(seed)) set.seed(seed)

    t <- seq_len(n)
    y <- numeric(n)

    if (trend) {
        y <- y + 0.1 * t
    }

    if (seasonal && frequency > 1) {
        y <- y + 10 * sin(2 * pi * t / frequency)
    }

    y <- y + rnorm(n, mean = 0, sd = noise_sd)

    ts(y, start = start, frequency = frequency)
}

#' Generate Series with Strong Seasonal Pattern
#'
#' Creates a time series with pronounced seasonal pattern for testing
#' seasonal models like SARIMA and ss_ar1_saz.
#'
#' @param n Number of observations
#' @param frequency Seasonal frequency
#' @param amplitude Amplitude of seasonal component
#' @param trend_coef Linear trend coefficient
#' @param noise_sd Standard deviation of noise
#' @param seed Random seed for reproducibility
#'
#' @return Time series object with strong seasonality
#'
#' @keywords internal

make_seasonal_series <- function(
    n = 144,
    frequency = 12,
    amplitude = 20,
    trend_coef = 0.05,
    noise_sd = 2,
    seed = NULL
) {
    if (!is.null(seed)) set.seed(seed)

    t <- seq_len(n)

    trend <- trend_coef * t

    seasonal <- amplitude * sin(2 * pi * t / frequency) +
        amplitude * 0.3 * cos(4 * pi * t / frequency)

    y <- 100 + trend + seasonal + rnorm(n, sd = noise_sd)

    ts(y, frequency = frequency)
}

#' Generate Regression Data and Series Pair
#'
#' Creates a regdata data.frame and corresponding time series with
#' known linear relationship for testing regression models.
#'
#' @param n Number of observations
#' @param n_predictors Number of predictor variables
#' @param coefs Coefficients (if NULL, random)
#' @param frequency Time series frequency
#' @param noise_sd Standard deviation of error term
#' @param seed Random seed for reproducibility
#' @param cor_predictors Correlation between predictors (0 = independent)
#'
#' @return List with elements:
#'   \describe{
#'     \item{serie}{Time series response variable}
#'     \item{regdata}{Data frame with predictor variables}
#'     \item{coefs}{True coefficients used}
#'   }
#'
#' @keywords internal

make_regression_data <- function(
    n = 100,
    n_predictors = 3,
    coefs = NULL,
    frequency = 12,
    noise_sd = 1,
    seed = NULL,
    cor_predictors = 0
) {
    if (!is.null(seed)) set.seed(seed)

    if (is.null(coefs)) {
        coefs <- runif(n_predictors, min = -2, max = 2)
    }

    stopifnot(length(coefs) == n_predictors)

    if (cor_predictors == 0) {
        X <- matrix(rnorm(n * n_predictors), nrow = n)
    } else {
        sigma <- matrix(cor_predictors, n_predictors, n_predictors)
        diag(sigma) <- 1
        X <- MASS::mvrnorm(n, mu = rep(0, n_predictors), Sigma = sigma)
    }

    regdata <- as.data.frame(X)
    names(regdata) <- paste0("X", seq_len(n_predictors))

    y <- as.numeric(X %*% coefs) + rnorm(n, sd = noise_sd)
    serie <- ts(y, frequency = frequency)

    list(serie = serie, regdata = regdata, coefs = coefs)
}

#' Generate Data for Periodic Model Testing
#'
#' Creates multiple time series of equal length representing different
#' periods (e.g., months) for testing modprevP.
#'
#' @param n_periods Number of periods (e.g., 12 for months)
#' @param len_period Length of each period
#' @param base_pattern Base pattern that repeats (if NULL, random)
#' @param noise_sd Standard deviation of noise added to each period
#' @param seed Random seed for reproducibility
#'
#' @return List of time series, one per period
#'
#' @keywords internal

make_periodic_data <- function(
    n_periods = 12,
    len_period = 10,
    base_pattern = NULL,
    noise_sd = 1,
    seed = NULL
) {
    if (!is.null(seed)) set.seed(seed)

    if (is.null(base_pattern)) {
        t <- seq_len(len_period)
        base_pattern <- 10 + 5 * sin(2 * pi * t / len_period)
    }

    stopifnot(length(base_pattern) == len_period)

    series_list <- lapply(seq_len(n_periods), function(i) {
        level_shift <- rnorm(1, mean = 0, sd = noise_sd * 2)
        y <- base_pattern + level_shift + rnorm(len_period, sd = noise_sd)

        ts(y, frequency = 1)
    })

    series_list
}

#' Generate Series with Missing Values
#'
#' Creates a time series with randomly placed missing values for testing
#' model robustness to incomplete data.
#'
#' @param n Number of observations
#' @param prop_missing Proportion of values to set to NA (0 to 1)
#' @param frequency Time series frequency
#' @param pattern Type of missingness ("random", "block", "end")
#' @param seed Random seed for reproducibility
#'
#' @return Time series with NA values
#'
#' @keywords internal

make_missing_data_series <- function(
    n = 100,
    prop_missing = 0.1,
    frequency = 12,
    pattern = "random",
    seed = NULL
) {
    if (!is.null(seed)) set.seed(seed)

    serie <- make_univariate_series(n = n, frequency = frequency, seed = seed)

    n_missing <- floor(n * prop_missing)

    if (n_missing == 0) return(serie)

    if (pattern == "random") {
        missing_idx <- sample(n, n_missing)
        serie[missing_idx] <- NA
    } else if (pattern == "block") {
        start_idx <- sample(1:(n - n_missing + 1), 1)
        serie[start_idx:(start_idx + n_missing - 1)] <- NA
    } else if (pattern == "end") {
        serie[(n - n_missing + 1):n] <- NA
    }

    serie
}

#' Generate New Data for Prediction Testing
#'
#' Creates newdata/newregdata for testing predict() methods of
#' regression models.
#'
#' @param n Number of future observations
#' @param regdata Original regdata (to match structure)
#' @param seed Random seed for reproducibility
#'
#' @return Data frame with same structure as regdata
#'
#' @keywords internal

make_newdata <- function(n, regdata, seed = NULL) {
    if (!is.null(seed)) set.seed(seed)

    newdata <- as.data.frame(
        lapply(regdata, function(col) {
            if (is.numeric(col)) {
                rnorm(n, mean = mean(col), sd = sd(col))
            } else if (is.factor(col)) {
                sample(levels(col), n, replace = TRUE)
            } else {
                rep(NA, n)
            }
        })
    )

    newdata
}
