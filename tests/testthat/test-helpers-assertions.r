####################################################################################################
# META-TESTS FOR ASSERTION HELPERS
####################################################################################################

test_that("expect_modprev_structure", {
    f <- expect_modprev_structure
    expect_true(is.function(f))

    test_that("passes with valid modprevU object", {
        mod <- estimamodelo(AirPassengers, "sarima")

        expect_error(f(mod, "sarima"), NA)
    })

    test_that("fails with wrong class", {
        bad <- list(modelo = NULL, serie = ts(1:10))

        expect_error(f(bad, "sarima"))
    })

    test_that("validates class order", {
        mod <- estimamodelo(AirPassengers, "sarima")
        class(mod) <- rev(class(mod))

        expect_error(f(mod, "sarima"))
    })

    test_that("validates subclass when provided", {
        serie <- ts(rnorm(120), frequency = 12)
        mod <- estimamodelo(serie, "sarima", periodico = TRUE)

        expect_error(f(mod, "sarima", subclass = "modprevP"), NA)
        expect_error(f(mod, "sarima", subclass = "wrong_class"))
    })

    test_that("validates required fields", {
        mod <- estimamodelo(AirPassengers, "sarima")
        mod_missing_modelo <- mod
        mod_missing_modelo$modelo <- NULL

        expect_error(f(mod_missing_modelo, "sarima"))

        mod_missing_serie <- mod
        mod_missing_serie$serie <- NULL

        expect_error(f(mod_missing_serie, "sarima"))
    })

    test_that("validates serie is ts", {
        mod <- estimamodelo(AirPassengers, "sarima")
        mod$serie <- as.numeric(mod$serie)

        expect_error(f(mod, "sarima"))
    })
})

test_that("expect_prediction_format", {
    f <- expect_prediction_format
    expect_true(is.function(f))

    test_that("passes with valid prediction", {
        mod <- estimamodelo(AirPassengers, "sarima")
        pred <- predict(mod, n.ahead = 12)

        expect_error(f(pred, n.ahead = 12), NA)
    })

    test_that("validates is time series", {
        pred <- matrix(1:24, ncol = 2)
        colnames(pred) <- c("prev", "sd")

        expect_error(f(pred))
    })

    test_that("validates number of columns", {
        pred <- ts(matrix(1:30, ncol = 3), frequency = 12)

        expect_error(f(pred))
    })

    test_that("validates column names", {
        pred <- ts(matrix(1:24, ncol = 2), frequency = 12)
        colnames(pred) <- c("wrong", "names")

        expect_error(f(pred))
    })

    test_that("validates number of rows when n.ahead provided", {
        pred <- ts(matrix(1:20, ncol = 2), frequency = 12)
        colnames(pred) <- c("prev", "sd")

        expect_error(f(pred, n.ahead = 12))
    })

    test_that("validates no NA in predictions", {
        pred <- ts(matrix(c(1:10, NA, 12:22), ncol = 2), frequency = 12)
        colnames(pred) <- c("prev", "sd")

        expect_error(f(pred))
    })

    test_that("validates positive standard deviations", {
        pred <- ts(matrix(c(1:12, -1, 2:12), ncol = 2), frequency = 12)
        colnames(pred) <- c("prev", "sd")

        expect_error(f(pred))
    })

    test_that("validates tsp when requested", {
        pred <- ts(matrix(1:24, ncol = 2), start = c(2020, 1), frequency = 12)
        colnames(pred) <- c("prev", "sd")
        pred[, "sd"] <- abs(pred[, "sd"])

        expect_error(f(pred, check_tsp = TRUE), NA)
    })
})

test_that("expect_update_preserves_class", {
    f <- expect_update_preserves_class
    expect_true(is.function(f))

    test_that("passes when class preserved", {
        mod <- estimamodelo(AirPassengers, "sarima")
        updated <- update(mod, AirPassengers, refit = FALSE)

        expect_error(f(mod, updated), NA)
    })

    test_that("fails when class changed", {
        mod <- estimamodelo(AirPassengers, "sarima")
        updated <- update(mod, AirPassengers, refit = FALSE)
        class(updated) <- c("different", "class")

        expect_error(f(mod, updated))
    })

    test_that("validates required fields in updated", {
        mod <- estimamodelo(AirPassengers, "sarima")
        updated <- update(mod, AirPassengers, refit = FALSE)
        updated$modelo <- NULL

        expect_error(f(mod, updated))
    })

    test_that("validates serie is ts in updated", {
        mod <- estimamodelo(AirPassengers, "sarima")
        updated <- update(mod, AirPassengers, refit = FALSE)
        updated$serie <- as.numeric(updated$serie)

        expect_error(f(mod, updated))
    })
})

test_that("expect_compatible_tsp", {
    f <- expect_compatible_tsp
    expect_true(is.function(f))

    test_that("passes with valid time series", {
        serie <- ts(1:100, frequency = 12)

        expect_error(f(serie), NA)
    })

    test_that("validates is time series", {
        not_ts <- 1:100

        expect_error(f(not_ts))
    })

    test_that("validates expected frequency", {
        serie <- ts(1:100, frequency = 12)

        expect_error(f(serie, expected_freq = 12), NA)
        expect_error(f(serie, expected_freq = 4))
    })

    test_that("validates expected length", {
        serie <- ts(1:100, frequency = 12)

        expect_error(f(serie, expected_length = 100), NA)
        expect_error(f(serie, expected_length = 50))
    })

    test_that("validates follows_serie logic", {
        serie <- ts(1:100, start = c(2000, 1), frequency = 12)
        continuation <- ts(101:112, start = c(2008, 5), frequency = 12)

        expect_error(f(continuation, follows_serie = serie), NA)
    })

    test_that("detects wrong frequency when follows_serie", {
        serie <- ts(1:100, frequency = 12)
        wrong_freq <- ts(101:112, start = end(serie) + c(0, 1), frequency = 4)

        expect_error(f(wrong_freq, follows_serie = serie))
    })
})
