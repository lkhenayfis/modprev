####################################################################################################
# META-TESTS FOR ASSERTION HELPERS
####################################################################################################

test_that("expect_modprev_structure passes with valid modprevU object", {
    mod <- estimamodelo(AirPassengers, "sarima")

    expect_no_error(expect_modprev_structure(mod, "sarima"))
    expect_error(expect_modprev_structure(list(modelo = NULL, serie = ts(1:10)), "sarima"))
})

test_that("expect_prediction_format passes with valid prediction", {
    mod <- estimamodelo(AirPassengers, "sarima")
    pred <- predict(mod, n.ahead = 12)

    expect_no_error(expect_prediction_format(pred, n.ahead = 12))
    expect_error(expect_prediction_format(matrix(1:24, ncol = 2)))
})

test_that("expect_update_preserves_class passes when class preserved", {
    mod <- estimamodelo(AirPassengers, "sarima")
    updated <- update(mod, AirPassengers, refit = FALSE)

    expect_no_error(expect_update_preserves_class(mod, updated))
})

test_that("expect_compatible_tsp passes with valid time series", {
    serie <- ts(1:100, frequency = 12)

    expect_no_error(expect_compatible_tsp(serie))
    expect_error(expect_compatible_tsp(1:100))
})
