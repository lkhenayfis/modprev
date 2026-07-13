####################################################################################################
# META-TESTS FOR INTEGRATION HELPERS
####################################################################################################

test_that("with_registered_models executes callback for each model", {
    tested <- character(0)

    result <- with_registered_models(function(tipo) {
        tested <<- c(tested, tipo)
        tipo
    })

    expect_true(length(tested) > 0)
    expect_true("sarima" %in% tested)
    expect_type(result, "list")
})

test_that("test_model_workflow returns valid result for model without explanatory variables", {
    result <- test_model_workflow("sarima", test_update = TRUE)

    expect_type(result, "list")
    expect_named(result, c("fit", "pred", "updated"))
    expect_s3_class(result$fit, "sarima")
    expect_s3_class(result$pred, "ts")
})

test_that("expect_janelamovel_compatible works with model without explanatory variables", {
    result <- expect_janelamovel_compatible("sarima")

    expect_type(result, "list")
    expect_true(length(result) > 0)
})

test_that("expect_periodic_compatible works with model without explanatory variables", {
    result <- expect_periodic_compatible("sarima")

    expect_s3_class(result, "modprevP")
    expect_s3_class(result, "modprev")
})
