# ==============================================================================
# Cross-Model Consistency Tests
#
# Purpose:
#   Validate cross-model consistency properties that cannot be tested
#   individually through behavioral tests.
#
# This file contains tests that require comparing behavior across all models
# to identify inconsistencies or document expected variations.
#
# Note: Most consistency checks (class hierarchy, component structure,
# prediction format) are handled by behavioral tests in test-model-behaviors.r
# using enhanced assertion helpers.
# ==============================================================================

test_that("all models handle missing data consistently", {
    missing_results <- list()

    with_registered_models(univariate_only = TRUE, function(tipo) {
        serie <- make_missing_data_series(n = 100, prop_missing = 0.1, seed = 108)

        result <- tryCatch({
            mod <- estimamodelo(serie, tipo)
            list(success = TRUE, error = NULL)
        }, error = function(e) {
            list(success = FALSE, error = e)
        })

        missing_results[[tipo]] <<- result
    })

    for (tipo in names(missing_results)) {
        result <- missing_results[[tipo]]

        if (!result$success) {
            testthat::expect_true(
                nchar(result$error$message) > 10,
                info = sprintf("%s error message should be informative", tipo)
            )
        }
    }
})
