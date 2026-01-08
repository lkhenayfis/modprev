test_that("evaluate_validation_control", {
    f <- evaluate_validation_control
    expect_true(is.function(f))

    test_that("evaluate_validation_control evaluates function correctly", {
        fn <- function(serie, regdata) list(nfold = length(serie) %/% 10)
        serie <- ts(1:100)
        regdata <- data.frame(x = 1:100)

        result <- f(fn, serie, regdata, "cv")

        expect_type(result, "list")
        expect_named(result, "nfold")
        expect_equal(result$nfold, 10)
    })

    test_that("evaluate_validation_control passes list through", {
        ctrl_list <- list(nfold = 5, stratified = TRUE)
        serie <- ts(1:100)

        result <- f(ctrl_list, serie, NULL, "cv")

        expect_identical(result, ctrl_list)
    })

    test_that("evaluate_validation_control handles function errors", {
        fn <- function(serie, regdata) stop("Test error")

        expect_error(
            f(fn, ts(1:10), NULL, "cv"),
            "Erro ao avaliar.*cv.*Test error"
        )
    })

    test_that("evaluate_validation_control validates return type", {
        fn <- function(serie, regdata) "not a list"

        expect_error(
            f(fn, ts(1:10), NULL, "cv"),
            "deve retornar uma lista"
        )
    })

    test_that("evaluate_validation_control rejects invalid input types", {
        expect_error(
            f("invalid", ts(1:10), NULL, "cv"),
            "deve ser uma lista ou funcao"
        )
        expect_error(
            f(123, ts(1:10), NULL, "cv"),
            "deve ser uma lista ou funcao"
        )
    })

    test_that("evaluate_validation_control includes validation mode in errors", {
        fn <- function(serie, regdata) stop("custom error")

        expect_error(
            f(fn, ts(1:10), NULL, "split"),
            "modo 'split'"
        )
        expect_error(
            f(fn, ts(1:10), NULL, "cv"),
            "modo 'cv'"
        )
    })

    test_that("evaluate_validation_control handles complex return values", {
        fn <- function(serie, regdata) {
            list(
                nfold = 5,
                stratified = TRUE,
                verbose = -1,
                nrounds = 100
            )
        }

        result <- f(fn, ts(1:100), NULL, "cv")

        expect_type(result, "list")
        expect_length(result, 4)
        expect_named(result, c("nfold", "stratified", "verbose", "nrounds"))
    })

    test_that("evaluate_validation_control handles NULL regdata", {
        fn <- function(serie, regdata) {
            list(nfold = ifelse(is.null(regdata), 3, 5))
        }

        result <- f(fn, ts(1:100), NULL, "cv")

        expect_equal(result$nfold, 3)
    })

    test_that("evaluate_validation_control function receives correct arguments", {
        received_serie <- NULL
        received_regdata <- NULL

        fn <- function(serie, regdata) {
            received_serie <<- serie
            received_regdata <<- regdata
            list(nfold = 5)
        }

        serie_input <- ts(1:50)
        regdata_input <- data.frame(x = 1:50)

        f(fn, serie_input, regdata_input, "cv")

        expect_identical(received_serie, serie_input)
        expect_identical(received_regdata, regdata_input)
    })
})

test_that("validate_control_lgbm_cv", {
    f <- validate_control_lgbm_cv
    expect_true(is.function(f))

    test_that("validate_control_lgbm_cv accepts valid CV parameters", {
        ctrl <- list(nfold = 5, stratified = TRUE)

        expect_silent(f(ctrl, 100))
        result <- f(ctrl, 100)
        expect_identical(result, ctrl)
    })

    test_that("validate_control_lgbm_cv accepts empty list", {
        expect_silent(f(list(), 100))
    })

    test_that("validate_control_lgbm_cv accepts unknown parameters", {
        ctrl <- list(nfold = 5, custom_param = "allowed")
        expect_silent(f(ctrl, 100))
    })

    test_that("validate_control_lgbm_cv validates nfold type", {
        expect_error(
            f(list(nfold = "5"), 100),
            "'nfold' deve ser um numero"
        )
        expect_error(
            f(list(nfold = c(5, 10)), 100),
            "'nfold' deve ser um numero"
        )
    })

    test_that("validate_control_lgbm_cv validates nfold value", {
        expect_error(
            f(list(nfold = 1), 100),
            "'nfold' deve ser >= 2"
        )
        expect_error(
            f(list(nfold = 0), 100),
            "'nfold' deve ser >= 2"
        )
        expect_error(
            f(list(nfold = -5), 100),
            "'nfold' deve ser >= 2"
        )
    })

    test_that("validate_control_lgbm_cv validates stratified type", {
        expect_error(
            f(list(stratified = "yes"), 100),
            "'stratified' deve ser um logical"
        )
        expect_error(
            f(list(stratified = c(TRUE, FALSE)), 100),
            "'stratified' deve ser um logical"
        )
        expect_error(
            f(list(stratified = 1), 100),
            "'stratified' deve ser um logical"
        )
    })

    test_that("validate_control_lgbm_cv validates folds is matrix", {
        expect_error(
            f(list(folds = c(TRUE, FALSE, TRUE)), 100),
            "'folds' deve ser uma matriz"
        )
        expect_error(
            f(list(folds = data.frame(a = rep(TRUE, 100))), 100),
            "'folds' deve ser uma matriz"
        )
    })

    test_that("validate_control_lgbm_cv validates folds is logical", {
        folds_numeric <- matrix(1:100, ncol = 5)
        expect_error(
            f(list(folds = folds_numeric), 20),
            "'folds' deve ser uma matriz logical"
        )

        folds_char <- matrix(rep("TRUE", 100), ncol = 5)
        expect_error(
            f(list(folds = folds_char), 20),
            "'folds' deve ser uma matriz logical"
        )
    })

    test_that("validate_control_lgbm_cv validates folds row count", {
        folds_wrong_rows <- matrix(rep(TRUE, 50), nrow = 10, ncol = 5)

        expect_error(
            f(list(folds = folds_wrong_rows), 100),
            "deve ter 100 linhas.*obteve 10"
        )

        expect_error(
            f(list(folds = folds_wrong_rows), 20),
            "deve ter 20 linhas.*obteve 10"
        )
    })

    test_that("validate_control_lgbm_cv accepts valid folds matrix", {
        folds_valid <- matrix(rep(c(TRUE, FALSE), each = 50), nrow = 100, ncol = 5)
        ctrl <- list(folds = folds_valid, nfold = 5)

        expect_silent(f(ctrl, 100))
        result <- f(ctrl, 100)
        expect_identical(result, ctrl)
    })

    test_that("validate_control_lgbm_cv rejects non-list input", {
        expect_error(
            f("not a list", 100),
            "control_list deve ser uma lista"
        )
        expect_error(
            f(NULL, 100),
            "control_list deve ser uma lista"
        )
    })

    test_that("validate_control_lgbm_cv accepts nfold boundary value", {
        expect_silent(f(list(nfold = 2), 100))
        expect_silent(f(list(nfold = 100), 100))
    })

    test_that("validate_control_lgbm_cv accepts both stratified values", {
        expect_silent(f(list(stratified = TRUE), 100))
        expect_silent(f(list(stratified = FALSE), 100))
    })

    test_that("validate_control_lgbm_cv returns input unchanged", {
        ctrl <- list(nfold = 5)
        result <- f(ctrl, 100)

        expect_identical(result, ctrl)
    })
})

test_that("validate_control_split", {
    f <- validate_control_split
    expect_true(is.function(f))

    test_that("validate_control_split accepts valid oob vector", {
        oob <- c(rep(TRUE, 80), rep(FALSE, 20))
        ctrl <- list(oob = oob)

        expect_silent(f(ctrl, 100))
        result <- f(ctrl, 100)
        expect_identical(result, ctrl)
    })

    test_that("validate_control_split detects missing oob", {
        expect_error(
            f(list(), 100),
            "deve conter 'oob'"
        )
        expect_error(
            f(list(other_param = 1), 100),
            "deve conter 'oob'"
        )
    })

    test_that("validate_control_split validates oob type", {
        expect_error(
            f(list(oob = c(1, 0, 1)), 3),
            "'oob' deve ser um vetor logical"
        )
        expect_error(
            f(list(oob = "TRUE"), 1),
            "'oob' deve ser um vetor logical"
        )
        expect_error(
            f(list(oob = c("TRUE", "FALSE")), 2),
            "'oob' deve ser um vetor logical"
        )
    })

    test_that("validate_control_split validates oob length", {
        oob <- c(TRUE, FALSE, TRUE)

        expect_error(
            f(list(oob = oob), 5),
            "comprimento 5.*obteve 3"
        )
        expect_error(
            f(list(oob = oob), 2),
            "comprimento 2.*obteve 3"
        )
    })

    test_that("validate_control_split requires both in-sample and out-of-sample", {
        expect_error(
            f(list(oob = rep(TRUE, 10)), 10),
            "pelo menos uma observacao out-of-sample"
        )
        expect_error(
            f(list(oob = rep(FALSE, 10)), 10),
            "pelo menos uma observacao in-sample"
        )
    })

    test_that("validate_control_split rejects non-list input", {
        expect_error(
            f("not a list", 10),
            "control_list deve ser uma lista"
        )
        expect_error(
            f(NULL, 10),
            "control_list deve ser uma lista"
        )
    })

    test_that("validate_control_split accepts minimal valid split", {
        oob <- c(TRUE, FALSE)
        expect_silent(f(list(oob = oob), 2))
    })

    test_that("validate_control_split handles various oob patterns", {
        expect_silent(f(list(oob = c(TRUE, TRUE, FALSE)), 3))
        expect_silent(f(list(oob = c(FALSE, TRUE, FALSE)), 3))
        expect_silent(f(list(oob = c(TRUE, FALSE, TRUE, FALSE)), 4))
    })

    test_that("validate_control_split returns input unchanged", {
        ctrl <- list(oob = c(TRUE, FALSE))
        result <- f(ctrl, 2)

        expect_identical(result, ctrl)
    })

    test_that("validate_control_split accepts additional parameters", {
        ctrl <- list(oob = c(TRUE, FALSE), extra_param = "allowed")
        expect_silent(f(ctrl, 2))
    })
})
