test_that("LGBM functional validation_control", {

    test_that("LGBM accepts function-based validation_control for CV mode", {
        skip_if_not_installed("lightgbm")

        set.seed(123)
        n <- 100
        serie <- ts(rnorm(n))
        regdata <- data.frame(x1 = rnorm(n), x2 = rnorm(n))

        ctrl_fn <- function(serie, regdata) {
            list(nfold = 5, nrounds = 10, verbose = -1)
        }

        mod <- LGBM(
            serie, regdata,
            validation = "cv",
            validation_control = ctrl_fn,
            train_params = list(objective = "regression")
        )

        expect_s3_class(mod, "LGBM")
        expect_s3_class(mod, "modprev")
    })

    test_that("LGBM accepts function-based validation_control for split mode", {
        skip_if_not_installed("lightgbm")

        set.seed(123)
        n <- 100
        serie <- ts(rnorm(n))
        regdata <- data.frame(x1 = rnorm(n), x2 = rnorm(n))

        ctrl_fn <- function(serie, regdata) {
            n <- length(serie)
            list(oob = c(rep(TRUE, floor(n * 0.8)), rep(FALSE, ceiling(n * 0.2))))
        }

        mod <- LGBM(
            serie, regdata,
            validation = "split",
            validation_control = ctrl_fn,
            train_params = list(objective = "regression", nrounds = 10, verbose = -1)
        )

        expect_s3_class(mod, "LGBM")
        expect_s3_class(mod, "modprev")
    })

    test_that("LGBM backward compatible with list validation_control for CV", {
        skip_if_not_installed("lightgbm")

        set.seed(123)
        n <- 100
        serie <- ts(rnorm(n))
        regdata <- data.frame(x1 = rnorm(n), x2 = rnorm(n))

        mod <- LGBM(
            serie, regdata,
            validation = "cv",
            validation_control = list(nfold = 5, nrounds = 10, verbose = -1),
            train_params = list(objective = "regression")
        )

        expect_s3_class(mod, "LGBM")
    })

    test_that("LGBM backward compatible with list validation_control for split", {
        skip_if_not_installed("lightgbm")

        set.seed(123)
        n <- 100
        serie <- ts(rnorm(n))
        regdata <- data.frame(x1 = rnorm(n), x2 = rnorm(n))

        oob <- c(rep(TRUE, 80), rep(FALSE, 20))

        mod <- LGBM(
            serie, regdata,
            validation = "split",
            validation_control = list(oob = oob),
            train_params = list(objective = "regression", nrounds = 10, verbose = -1)
        )

        expect_s3_class(mod, "LGBM")
    })

    test_that("LGBM rejects function returning non-list", {
        skip_if_not_installed("lightgbm")

        n <- 100
        serie <- ts(rnorm(n))
        regdata <- data.frame(x1 = rnorm(n), x2 = rnorm(n))

        ctrl_fn <- function(serie, regdata) "not a list"

        expect_error(
            LGBM(serie, regdata, validation = "cv", validation_control = ctrl_fn),
            "deve retornar uma lista"
        )
    })

    test_that("LGBM reports errors from validation_control function", {
        skip_if_not_installed("lightgbm")

        n <- 100
        serie <- ts(rnorm(n))
        regdata <- data.frame(x1 = rnorm(n), x2 = rnorm(n))

        ctrl_fn <- function(serie, regdata) stop("custom error in function")

        expect_error(
            LGBM(serie, regdata, validation = "cv", validation_control = ctrl_fn),
            "Erro ao avaliar.*cv.*custom error"
        )
    })

    test_that("LGBM validates CV control parameters from function", {
        skip_if_not_installed("lightgbm")

        n <- 100
        serie <- ts(rnorm(n))
        regdata <- data.frame(x1 = rnorm(n), x2 = rnorm(n))

        ctrl_fn <- function(serie, regdata) list(nfold = 1)

        expect_error(
            LGBM(serie, regdata, validation = "cv", validation_control = ctrl_fn),
            "'nfold' deve ser >= 2"
        )
    })

    test_that("LGBM validates split control parameters from function", {
        skip_if_not_installed("lightgbm")

        n <- 100
        serie <- ts(rnorm(n))
        regdata <- data.frame(x1 = rnorm(n), x2 = rnorm(n))

        ctrl_fn <- function(serie, regdata) list(other_param = TRUE)

        expect_error(
            LGBM(serie, regdata, validation = "split", validation_control = ctrl_fn),
            "deve conter 'oob'"
        )
    })

    test_that("LGBM function receives correct serie and regdata arguments", {
        skip_if_not_installed("lightgbm")

        n <- 100
        serie <- ts(rnorm(n))
        regdata <- data.frame(x1 = rnorm(n), x2 = rnorm(n))

        received_serie <- NULL
        received_regdata <- NULL

        ctrl_fn <- function(serie, regdata) {
            received_serie <<- serie
            received_regdata <<- regdata
            list(nfold = 3, nrounds = 5, verbose = -1)
        }

        LGBM(
            serie, regdata,
            validation = "cv",
            validation_control = ctrl_fn,
            train_params = list(objective = "regression")
        )

        expect_true(is.ts(received_serie))
        expect_equal(length(received_serie), n)
        expect_identical(received_regdata, regdata)
    })

    test_that("LGBM adaptive CV based on data size", {
        skip_if_not_installed("lightgbm")

        n <- 200
        serie <- ts(rnorm(n))
        regdata <- data.frame(x1 = rnorm(n), x2 = rnorm(n))

        ctrl_fn <- function(serie, regdata) {
            n <- length(serie)
            nfold <- max(3, min(10, n %/% 20))
            list(nfold = nfold, nrounds = 5, verbose = -1)
        }

        mod <- LGBM(
            serie, regdata,
            validation = "cv",
            validation_control = ctrl_fn,
            train_params = list(objective = "regression")
        )

        expect_s3_class(mod, "LGBM")
    })

    test_that("LGBM adaptive split based on data size", {
        skip_if_not_installed("lightgbm")

        n <- 150
        serie <- ts(rnorm(n))
        regdata <- data.frame(x1 = rnorm(n), x2 = rnorm(n))

        ctrl_fn <- function(serie, regdata) {
            n <- length(serie)
            train_size <- floor(n * 0.7)
            list(oob = c(rep(TRUE, train_size), rep(FALSE, n - train_size)))
        }

        mod <- LGBM(
            serie, regdata,
            validation = "split",
            validation_control = ctrl_fn,
            train_params = list(objective = "regression", nrounds = 10, verbose = -1)
        )

        expect_s3_class(mod, "LGBM")
    })

    test_that("LGBM none validation mode ignores validation_control function", {
        skip_if_not_installed("lightgbm")

        n <- 100
        serie <- ts(rnorm(n))
        regdata <- data.frame(x1 = rnorm(n), x2 = rnorm(n))

        ctrl_fn <- function(serie, regdata) stop("should not be called")

        mod <- LGBM(
            serie, regdata,
            validation = "none",
            validation_control = ctrl_fn,
            train_params = list(objective = "regression", nrounds = 10, verbose = -1)
        )

        expect_s3_class(mod, "LGBM")
    })

    test_that("LGBM validation function can access regdata features", {
        skip_if_not_installed("lightgbm")

        n <- 100
        serie <- ts(rnorm(n))
        regdata <- data.frame(
            x1 = rnorm(n),
            x2 = rnorm(n),
            category = sample(c("A", "B", "C"), n, replace = TRUE)
        )

        ctrl_fn <- function(serie, regdata) {
            has_categorical <- "category" %in% names(regdata)
            list(
                nfold = ifelse(has_categorical, 5, 3),
                nrounds = 5,
                verbose = -1
            )
        }

        mod <- LGBM(
            serie, regdata[, c("x1", "x2")],
            validation = "cv",
            validation_control = ctrl_fn,
            train_params = list(objective = "regression")
        )

        expect_s3_class(mod, "LGBM")
    })
})
