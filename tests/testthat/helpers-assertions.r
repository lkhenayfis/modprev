####################################################################################################
# ASSERTION HELPERS FOR MODPREV TESTING
####################################################################################################

#' Validate modprev Object Structure
#'
#' Checks that an object has the correct S3 class hierarchy and required
#' fields for a modprev model.
#'
#' @param object Model object to validate
#' @param tipo Model type (e.g., "sarima", "reg_lin")
#' @param subclass Optional additional class (e.g., "modprevP" for periodic)
#' @param has_atrs Logical; if TRUE, checks for "mod_atrs" attribute
#'
#' @return Invisibly returns the object (for chaining)
#'
#' @keywords internal

expect_modprev_structure <- function(object, tipo, subclass = NULL, has_atrs = FALSE) {
    classes <- class(object)
    testthat::expect_s3_class(object, "modprev")

    if (!is.null(subclass)) {
        testthat::expect_s3_class(object, subclass)

        subclass_pos <- which(classes == subclass)
        modprev_pos <- which(classes == "modprev")
        testthat::expect_true(
            subclass_pos < modprev_pos,
            label = sprintf("'%s' should appear before 'modprev' in class vector", subclass)
        )
    } else {
        testthat::expect_s3_class(object, "modprevU")

        testthat::expect_true(
            tipo %in% classes,
            label = sprintf("Class vector should contain '%s'", tipo)
        )

        tipo_pos <- which(classes == tipo)
        modprevu_pos <- which(classes == "modprevU")
        testthat::expect_true(
            tipo_pos < modprevu_pos,
            label = sprintf("'%s' should appear before 'modprevU' in class vector", tipo)
        )
    }

    testthat::expect_true(
        "modelo" %in% names(object) || "modelos" %in% names(object),
        label = "Object must have 'modelo' or 'modelos' component"
    )
    testthat::expect_true(
        "serie" %in% names(object),
        label = "Object must have 'serie' component"
    )

    testthat::expect_s3_class(object$serie, "ts")

    if (has_atrs) {
        testthat::expect_true(
            !is.null(attr(object, "mod_atrs")),
            label = "Object should have 'mod_atrs' attribute"
        )
    }

    invisible(object)
}

#' Validate Prediction Output Format
#'
#' Checks that predict() output has the correct structure: a time series
#' with two columns named "prev" and "sd".
#'
#' @param pred Prediction object from predict()
#' @param n.ahead Expected number of forecast steps
#' @param check_tsp Logical; if TRUE, validates tsp attributes
#' @param allow_na_sd Logical; if TRUE, allows NA values in sd column.
#'     Default is FALSE. Some models (e.g., reg_quant, BOOST, LGBM) do not
#'     provide uncertainty estimates and legitimately return NA.
#'
#' @return Invisibly returns pred (for chaining)
#'
#' @keywords internal

expect_prediction_format <- function(
    pred,
    n.ahead = NULL,
    check_tsp = FALSE,
    allow_na_sd = FALSE
) {
    testthat::expect_s3_class(pred, "ts")

    testthat::expect_equal(ncol(pred), 2, label = "Prediction should have 2 columns")

    if (!is.null(n.ahead)) {
        testthat::expect_equal(
            nrow(pred),
            n.ahead,
            label = sprintf("Prediction should have %d rows", n.ahead)
        )
    }

    testthat::expect_equal(
        colnames(pred),
        c("prev", "sd"),
        label = "Prediction columns should be 'prev' and 'sd'"
    )

    testthat::expect_true(
        typeof(pred[, "prev"]) %in% c("double", "integer"),
        label = "prev column should be numeric (double or integer)"
    )

    testthat::expect_true(
        typeof(pred[, "sd"]) %in% c("double", "integer"),
        label = "sd column should be numeric (double or integer)"
    )

    testthat::expect_false(
        any(is.na(pred[, "prev"])),
        label = "Predictions should not contain NA"
    )

    if (!allow_na_sd) {
        testthat::expect_false(
            any(is.na(pred[, "sd"])),
            label = "Standard deviations should not contain NA"
        )

        testthat::expect_true(
            all(pred[, "sd"] > 0),
            label = "Standard deviations should be positive"
        )
    } else {
        non_na_sd <- pred[!is.na(pred[, "sd"]), "sd"]
        if (length(non_na_sd) > 0) {
            testthat::expect_true(
                all(non_na_sd > 0),
                label = "Non-NA standard deviations should be positive"
            )
        }
    }

    if (check_tsp) {
        tsp_attr <- tsp(pred)
        testthat::expect_length(tsp_attr, 3)
        testthat::expect_true(tsp_attr[3] > 0, label = "Frequency should be positive")
    }

    invisible(pred)
}

#' Validate Update Method Behavior
#'
#' Checks that update() returns an object with the same class structure
#' as the original.
#'
#' @param original Original modprev object
#' @param updated Updated modprev object
#'
#' @return Invisibly returns updated (for chaining)
#'
#' @keywords internal

expect_update_preserves_class <- function(original, updated) {
    testthat::expect_equal(
        class(updated),
        class(original),
        label = "update() should preserve S3 class structure"
    )

    testthat::expect_true(
        "modelo" %in% names(updated),
        label = "Updated object should have 'modelo' component"
    )
    testthat::expect_true(
        "serie" %in% names(updated),
        label = "Updated object should have 'serie' component"
    )

    testthat::expect_s3_class(updated$serie, "ts")

    if (!is.null(attr(original, "mod_atrs"))) {
        testthat::expect_true(
            !is.null(attr(updated, "mod_atrs")),
            label = "update() should preserve 'mod_atrs' attribute"
        )
    }

    invisible(updated)
}

#' Validate Time Series Properties Compatibility
#'
#' Checks that time series properties (start, end, frequency) are
#' compatible with expectations.
#'
#' @param ts_object Time series object
#' @param expected_freq Expected frequency (optional)
#' @param expected_length Expected length (optional)
#' @param follows_serie Time series that this should follow (optional)
#'
#' @return Invisibly returns ts_object (for chaining)
#'
#' @keywords internal

expect_compatible_tsp <- function(
    ts_object,
    expected_freq = NULL,
    expected_length = NULL,
    follows_serie = NULL
) {
    testthat::expect_s3_class(ts_object, "ts")

    tsp_attr <- tsp(ts_object)

    if (!is.null(expected_freq)) {
        testthat::expect_equal(
            tsp_attr[3],
            expected_freq,
            label = sprintf("Frequency should be %d", expected_freq)
        )
    }

    if (!is.null(expected_length)) {
        testthat::expect_equal(
            length(ts_object),
            expected_length,
            label = sprintf("Length should be %d", expected_length)
        )
    }

    if (!is.null(follows_serie)) {
        serie_tsp <- tsp(follows_serie)

        testthat::expect_equal(
            tsp_attr[3],
            serie_tsp[3],
            label = "Frequency should match original series"
        )

        expected_start <- serie_tsp[2] + 1 / serie_tsp[3]
        testthat::expect_equal(
            tsp_attr[1],
            expected_start,
            tolerance = 1e-6,
            label = "Should start immediately after original series"
        )
    }

    invisible(ts_object)
}

#' Validate Modelo Component Structure
#'
#' Checks that the $modelo component has expected structure for a given
#' model type. This is model-type specific validation.
#'
#' @param modelo The $modelo component from modprev object
#' @param tipo Model type
#'
#' @return Invisibly returns modelo (for chaining)
#'
#' @keywords internal

expect_valid_modelo <- function(modelo, tipo) {
    if (tipo == "sarima") {
        testthat::expect_s3_class(modelo, "Arima")
        testthat::expect_true("fitted" %in% names(modelo))
        testthat::expect_true("residuals" %in% names(modelo))
    } else if (tipo == "sarimax") {
        testthat::expect_s3_class(modelo, "Arima")
        testthat::expect_true("fitted" %in% names(modelo))
        testthat::expect_true("residuals" %in% names(modelo))
    } else if (tipo %in% c("reg_lin", "reg_quant")) {
        testthat::expect_true("coefficients" %in% names(modelo))
        testthat::expect_true("fitted.values" %in% names(modelo))
        testthat::expect_true("residuals" %in% names(modelo))
    } else if (tipo %in% c("ss_ar1_saz", "ss_reg_din")) {
        testthat::expect_s3_class(modelo, "SSModel")
    } else if (tipo == "GAM") {
        testthat::expect_s3_class(modelo, "gam")
    } else if (tipo == "BOOST") {
        testthat::expect_s3_class(modelo, "mboost")
    } else if (tipo == "LGBM") {
        testthat::expect_s3_class(modelo, "lgb.Booster")
    }

    invisible(modelo)
}
