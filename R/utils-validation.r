####################################################################################################
# VALIDATION CONTROL UTILITIES
####################################################################################################

#' Evaluate Validation Control Parameter
#'
#' Evaluates validation_control if it is a function, or passes through if list
#'
#' This function supports both static list and function-based specification of
#' validation control parameters. When a function is provided, it is called with
#' serie and regdata to produce the control list dynamically.
#'
#' @param validation_control Either a list or function. If function, it should
#'     have signature \code{function(serie, regdata)} and return a named list
#'     of validation control parameters
#' @param serie Time series object being modeled
#' @param regdata Data frame or matrix of regressors
#' @param validation Character string specifying validation mode ("cv" or "split")
#'
#' @return Named list of validation control parameters suitable for the
#'     specified validation mode

evaluate_validation_control <- function(validation_control, serie, regdata, validation) {

    if (is.function(validation_control)) {

        control_list <- tryCatch(
            validation_control(serie, regdata),
            error = function(e) {
                stop(sprintf(
                    "Erro ao avaliar validation_control function para modo '%s': %s",
                    validation, e$message
                ), call. = FALSE)
            }
        )

        if (!is.list(control_list)) {
            stop(sprintf(
                "validation_control function deve retornar uma lista, obteve %s",
                class(control_list)[1]
            ), call. = FALSE)
        }

        return(control_list)

    } else if (is.list(validation_control)) {
        return(validation_control)

    } else {
        stop(sprintf(
            "validation_control deve ser uma lista ou funcao, obteve %s",
            class(validation_control)[1]
        ), call. = FALSE)
    }
}

#' Validate LGBM CV Mode Control Parameters
#'
#' Checks that control list contains valid parameters for LGBM cross-validation
#'
#' @param control_list Named list of validation control parameters
#' @param serie_length Integer length of the series being modeled
#'
#' @return The input control_list (invisibly) if valid, otherwise raises error

validate_control_lgbm_cv <- function(control_list, serie_length) {

    if (!is.list(control_list)) {
        stop("control_list deve ser uma lista", call. = FALSE)
    }

    if ("nfold" %in% names(control_list)) {
        if (!is.numeric(control_list$nfold) || length(control_list$nfold) != 1) {
            stop("'nfold' deve ser um numero", call. = FALSE)
        }
        if (control_list$nfold < 2) {
            stop("'nfold' deve ser >= 2", call. = FALSE)
        }
    }

    if ("stratified" %in% names(control_list)) {
        if (!is.logical(control_list$stratified) || length(control_list$stratified) != 1) {
            stop("'stratified' deve ser um logical", call. = FALSE)
        }
    }

    if ("folds" %in% names(control_list)) {
        folds <- control_list$folds

        if (!is.matrix(folds)) {
            stop("'folds' deve ser uma matriz", call. = FALSE)
        }

        if (!is.logical(folds)) {
            stop("'folds' deve ser uma matriz logical", call. = FALSE)
        }

        if (nrow(folds) != serie_length) {
            stop(sprintf(
                "'folds' deve ter %d linhas (igual ao comprimento de serie), obteve %d",
                serie_length, nrow(folds)
            ), call. = FALSE)
        }
    }

    invisible(control_list)
}

#' Validate Split Mode Control Parameters
#'
#' Checks that control list contains required 'oob' parameter for train/test split
#'
#' @param control_list Named list of validation control parameters
#' @param serie_length Integer length of the series being modeled
#'
#' @return The input control_list (invisibly) if valid, otherwise raises error

validate_control_split <- function(control_list, serie_length) {

    if (!is.list(control_list)) {
        stop("control_list deve ser uma lista", call. = FALSE)
    }

    if (!"oob" %in% names(control_list)) {
        stop(
            "Para validacao 'split', control_list deve conter 'oob'",
            call. = FALSE
        )
    }

    oob <- control_list$oob

    if (!is.logical(oob)) {
        stop("'oob' deve ser um vetor logical", call. = FALSE)
    }

    if (length(oob) != serie_length) {
        stop(sprintf(
            "'oob' deve ter comprimento %d (igual a serie), obteve %d",
            serie_length, length(oob)
        ), call. = FALSE)
    }

    if (!any(oob)) {
        stop("'oob' deve conter pelo menos uma observacao in-sample (TRUE)", call. = FALSE)
    }

    if (!any(!oob)) {
        stop("'oob' deve conter pelo menos uma observacao out-of-sample (FALSE)", call. = FALSE)
    }

    invisible(control_list)
}

#' Validate BOOST CV Mode Control Parameters
#'
#' Checks that control list contains valid parameters for BOOST cross-validation
#'
#' This is a lightweight validator that checks basic structure only. BOOST CV
#' uses `match_fun_args()` to extract parameters for `mboost::cv()`,
#' `mboost::cvrisk()`, and `parallel::mclapply()`, so comprehensive validation
#' is not feasible. We validate only critical parameters to catch confusing
#' errors early.
#'
#' @param control_list Named list of validation control parameters
#'
#' @return The input control_list (invisibly) if valid, otherwise raises error

validate_control_boost_cv <- function(control_list) {

    if (!is.list(control_list)) {
        stop("control_list deve ser uma lista", call. = FALSE)
    }

    if ("type" %in% names(control_list)) {
        valid_types <- c("bootstrap", "kfold", "subsampling")
        matched_type <- tryCatch(
            match.arg(control_list$type, valid_types),
            error = function(e) {
                stop(sprintf(
                    "'type' deve ser um de: %s",
                    paste(valid_types, collapse = ", ")
                ), call. = FALSE)
            }
        )
    }

    if ("B" %in% names(control_list)) {
        if (!is.numeric(control_list$B) || length(control_list$B) != 1) {
            stop("'B' deve ser um numero", call. = FALSE)
        }
        if (control_list$B < 2) {
            stop("'B' deve ser >= 2", call. = FALSE)
        }
    }

    if ("prob" %in% names(control_list)) {
        if (!is.numeric(control_list$prob) || length(control_list$prob) != 1) {
            stop("'prob' deve ser um numero", call. = FALSE)
        }
        if (control_list$prob <= 0 || control_list$prob >= 1) {
            stop("'prob' deve estar entre 0 e 1", call. = FALSE)
        }
    }

    invisible(control_list)
}
