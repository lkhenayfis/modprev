####################################################################################################
# MODELO REGRESSAO ESTATICA SIMPLES
####################################################################################################

#' Modelos \code{reg_lin}
#' 
#' Estimação e métodos de modelos da classe \code{reg_lin}
#' 
#' @name modelos_reg_lin
NULL

# ESTIMACAO ----------------------------------------------------------------------------------------

#' \bold{Estimação}:
#' 
#' @param serie série para ajustar
#' @param regdata \code{data.frame}-like contendo variáveis explicativas
#' @param formula opcional, fórmula da regressão. Se for omitido, todas as variaveis em 
#'     \code{regdata} serão utilizadas
#' 
#' @return Objeto da classe \code{modprev} e subclasse \code{reg_lin}, uma lista de dois 
#'     elementos: \code{modelo} e \code{serie} contendo o modelo estimado e a série passada
#' 
#' @rdname modelos_reg_lin

reg_lin <- function(serie, regdata, formula, ...) {

    if(missing(regdata)) stop("Forneca a variavel explicativa atraves do parametro regdata")

    if(missing(formula) || is.null(formula)) {
        warning("'formula' nao foi passado -- usando todas as variaveis de forma aditiva")
        formula <- colnames(regdata)
        if(length(formula) > 1) formula <- paste0(colnames(regdata), collapse = " + ")
        formula <- paste0("Y ~ ", formula)
        formula <- as.formula(formula)
    }

    if(!is.ts(serie)) serie <- ts(serie)
    aux_tsp <- tsp(serie)

    regdata <- cbind(Y = as.numeric(serie), regdata)
    fit <- lm(formula, regdata)

    mod_atrs <- list(tsp = aux_tsp)

    new_modprev(fit, serie, "reg_lin", mod_atrs)
}

