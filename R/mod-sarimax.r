####################################################################################################
# MODELO SARIMAX
####################################################################################################

#' Modelos \code{sarimax}
#' 
#' Estimação e métodos de modelos da classe \code{sarimax}
#' 
#' Esta especificação identifica e ajusta um modelo SARIMAX(p, d, q)(P, D, Q)^s através da função
#' \code{\link[forecast]{auto.arima}}, que realiza uma busca no espaço de hiperparâmetros de ordem
#' do modelo por aquele conjunto com menor BIC.
#' 
#' @name modelos_sarimax
NULL

# ESTIMACAO ----------------------------------------------------------------------------------------

#' @param serie serie para ajustar
#' @param regdata \code{data.frame}-like contendo variáveis explicativas
#' @param formula formula da regressão. Se for omitido, todas as variaveis em \code{regdata} serão
#'     utilizadas. So tem uso se \code{regdata} for passado
#' 
#' @return Objeto da classe \code{modprev} e subclasse \code{sarimax}, uma lista de dois elementos:
#'     \code{modelo} e \code{serie} contendo o modelo estimado e a série passada
#' 
#' @rdname modelos_sarimax

sarimax <- function(serie, regdata = NULL, formula = NULL) {

    if(is.null(formula)) {
        formula <- paste0(colnames(regdata), collapse = " + ")
        formula <- as.formula(paste0("~ ", formula))
    }

    Xreg <- model.frame(formula, data = regdata)
    Xreg <- data.matrix(Xreg)

    mod <- auto.arima(serie, allowdrift = FALSE, xreg = Xreg)
    out <- new_modprev(mod, serie, "sarimax")

    return(out)
}
