####################################################################################################
# MODELO REGRESSAO ESTATICA SIMPLES
####################################################################################################

#' Modelos \code{reg_lin}
#' 
#' Estimação e métodos de modelos da classe \code{reg_lin}
#' 
#' Modelos de regressao linear estaticas comuns, estimados atraves da funcao \code{\link[stats]{lm}}
#' 
#' @name modelos_reg_lin
NULL

# ESTIMACAO ----------------------------------------------------------------------------------------

#' \bold{Estimação}:
#' 
#' Para estimação deste tipo de modelo, além de ser fornecido o arumento \code{serie} também deve 
#' ser necessariamente informado \code{regdata}, um \code{data.frame}-like contendo as variáveis 
#' explicativas necessárias. 
#' 
#' Opcionalmente pode ser informado o argumento \code{formula} contendo a especificação da regressão
#' linear, no formato padrão do R (veja \code{\link{formula}}) porém sem o LHS. Caso \code{formula} 
#' seja omitido, todas as variáveis em \code{regdata} serão utilizadas aditivamemte, i.e. se existem
#' as colunas \code{c("V1", "V2", "V3")}, formula sera \code{~ V1 + V2 + V3}.
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

    if(missing(regdata)) stop("Forneca a variavel explicativa atraves do parametro 'regdata'")

    if(missing(formula)) formula <- expandeformula(regdata)
    formula <- update(formula, Y ~ .)

    if(!is.ts(serie)) serie <- ts(serie)
    aux_tsp <- tsp(serie)

    regdata <- cbind(Y = as.numeric(serie), regdata)
    fit <- lm(formula, regdata)

    mod_atrs <- list(formula = formula, tsp = aux_tsp)

    new_modprevU(fit, serie, "reg_lin", mod_atrs)
}

# METODOS ------------------------------------------------------------------------------------------

#' \bold{Predict}:
#' 
#' A previsão destes modelos é feita com um argumento opcional já passado por padrão que não pode
#' ser modificado. Este é \code{se.fit = TRUE}, determinando que seja retornado o desvio padrão 
#' associado a previsão realizada. Considerando estes fatores, \code{...} não pode conter 
#' \code{n.ahead} ou estes dois outros, ou então ocorrerá erro.
#' 
#' @param object objeto com classes \code{c("reg_lin", "modprev")} contendo modelo
#' @param newdata \code{data.frame}-like contendo variéveis explicativas fora da amostra
#' @param n.ahead número de passos à frente para previsão. Este argumento não é necessario, caso não
#'     seja informado a previsão sera feita tantos passos à frente quanto amostras em \code{newdata}
#' @param ... nao possui uso, existe apenas para consistencia com a generica
#' 
#' @return \code{predict} serie temporal multivariada contendo valor esperado e desvio padrao de
#'     previsão \code{n.ahead} passos à frente;
#' 
#' @rdname modelos_reg_lin
#' 
#' @export

predict.reg_lin <- function(object, newdata, n.ahead, ...) {
    modelo <- object$modelo
    aux_tsp <- attr(object, "mod_atrs")$tsp

    if(missing(newdata)) stop("Forneca a variavel explicativa para previsao atraves do parametro 'newdata'")

    if(!missing(n.ahead)) {
        regobs <- min(n.ahead, nrow(newdata))
        newdata <- newdata[seq(regobs), , drop = FALSE]
    }

    prev <- predict(modelo, newdata = newdata, se.fit = TRUE)
    prev <- do.call(cbind, prev[1:2])
    colnames(prev) <- c("prev", "sd")

    prox_t <- aux_tsp[2] + 1 / aux_tsp[3]
    prev <- ts(prev, start = prox_t, frequency = aux_tsp[3])

    return(prev)
}

#' @details 
#' 
#' @param newseries nova série com a qual atualizar o modelo
#' @param newregdata \code{data.frame}-like contendo variáveis explicativas na nova amostra
#' @param refit booleano indicando se o modelo deve ou nao ser reajustado
#' @param ... nao possui uso, existe apenas para consistencia com a generica
#' 
#' @return \code{update} retorna modelo com novos dados e, caso \code{refit == TRUE}, reajustado. 
#'     Contrário à função de estimação, \code{update} já retorna o objeto da classe \code{modprev};
#' 
#' @rdname modelos_reg_lin
#' 
#' @export

update.reg_lin <- function(object, newseries, newregdata, refit = FALSE, ...) {

    mod_atrs <- attr(object, "mod_atrs")

    if(refit) {
        formula <- mod_atrs$formula
        object  <- estimamodelo(newseries, "reg_lin", regdata = newregdata, formula = formula)
    } else {

        modelo <- object$modelo

        if(missing(newregdata)) stop("Forneca nova variavel explicativa atraves do parametro 'newregdata'")

        modelo$model <- cbind.data.frame(Y = as.numeric(newseries), newregdata)
        modelo$fitted.values <- predict(modelo)
        modelo$residuals <- as.numeric(newseries) - fitted(modelo)

        mod_atrs$tsp <- tsp(newseries)

        object <- new_modprevU(modelo, newseries, "reg_lin", mod_atrs)
    }

    return(object)
}
