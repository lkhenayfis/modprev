####################################################################################################
# MODELO REGRESSAO QUANTILICA
####################################################################################################

#' Modelos \code{reg_quant}
#' 
#' Estimação e métodos de modelos da classe \code{reg_quant}
#' 
#' Modelos de regressao linear estaticas comuns, estimados atraves da funcao \code{\link[stats]{lm}}
#' 
#' @name modelos_reg_quant
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
#' Na execucao em janela movel, o vetor de pesos deve ser passado com o tamanho igual ao da janela.
#' O mesmo vetor sera aplicado em todas as janelas avaliadas.
#' 
#' @param serie série para ajustar
#' @param regdata \code{data.frame}-like contendo variáveis explicativas
#' @param formula opcional, fórmula da regressão. Se for omitido, todas as variaveis em 
#'     \code{regdata} serão utilizadas
#' @param pesos opcional, vetor de pesos para cada observacao no ajuste. Sera normalizado 
#'     internamente
#' @param ... nao possui uso, existe apenas para consistencia com a generica
#' 
#' @return Objeto da classe \code{modprev} e subclasse \code{reg_quant}, uma lista de dois 
#'     elementos: \code{modelo} e \code{serie} contendo o modelo estimado e a série passada
#' 
#' @rdname modelos_reg_quant

reg_quant <- function(serie, regdata, formula, pesos = rep(1, length(serie)), ...) {

    if(missing(regdata)) stop("Forneca a variavel explicativa atraves do parametro 'regdata'")

    if(missing(formula)) formula <- expandeformula(regdata)
    formula <- update(formula, Y ~ .)

    if(!is.ts(serie)) serie <- ts(serie)
    aux_tsp <- tsp(serie)

    pesos <- pesos / sum(pesos)

    regdata <- cbind(Y = as.numeric(serie), regdata, pesos = pesos)
    fit <- rq(formula, data = regdata, weights = pesos)

    mod_atrs <- list(formula = formula, tsp = aux_tsp, pesos = pesos)

    new_modprevU(fit, serie, "reg_quant", mod_atrs)
}

# METODOS ------------------------------------------------------------------------------------------

#' \bold{Predict}:
#' 
#' A previsão destes modelos é feita com um argumento opcional já passado por padrão que não pode
#' ser modificado. Este é \code{se.fit = TRUE}, determinando que seja retornado o desvio padrão 
#' associado a previsão realizada. Considerando estes fatores, \code{...} não pode conter 
#' \code{n.ahead} ou estes dois outros, ou então ocorrerá erro.
#' 
#' @param object objeto com classes \code{c("reg_quant", "modprev")} contendo modelo
#' @param newdata \code{data.frame}-like contendo variéveis explicativas fora da amostra
#' @param n.ahead número de passos à frente para previsão. Este argumento não é necessario, caso não
#'     seja informado a previsão sera feita tantos passos à frente quanto amostras em \code{newdata}
#' @param ... nao possui uso, existe apenas para consistencia com a generica
#' 
#' @return \code{predict} serie temporal multivariada contendo valor esperado e desvio padrao de
#'     previsão \code{n.ahead} passos à frente;
#' 
#' @rdname modelos_reg_quant
#' 
#' @export

predict.reg_quant <- function(object, newdata, n.ahead, ...) {
    modelo <- object$modelo
    aux_tsp <- attr(object, "mod_atrs")$tsp

    if(missing(newdata)) stop("Forneca a variavel explicativa para previsao atraves do parametro 'newdata'")

    if(!missing(n.ahead)) {
        regobs <- min(n.ahead, nrow(newdata))
        newdata <- newdata[seq(regobs), , drop = FALSE]
    }

    prev <- predict(modelo, newdata = newdata)
    prev <- cbind(prev, rep(NA_real_, length(prev)))
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
#' @rdname modelos_reg_quant
#' 
#' @export

update.reg_quant <- function(object, newseries, newregdata, refit = FALSE, ...) {

    mod_atrs <- attr(object, "mod_atrs")

    if(refit) {
        pesos   <- mod_atrs$pesos[seq_along(newseries)]
        formula <- mod_atrs$formula
        object  <- estimamodelo(newseries, "reg_lin", regdata = newregdata, formula = formula,
            pesos = pesos)
    } else {

        modelo <- object$modelo

        if(missing(newregdata)) stop("Forneca nova variavel explicativa atraves do parametro 'newregdata'")

        modelo$model <- cbind.data.frame(Y = as.numeric(newseries), newregdata)
        modelo$fitted.values <- predict(modelo)
        modelo$residuals <- as.numeric(newseries) - fitted(modelo)

        mod_atrs$tsp <- tsp(newseries)

        object <- new_modprevU(modelo, newseries, "reg_quant", mod_atrs)
    }

    return(object)
}