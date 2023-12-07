####################################################################################################
# MODELO REGRESSAO ESTATICA SIMPLES
####################################################################################################

#' Modelos \code{GAM}
#' 
#' Estimação e métodos de modelos da classe \code{GAM}
#' 
#' Modelos Aditivos Generalizados, estimados atraves da funcao \code{\link[mgcv]{gam}}. Para mais
#' detalhes a respeito desta modelagem e seu uso, veja a documentacao oficial do pacote.
#' 
#' @name modelos_gam
#' 
#' @import mgcv
NULL

# ESTIMACAO ----------------------------------------------------------------------------------------

#' \bold{Estimação}:
#' 
#' Para estimação deste tipo de modelo, além de ser fornecido o arumento \code{serie} também deve 
#' ser necessariamente informado \code{regdata}, um \code{data.frame}-like contendo as variáveis 
#' explicativas necessárias. 
#' 
#' Opcionalmente pode ser informado o argumento \code{formula} contendo a especificação do modelo 
#' aditivo no formato compativel (veja \code{\link[mgcv]{gam}}), porem sem o LHS. Caso 
#' \code{formula} seja omitido, todas as variaveis serao utilizadas conjuntamente numa thin plate
#' regression spline com as configurações padrão do pacote, i.e. se existem as colunas
#' \code{c("V1", "V2", "V3")}, formula sera \code{~ s(V1, V2, V3)}.
#' 
#' Na execucao em janela movel, o vetor de pesos deve ser passado com o tamanho igual ao da janela.
#' O mesmo vetor sera aplicado em todas as janelas avaliadas.
#' 
#' @param serie série para ajustar
#' @param regdata \code{data.frame}-like contendo variáveis explicativas
#' @param formula opcional, fórmula da regressão. Se for omitido, todas as variaveis em 
#'     \code{regdata} serão utilizadas
#' @param tipo um de \code{c("gam", "bam")}, indicando a funcao de estimacao a ser utilizada. Veja
#'     \code{\link[mgcv]{bam}} para maiores detalhes
#' @param pesos opcional, vetor de pesos para cada observacao no ajuste. Sera normalizado 
#'     internamente
#' @param ... nao possui uso, existe apenas para consistencia com a generica
#' 
#' @return Objeto da classe \code{modprev} e subclasse \code{GAM}, uma lista de dois 
#'     elementos: \code{modelo} e \code{serie} contendo o modelo estimado e a série passada
#' 
#' @rdname modelos_gam

GAM <- function(serie, regdata, formula, fit_fun = c("gam", "bam"), pesos = rep(1, length(serie)), ...) {

    if (missing(regdata)) stop("Forneca a variavel explicativa atraves do parametro 'regdata'")

    if (missing(formula)) formula <- expandeformula(regdata, "gam")
    formula <- update(formula, Y ~ .)

    fit_fun <- match.arg(fit_fun)

    if (!is.ts(serie)) serie <- ts(serie)
    aux_tsp <- tsp(serie)

    pesos <- pesos / sum(pesos)

    regdata <- cbind(Y = as.numeric(serie), regdata)

    if (fit_fun == "gam") {
        fit <- mgcv::gam(formula, data = regdata)
    } else {
        fit <- mgcv::bam(formula, data = regdata)
    }

    mod_atrs <- list(formula = formula, tsp = aux_tsp, pesos = pesos, fit_fun = fit_fun)

    new_modprevU(fit, serie, "GAM", mod_atrs)
}

# METODOS ------------------------------------------------------------------------------------------

#' \bold{Predict}:
#' 
#' @param object objeto com classes \code{c("GAM", "modprev")} contendo modelo
#' @param newdata \code{data.frame}-like contendo variéveis explicativas fora da amostra
#' @param n.ahead número de passos à frente para previsão. Este argumento não é necessario, caso não
#'     seja informado a previsão sera feita tantos passos à frente quanto amostras em \code{newdata}
#' @param ... nao possui uso, existe apenas para consistencia com a generica
#' 
#' @return \code{predict} serie temporal multivariada contendo valor esperado e desvio padrao de
#'     previsão \code{n.ahead} passos à frente;
#' 
#' @rdname modelos_gam
#' 
#' @export

predict.GAM <- function(object, newdata, n.ahead, ...) {
    modelo <- object$modelo
    aux_tsp <- attr(object, "mod_atrs")$tsp

    if (missing(newdata)) stop("Forneca a variavel explicativa para previsao atraves do parametro 'newdata'")

    if (!missing(n.ahead)) {
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

#' @param newseries nova série com a qual atualizar o modelo
#' @param newregdata \code{data.frame}-like contendo variáveis explicativas na nova amostra
#' @param refit booleano indicando se o modelo deve ou nao ser reajustado
#' @param ... nao possui uso, existe apenas para consistencia com a generica
#' 
#' @return \code{update} retorna modelo com novos dados e, caso \code{refit == TRUE}, reajustado. 
#'     Contrário à função de estimação, \code{update} já retorna o objeto da classe \code{modprev};
#' 
#' @rdname modelos_gam
#' 
#' @export

update.GAM <- function(object, newseries, newregdata, refit = FALSE, ...) {

    mod_atrs <- attr(object, "mod_atrs")

    if (refit) {
        pesos   <- mod_atrs$pesos[seq_along(newseries)]
        fit_fun <- mod_atrs$fit_fun
        formula <- mod_atrs$formula
        object  <- estimamodelo(newseries, "GAM", regdata = newregdata, formula = formula,
            pesos = pesos, fit_fun = fit_fun)
    } else {

        # atualizar objetos gam tem um problema quando ha NAs na serie e/ou regdata. A funcao do
        # mgcv nao funciona com 'na.pass' nestes casos, dando erro por causa do NA, que precisam
        # ser retirados
        # Ele faz isso automaticamente, encurtando dataframes onde necessario, o que torna dificil
        # atualizar os regressores e outros valores por fora.
        # Como e um modelo de regressao e nao depende do dado recente, da pra so atualizar o slot
        # 'serie' para nao quebrar a previsao depois

        #if (missing(newregdata)) stop("Forneca nova variavel explicativa atraves do parametro 'newregdata'")

        #modelo$y <- as.numeric(newseries)
        #modelo$model <- cbind.data.frame(Y = as.numeric(newseries), newregdata)
        #modelo$fitted.values <- predict(modelo)
        #modelo$residuals <- as.numeric(newseries) - fitted(modelo)

        modelo <- object$modelo
        mod_atrs$tsp <- tsp(newseries)

        object <- new_modprevU(modelo, newseries, "GAM", mod_atrs)
    }

    return(object)
}
