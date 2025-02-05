####################################################################################################
# MODELO LGBM
####################################################################################################

#' Modelos \code{LGBM}
#' 
#' Estimação e métodos de modelos da classe \code{LGBM}
#' 
#' Boosting de árvores pelo algoritmo lgbm estimado atraves de \code{\link[lightgbm]{lightgbm}}.  
#' Para mais detalhes a respeito desta modelagem e seu uso, veja a documentacao oficial do pacote.
#' 
#' @name modelos_lightgbm
#' 
#' @import lightgbm
NULL

# ESTIMACAO ----------------------------------------------------------------------------------------

#' \bold{Estimação}:
#' 
#' Para estimação deste tipo de modelo, além de ser fornecido o arumento \code{serie} também deve 
#' ser necessariamente informado \code{regdata}, um \code{data.frame}-like contendo as variáveis 
#' explicativas necessárias. 
#' 
#' Ao contrario da maioria dos modelos com variáveis explicativas neste pacote, não há possibilidade
#' de passar argumentos \code{formula} para estimação de lightGBMs. Isto se deve ao fato de que o
#' modelo cresce árvores em todas as variáveis que lhe são informadas. Caso o usuário deseje reduzir
#' regressores, deve fazê-lo de antemão.
#' 
#' Na execucao em janela movel, o vetor de pesos deve ser passado com o tamanho igual ao da janela.
#' O mesmo vetor sera aplicado em todas as janelas avaliadas.
#' 
#' @param serie série para ajustar
#' @param regdata \code{data.frame}-like contendo variáveis explicativas
#' @param test_data uma lista contento valores out-of-sample da variavel dependente e regressores
#'     que serao utilizados para selecao do criterio de parada
#' @param dataset_params lista de parâmetros opcionais para construção do dataset. Veja
#'     \code{\link[lightgbm]{lgb.Dataset}} para mais detalhes
#' @param train_params lista de parâmetros opcionais para treinamento do modelo. Veja
#'     \code{\link[lightgbm]{lightgbm}} para mais detalhes
#' @param ... para estimacao, demais argumentos passados a funcao \code{\link[lightgbm]{lightgbm}}; 
#'     nas restantes nao possui uso
#' 
#' @return Objeto da classe \code{modprev} e subclasse \code{LGBM}, uma lista de dois 
#'     elementos: \code{modelo} e \code{serie} contendo o modelo estimado e a série passada
#' 
#' @rdname modelos_lightgbm

LGBM <- function(serie, regdata, test_data = list(c(), data.frame()),
    dataset_params = list(), train_params = list(), ...) {

    if (missing(regdata)) stop("Forneca a variavel explicativa atraves do parametro 'regdata'")

    if (!is.ts(serie)) serie <- ts(serie)
    aux_tsp <- tsp(serie)

    regdata <- lgb.Dataset(data.matrix(regdata), dataset_params, label = as.numeric(serie))

    if (length(test_data[[1]]) != 0) {
        fit <- LGBM_traintest(regdata, test_data, train_params, ...)
    } else {
        fit <- lightgbm(regdata, train_params, ...)
    }

    mod_atrs <- list(call = match.call(), tsp = aux_tsp)

    new_modprevU(fit, serie, "LGBM", mod_atrs)
}

LGBM_traintest <- function(regdata, test_data, train_params, ...) {

    test_data <- lgb.Dataset.create.valid(regdata, data.matrix(test_data[[2]]),
        as.numeric(test_data[[1]]))

    fit <- lightgbm(regdata, train_params,
        valids = list(test = test_data), ...)

    return(fit)
}

# METODOS ------------------------------------------------------------------------------------------

#' \bold{Predict}:
#' 
#' @param object objeto com classes \code{c("LGBM", "modprev")} contendo modelo
#' @param newdata \code{data.frame}-like contendo variéveis explicativas fora da amostra
#' @param n.ahead número de passos à frente para previsão. Este argumento não é necessario, caso não
#'     seja informado a previsão sera feita tantos passos à frente quanto amostras em \code{newdata}
#' 
#' @return \code{predict} serie temporal multivariada contendo valor esperado e desvio padrao de
#'     previsão \code{n.ahead} passos à frente;
#' 
#' @rdname modelos_lgbm
#' 
#' @export

predict.LGBM <- function(object, newdata, n.ahead, ...) {
    modelo <- object$modelo
    aux_tsp <- attr(object, "mod_atrs")$tsp

    if (missing(newdata)) stop("Forneca a variavel explicativa para previsao atraves do parametro 'newdata'")

    if (!missing(n.ahead)) {
        regobs <- min(n.ahead, nrow(newdata))
        newdata <- newdata[seq(regobs), , drop = FALSE]
    }

    prev <- predict(modelo, newdata = data.matrix(newdata))
    prev <- cbind(prev, rep(NA_real_, length(prev)))
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
#' @rdname modelos_lgbm
#' 
#' @export

update.LGBM <- function(object, newseries, newregdata, refit = FALSE, ...) {

    mod_atrs <- attr(object, "mod_atrs")

    if (refit) {
        call  <- mod_atrs$call
        call$serie   <- newseries
        call$regdata <- newregdata
        object <- eval(call, parent.frame())
    } else {

        modelo <- object$modelo
        mod_atrs$tsp <- tsp(newseries)

        object <- new_modprevU(modelo, newseries, "LGBM", mod_atrs)
    }

    return(object)
}