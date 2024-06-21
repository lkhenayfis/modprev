####################################################################################################
# MODELO LGBM
####################################################################################################

#' Modelos \code{LGBM}
#' 
#' Estimação e métodos de modelos da classe \code{LGBM}
#' 
#' Boosting de árvores pelo algoritmo lgbm estimado atraves de \code{\link[lightgbm]{lgbm}}. Para 
#' mais detalhes a respeito desta modelagem e seu uso, veja a documentacao oficial do pacote.
#' 
#' @name light_gbm
#' 
#' @import mboost
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
#' @rdname light_gbm

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
