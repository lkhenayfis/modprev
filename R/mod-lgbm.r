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
#' @param validation character string especificando o método de validação. Um de "none" (sem
#'     validação adicional), "cv" (validação cruzada), ou "split" (train/test split)
#' @param validation_control lista nomeada com argumentos específicos de validação, ou uma função
#'     que retorna tal lista. Se função, deve ter assinatura \code{function(serie, regdata)} e
#'     retornar uma lista nomeada. Para "cv": argumentos passados a
#'     \code{\link[lightgbm]{lgb.cv}} (como \code{nfold}, \code{stratified}, \code{nrounds}, etc.).
#'     Para "split": deve conter \code{oob}, um vetor lógico com TRUE para observações in-sample
#'     e FALSE para observações out-of-sample
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

LGBM <- function(serie, regdata, dataset_params = list(), train_params = list(),
    validation = c("none", "cv", "split"), validation_control = list(),  ...) {

    if (missing(regdata)) stop("Forneca a variavel explicativa atraves do parametro 'regdata'")

    validation <- match.arg(validation)

    if (!is.ts(serie)) serie <- ts(serie)
    aux_tsp <- tsp(serie)

    if (validation != "none") {
        validation_control <- evaluate_validation_control(
            validation_control, serie, regdata, validation
        )

        if (validation == "cv") {
            validate_control_lgbm_cv(validation_control, length(serie))
        } else if (validation == "split") {
            validate_control_split(validation_control, length(serie))
        }
    }

    regdata_dataset <- lgb.Dataset(data.matrix(regdata), dataset_params, label = as.numeric(serie))

    fit <- switch(validation,
        "none"  = lightgbm(regdata_dataset, train_params, ...),
        "cv"    = LGBM_CV(regdata_dataset, train_params, validation_control, ...),
        "split" = LGBM_SPLIT(serie, regdata, dataset_params, train_params, validation_control$oob, ...)
    )

    mod_atrs <- list(call = match.call(), tsp = aux_tsp)

    new_modprevU(fit, serie, "LGBM", mod_atrs)
}

LGBM_CV <- function(regdata, train_params, cv_control, ...) {

    cv_call <- c(
        list(quote(lgb.cv)),
        list(params = train_params, data = regdata),
        cv_control
    )

    cv_result <- eval(as.call(cv_call))

    best_iter <- cv_result$best_iter
    if (!is.null(best_iter)) {
        train_params$nrounds <- best_iter
    }

    fit <- lightgbm(data = regdata, params = train_params, ...)

    return(fit)
}

LGBM_SPLIT <- function(serie, regdata, dataset_params, train_params, oob, ...) {

    if (is.null(oob)) {
        stop("Para validacao 'split', validation_control deve conter 'oob'")
    }

    if (length(oob) != length(serie)) {
        stop("O vetor 'oob' deve ter o mesmo comprimento que 'serie'")
    }

    train_data <- lgb.Dataset(data.matrix(regdata[oob, , drop = FALSE]), dataset_params,
        label = as.numeric(serie[oob]))

    test_data <- lgb.Dataset.create.valid(train_data,
        data.matrix(regdata[!oob, , drop = FALSE]),
        as.numeric(serie[!oob]))

    fit <- lightgbm(train_data, train_params,
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