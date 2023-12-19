####################################################################################################
# MODELO BOOST DE REGRESSOES
####################################################################################################

#' Modelos \code{BOOST}
#' 
#' Estimação e métodos de modelos da classe \code{BOOST}
#' 
#' Boosting de modelos, estimados atraves da funcao \code{\link[mboost]{mboost}}. Para mais
#' detalhes a respeito desta modelagem e seu uso, veja a documentacao oficial do pacote.
#' 
#' @name modelos_boost
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
#' Opcionalmente pode ser informado o argumento \code{formula} contendo a especificação do modelo 
#' aditivo no formato compativel (veja \code{\link[mboost]{mboost}}), porem sem o LHS. Caso 
#' \code{formula} seja omitido, todas as variaveis serao utilizadas.
#' 
#' Na execucao em janela movel, o vetor de pesos deve ser passado com o tamanho igual ao da janela.
#' O mesmo vetor sera aplicado em todas as janelas avaliadas.
#' 
#' @param serie série para ajustar
#' @param regdata \code{data.frame}-like contendo variáveis explicativas
#' @param formula opcional, fórmula da regressão. Se for omitido, todas as variaveis em 
#'     \code{regdata} serão utilizadas
#' @param family string indicando uma familia para funcao perda. Veja \code{\link[mboost]{mboost}}
#'     para mais detalhes
#' @param cv_control uma lista nomeada contendo quaisquer argumentos 
#' @param ... nao possui uso, existe apenas para consistencia com a generica
#' 
#' @return Objeto da classe \code{modprev} e subclasse \code{BOOST}, uma lista de dois 
#'     elementos: \code{modelo} e \code{serie} contendo o modelo estimado e a série passada
#' 
#' @rdname modelos_boost

BOOST <- function(serie, regdata, formula, family = "Gaussian", cv_control = list(), ...) {

    # algumas familias tem chamadas de 'risk' e 'loss' inacreditavelmente porcas, que envolvem
    # avaliacao de nomes em outros environments, nao controlados, que sao atualizados por fora
    # Isso cria um problema enorme na execucao em janela movel, pois pode ter efeitos colaterais
    # de modificacoes numa janela anterior na janela atual
    # Passando esse parametro como string e avaliando toda vez evita esse problema
    # por outro lado, e ruim ter que ficar expondo certos argumentos de 'mboost' pela wrapper
    family <- eval(parse(text = paste0("mboost:::", family, "()")))

    if (missing(regdata)) stop("Forneca a variavel explicativa atraves do parametro 'regdata'")

    if (missing(formula)) formula <- Y ~ . else formula <- update(formula, Y ~ .)

    if (!is.ts(serie)) serie <- ts(serie)
    aux_tsp <- tsp(serie)

    regdata <- cbind(Y = as.numeric(serie), regdata)
    fit <- mboost(formula, data = regdata, family = family, ...)

    cv_spec <- c(list(quote(cv), model.weights(fit)), match_fun_args(cv_control, mboost::cv))
    cv_spec <- eval(as.call(cv_spec))

    args_cvrisk  <- match_fun_args(cv_control, mboost:::cvrisk.mboost)
    args_mcapply <- match_fun_args(cv_control, parallel::mclapply)
    cv <- c(
        list(quote(cvrisk), quote(fit), quote(cv_spec)),
        args_cvrisk,
        args_mcapply[!grepl("mc\\.preschedule", names(args_mcapply))]
    )
    cv <- eval(as.call(cv))
    fit <- fit[mstop(cv)]

    mod_atrs <- list(call = match.call(), tsp = aux_tsp)

    new_modprevU(fit, serie, "BOOST", mod_atrs)
}

# METODOS ------------------------------------------------------------------------------------------

#' \bold{Predict}:
#' 
#' @param object objeto com classes \code{c("BOOST", "modprev")} contendo modelo
#' @param newdata \code{data.frame}-like contendo variéveis explicativas fora da amostra
#' @param n.ahead número de passos à frente para previsão. Este argumento não é necessario, caso não
#'     seja informado a previsão sera feita tantos passos à frente quanto amostras em \code{newdata}
#' @param ... nao possui uso, existe apenas para consistencia com a generica
#' 
#' @return \code{predict} serie temporal multivariada contendo valor esperado e desvio padrao de
#'     previsão \code{n.ahead} passos à frente;
#' 
#' @rdname modelos_boost
#' 
#' @export

predict.BOOST <- function(object, newdata, n.ahead, ...) {
    modelo <- object$modelo
    aux_tsp <- attr(object, "mod_atrs")$tsp

    if (missing(newdata)) stop("Forneca a variavel explicativa para previsao atraves do parametro 'newdata'")

    if (!missing(n.ahead)) {
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

#' @param newseries nova série com a qual atualizar o modelo
#' @param newregdata \code{data.frame}-like contendo variáveis explicativas na nova amostra
#' @param refit booleano indicando se o modelo deve ou nao ser reajustado
#' @param ... nao possui uso, existe apenas para consistencia com a generica
#' 
#' @return \code{update} retorna modelo com novos dados e, caso \code{refit == TRUE}, reajustado. 
#'     Contrário à função de estimação, \code{update} já retorna o objeto da classe \code{modprev};
#' 
#' @rdname modelos_boost
#' 
#' @export

update.BOOST <- function(object, newseries, newregdata, refit = FALSE, ...) {

    mod_atrs <- attr(object, "mod_atrs")

    if (refit) {
        envir <- mod_atrs$envir
        call  <- mod_atrs$call
        call$serie   <- newseries
        call$regdata <- newregdata
        object <- eval(call, parent.frame())
    } else {

        # manipular os objetos 'mboost' e extremamente complicado dado sua complexidade. Por enquanto
        # o update sem refit vai deixar esse cara parado como era, dado que isso nao afeta em nada a
        # previsao (que precisa receber 'newdata' de qualquer forma), atualizando apenas a serie
        # do objeto 'modprev' pois isso e importante na hora de montar a ts de previsao
        modelo <- object$modelo
        mod_atrs$tsp <- tsp(newseries)

        object <- new_modprevU(modelo, newseries, "BOOST", mod_atrs)
    }

    return(object)
}
