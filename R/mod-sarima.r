####################################################################################################
# MODELO SARIMA
####################################################################################################

#' Modelos \code{sarima}
#' 
#' Estimação e métodos de modelos da classe \code{sarima}
#' 
#' Esta especificação identifica e ajusta um modelo SARIMA(p, d, q)(P, D, Q)^s através da função
#' \code{\link[forecast]{auto.arima}}, que realiza uma busca no espaço de hiperparâmetros de ordem
#' do modelo por aquele conjunto com menor BIC.
#' 
#' @param ... Para estimacao, demais argumentos que possam ser passados a
#'     \code{\link[forecast]{auto.arima}} -- Ver Detalhes; Para \code{predict}, demais argumentos 
#'     passados a \code{\link[stats]{predict.Arima}}, exceto por \code{n.ahead} que ja vai 
#'     automaticamente; para \code{update} nao tem uso, existe apenas para consistência com a
#'     generica
#' 
#' @name modelos_sarima
NULL

# ESTIMACAO ----------------------------------------------------------------------------------------

#' \bold{Estimacao:}
#' 
#' \code{...} pode conter qualquer, ou nenhum, argumento de \code{\link[forecast]{auto.arima}} 
#' exceto por \code{y}, que sera passado automaticamente como \code{serie}. Alem disso, por padrao
#' \code{allowdrift = FALSE} dado que em geral as series com as quais \code{modprev} lida nao 
#' possuem tendencias lineares no tempo.
#' 
#' Paralelamente, se \code{order} ou \code{seasonal} forem passados, sera estimado um modelo com 
#' estas ordens especificas. Observe que a selecao automatica nao funciona para so uma das partes,
#' isto e, se um for especificado, nao e possivel fazer uma pesquisa para as melhores ordens da
#' outra parte.
#' 
#' @param serie serie para ajustar
#' @param regdata opcional, \code{data.frame}-like contendo variáveis explicativas
#' @param formula opcional, fórmula da regressão. Se for omitido, todas as variaveis em 
#'     \code{regdata} serão utilizadas. So tem uso se \code{regdata} for passado
#' @param ... Para estimacao, argumentos que possam ser passados a \code{\link[forecast]{Arima}} ou
#'     \code{\link[forecast]{auto.arima}} -- Ver Detalhes
#' 
#' @return Objeto da classe \code{modprev} e subclasse \code{sarima}, uma lista de dois elementos:
#'     \code{modelo} e \code{serie} contendo o modelo estimado e a série passada
#' 
#' @rdname modelos_sarima

sarima <- function(serie, regdata, formula = expandeformula(regdata, "ls"), ...) {

    # o is.null e necessario para garantir que nao entre no sarimax errado durante janelamovel
    if (!missing(regdata) && !is.null(regdata)) return(sarimax(serie, regdata, formula, ...))

    args <- list(...)

    # caso seja passado 'order' ou 'seasonal', estima direto sem parte automatica
    # atualmente, pelo auto.arima nao da pra fixar uma parte ou outra e pesquisar o resto, de modo
    # que ou estima um modelo cravado ou deixa a pesquisa ser completa
    not_auto <- any(c("order", "seasonal") %in% names(args))
    fitfunc <- ifelse(not_auto, Arima, auto.arima)

    if (!not_auto && !("allowdrift" %in% names(args))) args$allowdrift <- FALSE

    mc <- as.call(c(list(fitfunc, substitute(serie)), args))
    mod <- eval(mc, envir = parent.frame())
    out <- new_modprevU(mod, serie, "sarima")
    return(out)
}

# METODOS ------------------------------------------------------------------------------------------

#' @param object objeto com classes \code{c("sarima", "modprev")} contendo modelo
#' @param n.ahead número de passos à frente para previsão
#' @param ... Para \code{predict}, demais argumentos passados a \code{\link[stats]{predict.Arima}}, 
#'     exceto por \code{n.ahead} que ja vai automaticamente
#' 
#' @return \code{predict} retorna uma série temporal multivariada contendo valor esperado e desvio 
#'     padrão da previsão \code{n.ahead} passos à frente;
#' 
#' @rdname modelos_sarima
#' 
#' @export

predict.sarima <- function(object, n.ahead, ...) {
    modelo <- object$modelo

    prev <- predict(modelo, n.ahead = n.ahead, ...)
    prev <- do.call(cbind, prev)
    colnames(prev) <- c("prev", "sd")

    return(prev)
}

#' @param newseries nova série com a qual atualizar o modelo
#' @param refit booleano indicando se o modelo deve ou não ser reajustado
#' @param ... para \code{update} nao tem uso, existe apenas para consistência com a genérica
#' 
#' @return \code{update} retorna modelo com novos dados e, caso \code{refit == TRUE}, reajustado. 
#'     Contrário à função de estimação, \code{update} já retorna o objeto da classe \code{modprev};
#' 
#' @rdname modelos_sarima
#' 
#' @export

update.sarima <- function(object, newseries, refit = FALSE, ...) {
    if (refit) {
        object <- estimamodelo(newseries, "sarima")
    } else {
        newseries <- if (is.ts(newseries)) newseries else ts(newseries)
        modelo <- Arima(newseries, model = object$modelo)

        object <- new_modprevU(modelo, newseries, "sarima")
    }

    return(object)
}