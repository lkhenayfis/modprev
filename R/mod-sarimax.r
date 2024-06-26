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
#' @param ... Para estimacao, demais argumentos que possam ser passados a
#'     \code{\link[forecast]{auto.arima}} -- Ver Detalhes; Para \code{predict}, demais argumentos 
#'     passados a \code{\link[stats]{predict.Arima}}, exceto por \code{n.ahead} que ja vai 
#'     automaticamente; para \code{update} nao tem uso, existe apenas para consistência com a
#'     generica
#' 
#' 
#' @name modelos_sarimax
NULL

# ESTIMACAO ----------------------------------------------------------------------------------------

#' \bold{Estimacao:}
#' 
#' \code{...} pode conter qualquer, ou nenhum, argumento de \code{\link[forecast]{auto.arima}} 
#' exceto por \code{y}, que sera passado automaticamente como \code{serie}, e \code{xreg} que e 
#' montado internamente pelo estimador. Alem disso, por padrao \code{allowdrift = FALSE} dado que em
#' geral as series com as quais \code{modprev} lida nao possuem tendencias lineares no tempo.
#' 
#' Paralelamente, se \code{order} ou \code{seasonal} forem passados, sera estimado um modelo com 
#' estas ordens especificas. Observe que a selecao automatica nao funciona para so uma das partes,
#' isto e, se um for especificado, nao e possivel fazer uma pesquisa para as melhores ordens da
#' outra parte.
#' 
#' @param serie serie para ajustar
#' @param regdata \code{data.frame}-like contendo variáveis explicativas
#' @param formula formula da regressão. Se for omitido, todas as variaveis em \code{regdata} serão
#'     utilizadas. So tem uso se \code{regdata} for passado
#' @param ... Para estimacao, argumentos que possam ser passados a \code{\link[forecast]{Arima}} ou
#'     \code{\link[forecast]{auto.arima}} -- Ver Detalhes
#' 
#' @return Objeto da classe \code{modprev} e subclasse \code{sarimax}, uma lista de dois elementos:
#'     \code{modelo} e \code{serie} contendo o modelo estimado e a série passada
#' 
#' @rdname modelos_sarimax

sarimax <- function(serie, regdata, formula = expandeformula(regdata, "ls"), ...) {

    if (missing(regdata)) stop("Forneca a variavel explicativa atraves do parametro 'regdata'")

    Xreg <- expandexreg(regdata, formula)

    args <- list(...)
    args$xreg <- Xreg

    # caso seja passado 'order' ou 'seasonal', estima direto sem parte automatica
    # atualmente, pelo auto.arima nao da pra fixar uma parte ou outra e pesquisar o resto, de modo
    # que ou estima um modelo cravado ou deixa a pesquisa ser completa
    not_auto <- any(c("order", "seasonal") %in% names(args))
    fitfunc <- ifelse(not_auto, Arima, auto.arima)

    if (!not_auto && !("allowdrift" %in% names(args))) args$allowdrift <- FALSE

    mc <- as.call(c(list(fitfunc, substitute(serie)), args))
    mod <- eval(mc, envir = parent.frame())

    mod_atrs <- list(formula = formula)
    out <- new_modprevU(mod, serie, "sarimax", mod_atrs)

    return(out)
}

# METODOS ------------------------------------------------------------------------------------------

#' @param object objeto com classes \code{c("sarimax", "modprev")} contendo modelo
#' @param newdata \code{data.frame}-like contendo variéveis explicativas fora da amostra
#' @param n.ahead número de passos à frente para previsão. Este argumento não é necessario, caso não
#'     seja informado a previsão sera feita tantos passos à frente quanto amostras em \code{newdata}
#' @param ... Para \code{predict}, demais argumentos passados a \code{\link[stats]{predict.Arima}}, 
#'     exceto por \code{n.ahead} que ja vai automaticamente
#' 
#' @return \code{predict} serie temporal multivariada contendo valor esperado e desvio padrao de
#'     previsão \code{n.ahead} passos à frente;
#' 
#' @rdname modelos_sarimax
#' 
#' @export

predict.sarimax <- function(object, newdata, n.ahead, ...) {
    modelo <- object$modelo

    if (missing(newdata)) stop("Forneca a variavel explicativa para previsao atraves do parametro 'newdata'")

    if (!missing(n.ahead)) {
        regobs <- min(n.ahead, nrow(newdata))
        newdata <- newdata[seq(regobs), , drop = FALSE]
    }

    formula <- attr(object, "mod_atrs")$formula
    Xreg <- expandexreg(newdata, formula)

    prev   <- forecast(modelo, xreg = Xreg, level = c(.95), ...)
    prevsd <- with(prev, mean - lower) / qnorm(.975)

    prev <- cbind(prev$mean, prevsd)
    colnames(prev) <- c("prev", "sd")

    return(prev)
}

#' 
#' @param newseries nova série com a qual atualizar o modelo
#' @param newregdata \code{data.frame}-like contendo variáveis explicativas na nova amostra
#' @param refit booleano indicando se o modelo deve ou nao ser reajustado
#' @param ... para \code{update} nao tem uso, existe apenas para consistência com a genérica
#' 
#' @return \code{update} retorna modelo com novos dados e, caso \code{refit == TRUE}, reajustado. 
#'     Contrário à função de estimação, \code{update} já retorna o objeto da classe \code{modprev};
#' 
#' @rdname modelos_sarimax
#' 
#' @export

update.sarimax <- function(object, newseries, newregdata, refit = FALSE, ...) {

    mod_atrs <- attr(object, "mod_atrs")

    if (refit) {
        formula <- mod_atrs$formula
        object <- estimamodelo(newseries, "sarimax", regdata = newregdata, formula = formula)
    } else {

        if (missing(newregdata)) stop("Forneca nova variavel explicativa atraves do parametro 'newregdata'")

        formula <- mod_atrs$formula
        Xreg <- expandexreg(newregdata, formula)

        newseries <- if (is.ts(newseries)) newseries else ts(newseries)
        modelo <- Arima(newseries, xreg = Xreg, model = object$modelo)

        object <- new_modprevU(modelo, newseries, "sarimax", mod_atrs)
    }

    return(object)
}

# HELPERS ------------------------------------------------------------------------------------------

expandexreg <- function(data, formula) {
    formula <- formula
    Xreg <- model.frame(formula, data = data)
    Xreg <- data.matrix(Xreg)

    return(Xreg)
}