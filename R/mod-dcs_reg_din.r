####################################################################################################
# MODELO REGRESSAO DINAMICA SIMPLES
####################################################################################################

#' Modelos \code{dcs_reg_din}
#' 
#' Estimação e métodos de modelos da classe \code{dcs_reg_din}
#' 
#' @name modelos_dcs_reg_din
NULL

# ESTIMACAO ----------------------------------------------------------------------------------------

#' \bold{Estimação}:
#' 
#' Os modelos de regressão dinâmica estão entre os mais complexos deste pacote. Para sua estimação,
#' além de ser fornecido o arumento \code{serie} também deve ser necessariamente informado 
#' \code{regdata}, um \code{data.frame}-like contendo as variáveis explicativas necessárias. 
#' 
#' Opcionalmente pode ser informado o argumento \code{formula} contendo a especificação da regressão
#' linear, no formato padrão do R (veja \code{\link{formula}}) porém sem o LHS. Caso \code{formula} 
#' seja omitido, todas as variáveis em \code{regdata} serão utilizadas aditivamemte, i.e. se existem
#' as colunas \code{c("V1", "V2", "V3")}, formula sera \code{~ V1 + V2 + V3}.
#' 
#' \code{vardin} controla se o modelo será estimado com variâncias variantes no tempo ou não. Caso
#' seja \code{FALSE} um modelo homocedástico é estimado; \code{TRUE} tenta buscar sazonalidade nos
#' atributos de \code{serie} para heterocedasticidade e, por fim, se \code{vardin} for um inteiro
#' utiliza este número como sazonalidade.
#' 
#' As variâncias variantes no tempo são modeladas como função de variáveis circulares determinadas
#' a partir da sazonalidade. Isto garante que os valores sejam consistentes entre se e, mais 
#' importante, só possui um parâmetro a mais em relação aos modelos homocedásticos.
#' 
#' O argumento \code{lambda} permite que seja introduzida uma penalidade na estimacao do modelo. O 
#' valor passado por este argumento ser multiplicado pelo traco da matriz Q e somado a funcao 
#' objetivo. Isso permite que a regressao dinamica seja controlada para apresentar comportamento 
#' menos adaptativo. A funcao \code{\link{CV_regdin}} permite otimizar o coeficiente de penalidade
#' por validacao cruzada.
#' 
#' @param serie série para ajustar
#' @param regdata \code{data.frame}-like contendo variáveis explicativas
#' @param formula opcional, fórmula da regressão. Se for omitido, todas as variaveis em 
#'     \code{regdata} serão utilizadas
#' @param ... nao possui uso, existe apenas para consistencia com a generica
#' 
#' @return Objeto da classe \code{modprev} e subclasse \code{dcs_reg_din}, uma lista de dois 
#'     elementos: \code{modelo} e \code{serie} contendo o modelo estimado e a série passada
#' 
#' @rdname modelos_dcs_reg_din

dcs_reg_din <- function(serie, regdata, formula, d = 1, vardin = FALSE, init.func = default_init_dcs,
    ...) {

    if(missing(regdata)) stop("Forneca a variavel explicativa atraves do parametro 'regdata'")

    if(any(is.na(regdata))) {
        warning("Ha NAs na variavel explicativa")
        linna <- rowSums(is.na(regdata)) > 0
        serie[linna] <- NA_real_
        regdata[linna, ] <- 0
    }

    if(missing(formula)) formula <- expandeformula(regdata)

    spec <- list(mu ~ DCSreg(formula, regdata, TRUE))
    if(vardin) {
        spec <- c(spec, list(sigma2 ~ DCScycle(frequency(serie))))
    }

    mod <- DCSmodel(serie, "t", spec, d)

    start_fixed <- init.func(coef(mod), serie, regdata, formula, vardin)

    if(is.list(start_fixed)) {
        start <- start_fixed[[1]]
        fixed <- start_fixed[[2]]
    } else {
        start <- start_fixed
        fixed <- rep(FALSE, length(start))
    }

    fit <- DCSfit(mod, start, fixed)

    mod_atrs <- list(formula = formula, vardin = vardin)
    out <- new_modprevU(fit$model, serie, "dcs_reg_din", mod_atrs)

    return(out)
}

default_init_dcs <- function(coefs, serie, regdata, formula, vardin) {

    start <- coefs
    start[] <- 0

    if(vardin) {
        fixed <- grep("cycle.*score", names(start))
    } else {
        fixed <- rep(FALSE, length(start))
    }

    out <- list(start, fixed)

    return(out)
}

# METODOS ------------------------------------------------------------------------------------------

#' \bold{Predict}:
#' 
#' A previsão destes modelos é feita com alguns argumetnos opcionais já passados por padrão que
#' não podem ser modificados. Estes são: \code{se.fit = TRUE, filter = TRUE}. O primeiro retorna
#' além do valor previsto o desvio padrão associado, o segundo garante que são retornados os valores
#' advindos da distribuição preditiva e não de suavização. Considerando estes fatores, \code{...} 
#' não pode conter \code{n.ahead} ou estes dois outros, ou então ocorrerá erro.
#' 
#' @param object objeto com classes \code{c("dcs_reg_din", "modprev")} contendo modelo
#' @param newdata \code{data.frame}-like contendo variéveis explicativas fora da amostra
#' @param n.ahead número de passos à frente para previsão. Este argumento não é necessario, caso não
#'     seja informado a previsão sera feita tantos passos à frente quanto amostras em \code{newdata}
#' @param ... demais argumentos passados a \link[KFAS]{\code{predict.SSModel}}
#' 
#' @return \code{predict} serie temporal multivariada contendo valor esperado e desvio padrao de
#'     previsão \code{n.ahead} passos à frente;
#' 
#' @rdname modelos_dcs_reg_din
#' 
#' @export

predict.dcs_reg_din <- function(object, newdata, n.ahead, S = 1000, ...) {
    modelo <- object$modelo

    if(missing(newdata)) stop("Forneca a variavel explicativa para previsao atraves do parametro 'newdata'")

    if(!missing(n.ahead)) {
        regobs <- min(n.ahead, nrow(newdata))
        newdata <- newdata[seq(regobs), , drop = FALSE]
    }

    prev <- predict(modelo, n.ahead = nrow(newdata), newdata, S, FALSE, ...)
    prev <- cbind(prev = prev, sd = rep(NA_real_, length(prev)))

    return(prev)
}

#' @details 
#' 
#' \bold{Update}:
#' 
#' A atualização de modelos \code{dcs_reg_din} sempre vai checar se o modelo passado foi estimado
#' corretamente. Como modelos em espaçoo de estados dependem bastante de inicialização, às vezes não
#' dá para estimar direito. Nesses casos ele tenta reestimar o modelo independentemente de 
#' \code{refit}
#' 
#' @param newseries nova série com a qual atualizar o modelo
#' @param newregdata \code{data.frame}-like contendo variáveis explicativas na nova amostra
#' @param refit booleano indicando se o modelo deve ou nao ser reajustado
#' @param ... nao possui uso, existe apenas para consistencia com a generica
#' 
#' @return \code{update} retorna modelo com novos dados e, caso \code{refit == TRUE}, reajustado. 
#'     Contrário à função de estimação, \code{update} já retorna o objeto da classe \code{modprev};
#' 
#' @rdname modelos_dcs_reg_din
#' 
#' @export

update.dcs_reg_din <- function(object, newseries, newregdata, refit = FALSE, ...) {

    mod_atrs <- attr(object, "mod_atrs")

    if(refit) {
        formula <- mod_atrs$formula
        vardin  <- mod_atrs$vardin
        object  <- estimamodelo(newseries, "dcs_reg_din", regdata = newregdata, formula = formula,
            vardin = vardin)
    } else {

        modelo <- object$modelo

        if(missing(newregdata)) stop("Forneca nova variavel explicativa atraves do parametro 'newregdata'")

        modelo <- update(modelo, newseries, newregdata, FALSE, TRUE)
        object <- new_modprevU(modelo, newseries, "dcs_reg_din", mod_atrs)
    }

    return(object)
}
