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
#' a partir da sazonalidade. Isto garante que os valores sejam consistentes entre si e não aumenta
#' muito o número de hiperparâmetros sendo estimados
#' 
#' A função \code{init.func} deve ser da forma 
#' \code{function(coefs, serie, regdata, formula, vardin, ...)}, em que \code{...} são demais 
#' argumentos que ela possa precisar, passados por \code{...} em \code{dcs_reg_din}. Esta funcao 
#' deve sempre retornar uma lista de dois elementos: o primeiro deve ser o vetor de valores iniciais
#' dos hiperparâmetros; o segundo um vetor indicando quais devem ser tratados como valores fixos. 
#' Por padrão é usada a função \code{\link{default_init_dcs}}.
#' 
#' @param serie série para ajustar
#' @param regdata \code{data.frame}-like contendo variáveis explicativas
#' @param formula opcional, fórmula da regressão. Se for omitido, todas as variaveis em 
#'     \code{regdata} serão utilizadas
#' @param d expoente da matrix de informação de Fisher no cálculo do score padronizado
#' @param vardin booleano indicando se deve ser estimado modelo com heterocedasticidade.
#' @param init.func uma funcao que retorne inicializacao dos hiperparametros. Ver Detalhes
#' @param ... argumentos extras passados para \code{init.func}
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

    start_fixed <- init.func(coef(mod), serie, regdata, formula, vardin, ...)

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

#' Inicializacao Padrao De \code{dcs_reg_din}
#' 
#' Funcao default para inicializacao dos hiperparametros em modelos \code{dcs_reg_din}
#' 
#' A funcao de inicializacao padrao implementa uma heuristica relativamente simples para 
#' inicializacao dos hiperparametros. A partir das \code{num.obs} primeiras observacoes da serie e
#' variaveis explicativas, estima uma regressao linear simples com a mesma especificacao da que sera
#' modelada dinamicamente e usa estes coeficientes como valores iniciais para a otimizacao dos 
#' hiperparametros em \code{dcs_reg_din}. Estes valores sao entao removidos da otimizacao, isto e,
#' sao tratados como fixos (geralmente nao e um problema, ainda mais com series mais longas).
#' 
#' Se \code{vardin = TRUE}, tambem coloca os scores dos ciclos de variancia como valores fixos em 0,
#' gerando assim heterocedasticidade deterministica.
#' 
#' @param coefs coeficientes do modelo como retornado por \code{\link[DCS]{coef.DCSmodel}}
#' @param serie série para ajustar
#' @param regdata \code{data.frame}-like contendo variáveis explicativas
#' @param formula opcional, fórmula da regressão. Se for omitido, todas as variaveis em 
#'     \code{regdata} serão utilizadas
#' @param vardin booleano indicando se deve ser estimado modelo com heterocedasticidade.
#' @param num.obs numero de observacoes iniciais para usar na heuristica. Se for menor do que 1, se
#'     assume que e um percentual do tamanho da serie
#' 
#' @return lista de dois elementos: valores iniciais para otimizacao e vetor de hiperparametros 
#'     fixos

default_init_dcs <- function(coefs, serie, regdata, formula, vardin, num.obs = .2) {

    if(num.obs < 1) num.obs <- ceiling(num.obs * length(serie))

    start <- coefs
    start[] <- 0

    lmini <- lm(update(formula, Y ~ .), cbind.data.frame(Y = serie, regdata)[seq_len(num.obs), ])

    start["mu_reg.*init"] <- coef(lmini)
    fixed <- grep("mu_reg.*init", names(start))

    if(vardin) {
        fixed <- c(fixed, grep("cycle.*score", names(start)))
    }

    out <- list(start, fixed)

    return(out)
}

# METODOS ------------------------------------------------------------------------------------------

#' \bold{Predict}:
#' 
#' A previsão destes modelos é feita por simulação, ou seja, é representada pela média de \code{S}
#' cenários simulados \code{n.ahead} passos à frente
#' 
#' @param object objeto com classes \code{c("dcs_reg_din", "modprev")} contendo modelo
#' @param newdata \code{data.frame}-like contendo variéveis explicativas fora da amostra
#' @param n.ahead número de passos à frente para previsão. Este argumento não é necessario, caso não
#'     seja informado a previsão sera feita tantos passos à frente quanto amostras em \code{newdata}
#' @param S inteiro indicando o numero de simulações a serem realizadas
#' @param ... existe apenas para consistência com a genérica.
#' 
#' @return \code{predict} serie temporal multivariada contendo valor esperado e desvio padrao de
#'     previsão \code{n.ahead} passos à frente;
#' 
#' @rdname modelos_dcs_reg_din
#' 
#' @export

predict.dcs_reg_din <- function(object, newdata, n.ahead, S = 5000, ...) {
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

        modelo <- update(modelo, newseries, newregdata, FALSE)
        object <- new_modprevU(modelo, newseries, "dcs_reg_din", mod_atrs)
    }

    return(object)
}
