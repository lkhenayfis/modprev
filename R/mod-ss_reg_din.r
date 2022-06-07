####################################################################################################
# MODELO REGRESSAO DINAMICA SIMPLES
####################################################################################################

#' Modelos \code{ss_reg_din}
#' 
#' Estimacao e metodos de modelos da classe \code{ss_reg_din}
#' 
#' @name modelos_ss_reg_din
NULL

# ESTIMACAO ----------------------------------------------------------------------------------------

#' @param serie serie para ajustar
#' @param regdata vetor, matriz ou data.frame contendo variaveis explicativas
#' @param formula opcional, formula da regressao. Se for omitido, todas as variaveis em 
#'     \code{regdata} serao utilizadas
#' @param vardin booleano ou inteiro indicando se deve ser estimado modelo com heterocedasticidade.
#'     Caso \code{TRUE} tenta pegar a sazonalidade da serie, se for um numero inteiro assume este
#'     valor como a sazonalidade
#' 
#' @return \code{ss_reg_din} retorna modelo de regressao dinamica simples
#' 
#' @rdname modelos_ss_reg_din

ss_reg_din <- function(serie, regdata, formula, vardin = FALSE) {

    if(missing(regdata)) stop("Forneca a variavel explicativa atraves do parametro regdata")

    if(missing(formula) || is.null(formula)) {
        formula <- colnames(regdata)
        if(length(formula) > 1) formula <- paste0(colnames(regdata), collapse = " + ")
        formula <- paste0("~ ", formula)
        formula <- as.formula(formula)
    }

    nvars  <- ncol(model.matrix(formula, data = regdata)) - 1 # model.matrix inclui intercept

    if(vardin & (frequency(serie) == 1)) warning("'vardin' e TRUE mas 'serie' nao possui sazonalidade")
    vardin <- vardin * 1

    if(vardin != 0) {

        saz <- frequency(serie) * (vardin == 1) + vardin * (vardin > 1)
        upfunc <- function(par, mod, ...) {
            parH <- par[1:2]
            uH   <- cos(0:(saz - 1) * 2 * pi / saz)
            vH   <- sin(0:(saz - 1) * 2 * pi / saz)
            mod["H"][] <- rep(exp(parH[1] * uH + parH[2] * vH), length.out = dim(mod["H"])[3])

            parQ <- par[-c(1:2)]
            for(i in seq_along(parQ)) mod["Q"][i, i, 1] <- exp(parQ[i])

            return(mod)
        }

    } else {

        saz <- 1
        upfunc <- function(par, mod, ...) {
            mod["H"][1, 1, 1] <- exp(par[1])

            parQ <- par[-1]
            for(i in seq_along(parQ)) mod["Q"][i, i, 1] <- exp(parQ[i])

            return(mod)
        }
    }

    mod <- SSModel(serie ~ SSMregression(formula, regdata, Q = diag(NA_real_, nvars)),
            H = array(NA_real_, c(1, 1, saz)))
    fit <- fitSSM(mod, rep(0, nvars + 1 + (vardin != 0)), upfunc, method = "BFGS")

    if(fit$optim.out$convergence < 0) {
        fit$model$Z[] <- NA
    }

    attr(fit$model, "formula") <- formula
    attr(fit$model, "vardin")  <- vardin
    attr(fit$model, "saz")     <- saz

    return(fit$model)
}

# METODOS ------------------------------------------------------------------------------------------

#' @param object objeto com classes \code{c("ss_reg_din", "mod_eol")} contendo modelo
#' @param newdata vetor, matriz ou data.frame contendo variaveis explicativas fora da amostra
#' @param n.ahead numero de passos a frente para previsao. Este argumento nao e necessario, caso nao
#'     seja informado a previsao sera feita tantos passos a frente quanto amostras em \code{newdata}
#' @param ... demais argumentos passados a \link[KFAS]{\code{predict.SSModel}}
#' 
#' @return \code{predict} serie temporal multivariada contendo valor esperado e desvio padrao de
#'     previsao;
#' 
#' @rdname modelos_ss_reg_din
#' 
#' @export

predict.ss_reg_din <- function(object, newdata, n.ahead, ...) {
    modelo <- object$modelo

    if(missing(newdata)) stop("Forneca a variavel explicativa para previsao atraves do parametro newdata")

    if(!missing(n.ahead)) {
        regobs <- min(n.ahead, nrow(newdata))
        newdata <- newdata[seq(regobs), , drop = FALSE]
    }

    # Como extserie nao tem sazonalidade, update vai lancar um aviso que nao tem utilidade aqui
    extserie <- rep(NA_real_, nrow(newdata))
    extmod   <- suppressWarnings(update(object, extserie, newdata)$modelo)

    prev <- predict(modelo, newdata = extmod, se.fit = TRUE, filtered = TRUE, ...)
    colnames(prev) <- c("prev", "sd")

    return(prev)
}

#' @details 
#' 
#' \bold{Update}:
#' 
#' A atualizacao de modelos \code{ss_reg_din} sempre vai checar se o modelo passado foi estimado
#' corretamente. Como modelos em espaco de estados dependem bastante de inicializacao, as vezes nao
#' da pra estimar direito. Nesses casos ele tenta reestimar o modelo independentemente de 
#' \code{refit}
#' 
#' @param newseries nova serie com a qual atualizar o modelo
#' @param newregdata vetor, matriz ou data.frame contendo variaveis explicativas na nova amostra
#' @param refit booleano indicando se o modelo deve ou nao ser reajustado
#' @param ... demais argumentos passados a \code{\link[KFAS]{predict.SSModel}}
#' 
#' @return \code{update} retorna modelo com novos dados e, caso \code{refit == TRUE}, reajustado;
#' 
#' @rdname modelos_ss_reg_din
#' 
#' @export

update.ss_reg_din <- function(object, newseries, newregdata, refit = FALSE, ...) {

    if(refit) {
        formula <- attr(object$modelo, "formula")
        vardin  <- attr(object$modelo, "vardin")
        object  <- estimamodelo(newseries, "ss_reg_din", regdata = newregdata, formula = formula,
            vardin = vardin)
    } else {

        modelo <- object$modelo

        if(missing(newregdata)) {
            stop("Forneca nova variavel explicativa atraves do parametro newregdata")
        }

        saz <- attr(modelo, "saz")

        desloc <- parsedesloc(object$serie, newseries, saz)

        Hmat <- modelo["H"][, , seq_len(saz), drop = FALSE]
        Hmat <- Hmat[, , shift(seq_len(saz), desloc), drop = FALSE]

        Qmat <- modelo["Q"]
        form <- attr(modelo, "formula")
        modelo <- SSModel(newseries ~ SSMregression(form, newregdata, Q = Qmat), H = Hmat)

        attr(modelo, "formula") <- attr(object$modelo, "formula")
        attr(modelo, "vardin")  <- attr(object$modelo, "vardin")
        attr(modelo, "saz")     <- attr(object$modelo, "saz")

        object$modelo <- modelo
        object$serie  <- newseries
    }

    return(object)
}

# HELPERS ------------------------------------------------------------------------------------------

#' Deslocamento Do Array De Variancias
#' 
#' Identifica a necessidade de deslocar o array H dependendo de em que instante cada serie comeca
#' 
#' Modelos com heterocedasticidade tem um array de variancias H, ao inves de uma matriz simples. 
#' Isto significa que, dependendo de em que indice sazonal a serie original e nova comecam, pode ser
#' necessario deslocar o array um numero de posicoes de modo que as variancias corretas estejam
#' associadas as novas observacoes. Por exemplo, se serie original tem freq = 4, H seria um array de
#' quatro posicoes ao longo da terceira dimensao. Se serie comecava em (XX, 3), as variancias em H
#' estao associadas aos indices 3, 4, 1, 2. Desta forma, se newseries comeca em (YY, 2), H deve ser
#' deslocado uma posicao a direita, de modo que o array resultante tenha, em ordem, as variacias 
#' associadas aos indices 2, 3, 4, 1, o que e a ordem em que se encontram em newseries.
#' 
#' Tudo isso so faz sentido se assumirmos que tanto serie quanto newseries foram fornecidos
#' como objetos de serie temporal, de modo que e possivel identificar esse tipo de coisa. 
#' Caso serie tenha sido um vetor, sera assumido que comeca no indice sazonal 1, o que pode
#' estar errado. Similarmente, se newseries for um vetor simples, sera assumido que comeca
#' no instante de tempo imediatamente apos o final da serie original.
#' 
#' @param serie a serie original do modelo
#' @param newseries nova serie para update
#' @param saz frequencia da heterocedasticidade
#' 
#' @return inteiro indicando numero de posicoes a deslocar por \code{\link{shift}}

parsedesloc <- function(serie, newseries, saz) {

    freq_old <- frequency(serie)
    freq_new <- frequency(newseries)

    if(freq_old != saz) {
        wrn <- paste0("O modelo foi ajustado com heterocedasticidade, mas 'serie' nao era um objeto",
            "ts -- Sera transformado com inicio = c(1, 1) e frequecia igual a da heterocedasticidade")
        warning(wrn)
        serie <- ts(serie, freq = saz)
    }
    init_old <- start(serie)[2]

    if(freq_new != saz) {
        wrn <- paste0("'newseries' nao e serie temporal ou nao tem sazonalidade igual a da",
            " heterocedasticidade -- Sera transformada para uma ts iniciando imediatamente apos ",
            "o termino da serie original")
        warning(wrn)
        newseries <- ts(newseries, start = deltats(end(serie), 1, saz), freq = saz)
    }
    init_new <- start(newseries)[2]

    desloc <- init_old - init_new

    return(desloc)
}
