####################################################################################################
# MODELO REGRESSAO DINAMICA SIMPLES
####################################################################################################

#' Modelos \code{ss_reg_din}
#' 
#' Estimação e métodos de modelos da classe \code{ss_reg_din}
#' 
#' @name modelos_ss_reg_din
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
#' @param vardin booleano ou inteiro indicando se deve ser estimado modelo com heterocedasticidade.
#'     Caso \code{TRUE} tenta pegar a sazonalidade da série; se for um número inteiro assume este
#'     valor como a sazonalidade
#' @param estatica booleano indicando se a regressao deve ser de coeficientes estaticos
#' @param lambda penalidade multiplicando o traco da matriz Q. Ver Detalhes
#' @param ... nao possui uso, existe apenas para consistencia com a generica
#' 
#' @return Objeto da classe \code{modprev} e subclasse \code{ss_reg_din}, uma lista de dois 
#'     elementos: \code{modelo} e \code{serie} contendo o modelo estimado e a série passada
#' 
#' @rdname modelos_ss_reg_din

ss_reg_din <- function(serie, regdata, formula, vardin = FALSE, estatica = FALSE,
    lambda = 0, ...) {

    if(missing(regdata)) stop("Forneca a variavel explicativa atraves do parametro 'regdata'")

    if(missing(formula)) formula <- expandeformula(regdata)

    nvars  <- ncol(model.matrix(formula, data = regdata)) - 1 # model.matrix inclui intercept

    if(vardin & (frequency(serie) == 1)) warning("'vardin' e TRUE mas 'serie' nao possui sazonalidade")
    vardin <- vardin * 1

    saz <- ifelse(vardin == 0, 1, frequency(serie) * (vardin == 1) + vardin * (vardin > 1))

    Qfill <- ifelse(estatica, 0, NA_real_)
    mod   <- SSModel(serie ~ SSMregression(formula, regdata, Q = diag(Qfill, nvars)),
        H = array(NA_real_, c(1, 1, saz)))

    updQ <- ifelse(estatica, updQ_stat, updQ_din)
    updH <- ifelse(vardin == 0, updH_homoc, updH_heter_trig)
    upfunc <- function(par, mod) updH(par, updQ(par, mod), saz = saz)

    start <- rep(0, nvars * is.na(Qfill) + 1 + (vardin != 0))
    fit <- fitSSM2(mod, start, upfunc, method = "BFGS", lambda = lambda)

    if(fit$optim.out$convergence < 0) fit$model$Z[] <- NA

    mod_atrs <- list(formula = formula, vardin = vardin, saz = saz, estatica = estatica,
        lambda = lambda)
    out <- new_modprevU(fit$model, serie, "ss_reg_din", mod_atrs)

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
#' @param object objeto com classes \code{c("ss_reg_din", "modprev")} contendo modelo
#' @param newdata \code{data.frame}-like contendo variéveis explicativas fora da amostra
#' @param n.ahead número de passos à frente para previsão. Este argumento não é necessario, caso não
#'     seja informado a previsão sera feita tantos passos à frente quanto amostras em \code{newdata}
#' @param ... demais argumentos passados a \link[KFAS]{\code{predict.SSModel}}
#' 
#' @return \code{predict} serie temporal multivariada contendo valor esperado e desvio padrao de
#'     previsão \code{n.ahead} passos à frente;
#' 
#' @rdname modelos_ss_reg_din
#' 
#' @export

predict.ss_reg_din <- function(object, newdata, n.ahead, ...) {
    modelo <- object$modelo

    if(missing(newdata)) stop("Forneca a variavel explicativa para previsao atraves do parametro 'newdata'")

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
#' A atualização de modelos \code{ss_reg_din} sempre vai checar se o modelo passado foi estimado
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
#' @rdname modelos_ss_reg_din
#' 
#' @export

update.ss_reg_din <- function(object, newseries, newregdata, refit = FALSE, ...) {

    mod_atrs <- attr(object, "mod_atrs")

    if(refit) {
        formula <- mod_atrs$formula
        vardin  <- mod_atrs$vardin
        lambda  <- mod_atrs$lambda
        object  <- estimamodelo(newseries, "ss_reg_din", regdata = newregdata, formula = formula,
            vardin = vardin, lambda = lambda)
    } else {

        modelo <- object$modelo

        if(missing(newregdata)) stop("Forneca nova variavel explicativa atraves do parametro 'newregdata'")

        saz <- mod_atrs$saz

        desloc <- parsedesloc(object$serie, newseries, saz)

        Hmat <- modelo["H"][, , seq_len(saz), drop = FALSE]
        Hmat <- Hmat[, , shift(seq_len(saz), desloc), drop = FALSE]

        Qmat <- modelo["Q"]
        form <- mod_atrs$formula
        modelo <- SSModel(newseries ~ SSMregression(form, newregdata, Q = Qmat), H = Hmat)

        object <- new_modprevU(modelo, newseries, "ss_reg_din", mod_atrs)
    }

    return(object)
}

# HELPERS ------------------------------------------------------------------------------------------

#' Auxiliares Para Estimacao
#' 
#' Funcoes de atualizacao das matrizes H e Q num modelo espaco de estados
#' 
#' Cada funcao \code{updX_*} atualiza a matriz \code{X} do sistema por uma abordagem diferente. 
#' Independentemente do numero de hiperparametros necessarios, cada funcao deve receber o vetor 
#' completo e extrai dele os que sao pertinentes para si. Por padrao, para facilitar este processo,
#' os hiperparametros associado a matriz H sempre estao primeiro e os a Q por ultimo.
#' 
#' Por exemplo, \code{updH_heter_trig} assume que as variancias sazonais sao geradas por uma funcao
#' trigonometrica de um harmonico definida em termos de dois hiperparametros, os dois primeiros 
#' elementos do vetor \code{par}. Os demais, quantos quer que sejam, sao relativos a Q.
#' 
#' @param par vetor completo de hiperparametros. Ver Detalhes
#' @param mod o modelo a ser atualizado
#' @param saz sazonalidade da serie modelada
#' @param ... sem uso, apenas para consistencia de argumentos entre funcoes
#' 
#' @return \code{mod} com a matriz modificada
#' 
#' @name update_funs
NULL

#' @rdname update_funs

updH_homoc <- function(par, mod, ...) {
    mod["H"][1, 1, 1] <- exp(par[1])
    return(mod)
}
#' @rdname update_funs

updH_heter_trig <- function(par, mod, saz, ...) {
    uH   <- cos(0:(saz - 1) * 2 * pi / saz)
    vH   <- sin(0:(saz - 1) * 2 * pi / saz)

    parH <- head(par, 2)
    mod["H"][] <- rep(exp(parH[1] * uH + parH[2] * vH), length.out = dim(mod["H"])[3])

    return(mod)
}
#' @rdname update_funs

updQ_stat <- function(par, mod, ...) {
    return(mod)
}
#' @rdname update_funs

updQ_din <- function(par, mod, ...) {
    nQ <- dim(mod["Q"])[1]

    parQ <- tail(par, nQ)
    for(i in seq_along(parQ)) mod["Q"][i, i, 1] <- exp(parQ[i])
    return(mod)
}

#' Deslocamento Do Array De Variancias
#' 
#' Identifica a necessidade de deslocar o array H dependendo de em que instante cada serie comeca
#' 
#' Modelos com heterocedasticidade tem um array de variancias H, ao inves de uma matriz simples. 
#' Isto significa que, dependendo de em que indice sazonal a serie original e nova comecam, pode ser
#' necessario deslocar o array um número de posicoes de modo que as variancias corretas estejam
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
#' @return inteiro indicando número de posicoes a deslocar por \code{\link{shift}}

parsedesloc <- function(serie, newseries, saz) {

    freq_old <- frequency(serie)
    freq_new <- frequency(newseries)

    if(saz == 1) return(0)

    if(freq_old != saz) {
        wrn <- paste0("O modelo foi ajustado com heterocedasticidade, mas 'serie' nao era um objeto",
            "ts -- Sera transformado com inicio = c(1, 1) e frequecia igual a da heterocedasticidade")
        warning(wrn)
        serie <- ts(serie, frequency = saz)
    }
    init_old <- start(serie)[2]

    if(freq_new != saz) {
        wrn <- paste0("'newseries' nao e serie temporal ou nao tem sazonalidade igual a da",
            " heterocedasticidade -- Sera transformada para uma ts iniciando imediatamente apos ",
            "o termino da serie original")
        warning(wrn)
        newseries <- ts(newseries, start = deltats(end(serie), 1, saz), frequency = saz)
    }
    init_new <- start(newseries)[2]

    desloc <- init_old - init_new

    return(desloc)
}

# CROSS VALIDATION ---------------------------------------------------------------------------------

#' Estimacao De SSM Com lambda
#' 
#' Funcao que faz a estimacao de modelos em espaco de estado com penalizacao da matriz Q
#' 
#' \code{fitSSM2} e uma copia de \code{\link[KFAS]{fitSSM}}, com uma simples modificacao: a funcao 
#' objetivo agora e uma ponderacao entre a verossimilhanca e uma penalizacao da variacao total da 
#' matriz Q. Isto e feito no contexto de regressoes dinamicas com o intuito de controlar o quao 
#' variaveis sao os regressores. 
#' 
#' Para maiores detalhes, ver \code{\link[KFAS]{fitSSM}}, pois todo o restante de interface e saidas
#' e igual
#' 
#' @param inits Initial values for \code{\link{optim}}.
#' @param model Model object of class \code{SSModel}.
#' @param updatefn User defined function which updates the model given the
#'   parameters. Must be of form \code{updatefn(pars, model, ...)},
#'   where \code{...} correspond to optional additional arguments.
#'   Function should return the original model with updated parameters.
#'   See details for description of the default \code{updatefn}.
#' @param checkfn Optional function of form \code{checkfn(model)} for checking
#' the validity of the model. Should return \code{TRUE} if the model is valid,
#' and \code{FALSE} otherwise. See details.
#' @param update_args Optional list containing additional arguments to \code{updatefn}.
#' @param lambda penality coefficient
#' @param ... Further arguments for functions \code{optim} and
#'  \code{logLik.SSModel}, such as \code{nsim = 1000}, \code{marginal = TRUE}, 
#'   and \code{method = "BFGS"}.

fitSSM2 <- function (model, inits, updatefn, checkfn, update_args = NULL, lambda = 0, ...) {
    is_gaussian <- all(model$distribution == "gaussian")
    if (missing(updatefn)) {
        estH <- is_gaussian && any(is.na(model$H))
        estQ <- any(is.na(model$Q))
        if ((dim(model$H)[3] > 1 && estH || (dim(model$Q)[3] >
            1) && estQ))
            stop("No model updating function supplied, but cannot use default\n   
          function as the covariance matrices are time varying.")
        updatefn <- function(pars, model) {
            if (estQ) {
                Q <- as.matrix(model$Q[, , 1])
                naQd <- which(is.na(diag(Q)))
                naQnd <- which(upper.tri(Q[naQd, naQd]) & is.na(Q[naQd,
                  naQd]))
                Q[naQd, naQd][lower.tri(Q[naQd, naQd])] <- 0
                diag(Q)[naQd] <- exp(0.5 * pars[1:length(naQd)])
                Q[naQd, naQd][naQnd] <- pars[length(naQd) + 1:length(naQnd)]
                model$Q[naQd, naQd, 1] <- crossprod(Q[naQd, naQd])
            }
            else naQnd <- naQd <- NULL
            if (estH) {
                H <- as.matrix(model$H[, , 1])
                naHd <- which(is.na(diag(H)))
                naHnd <- which(upper.tri(H[naHd, naHd]) & is.na(H[naHd,
                  naHd]))
                H[naHd, naHd][lower.tri(H[naHd, naHd])] <- 0
                diag(H)[naHd] <- exp(0.5 * pars[length(naQd) +
                  length(naQnd) + 1:length(naHd)])
                H[naHd, naHd][naHnd] <- pars[length(naQd) + length(naQnd) +
                  length(naHd) + 1:length(naHnd)]
                model$H[naHd, naHd, 1] <- crossprod(H[naHd, naHd])
            }
            model
        }
    }
    is.SSModel(do.call(updatefn, args = c(list(inits, model),
        update_args)), na.check = TRUE, return.logical = FALSE)
    if (!is_gaussian && is.null(list(...)$theta)) {
        theta <- KFAS:::initTheta(model$y, model$u, model$distribution)
    }
    else theta <- NULL
    if (missing(checkfn)) {
        if (is_gaussian) {
            checkfn <- function(model) {
                all(sapply(c("H", "T", "R",
                  "Q", "a1", "P1", "P1inf"),
                  function(x) {
                    all(is.finite(model[[x]]))
                  })) && max(model$Q) <= 1e+07 && max(model$H) <=
                  1e+07
            }
        }
        else {
            checkfn <- function(model) {
                all(sapply(c("u", "T", "R",
                  "Q", "a1", "P1", "P1inf"),
                  function(x) {
                    all(is.finite(model[[x]]))
                  })) && max(model$Q) <= 1e+07
            }
        }
    }
    likfn <- function(pars, model, ...) {
        model <- do.call(updatefn, args = c(list(pars, model),
            update_args))
        if (checkfn(model)) {
            out <- -logLik(object = model, check.model = FALSE, theta = theta, ...)
        }
        else out <- .Machine$double.xmax^0.75

        out <- out + lambda * sum(model["Q"])
    }
    out <- NULL
    out$optim.out <- optim(par = inits, fn = likfn, model = model,
        ...)
    out$model <- do.call(updatefn, args = c(list(out$optim.out$par,
        model), update_args))
    is.SSModel(out$model, na.check = TRUE, return.logical = FALSE)
    out
}

#' Validacao Cruzada De Regressao Dinamica
#' 
#' Realiza validacao cruzada de regressao dinamica por grid check
#' 
#' Esta funcao executa uma regressao dinamica em janela movel com diferentes valores de penalidade
#' \code{lambda} buscando aquele que leva ao melhor modelo do ponto de vista de previsao.
#' 
#' A lista \code{cv_control} pode conter um ou mais dos seguintes elementos nomeados
#' 
#' \describe{
#' \item{janela}{tamanho da janela para ajuste -- similar ao argumento homonimo em 
#'     \code{\link{janelamovel}}}
#' \item{passo}{salto entre uma janela e outra -- similar ao argumento homonimo em 
#'     \code{\link{janelamovel}}}
#' \item{refit.cada}{inteiro indicando de quantas em quantas janelas o modelo deve ser reajustado -- 
#'     similar ao argumento homonimo em \code{\link{janelamovel}}}
#' \item{obj.ahead}{vetor de inteiros indicando a janela de passos a frente de interesse para a 
#'     previsao}
#' \item{error_func}{funcao que recebe um vetor de erros de previsao e retorna um escalar como 
#'     metrica de performance. Default function(x) sum(x^2)}
#' \item{lambda}{vetor de lambdas a serem testados}
#' } 
#' 
#' @param serie série para ajustar
#' @param regdata \code{data.frame}-like contendo variáveis explicativas
#' @param formula opcional, fórmula da regressão. Se for omitido, todas as variaveis em 
#'     \code{regdata} serão utilizadas
#' @param vardin booleano ou inteiro indicando se deve ser estimado modelo com heterocedasticidade.
#'     Caso \code{TRUE} tenta pegar a sazonalidade da série; se for um número inteiro assume este
#'     valor como a sazonalidade
#' @param estatica booleano indicando se a regressao deve ser de coeficientes estaticos
#' @param cv_control opcional, uma lista de controle da validacao cruzada. Ver Detalhes
#' 
#' @return valor otimo de \code{lambda}
#' 
#' @export

CV_regdin <- function(serie, regdata, formula, vardin = FALSE, estatica = FALSE,
    cv_control = list()) {

    df_cv_ctrl <- list(
        janela = floor(length(serie) / 10),
        passo = 1L,
        obj.ahead = 5:10,
        refit.cada = NA,
        error_func = function(x) sum(x^2),
        lambda = c(0, exp(seq(0, 5, .5)))
    )
    df_cv_ctrl[names(cv_control)] <- cv_control

    params_janela <- lapply(df_cv_ctrl$lambda, function(lambda) {
        out <- df_cv_ctrl
        out$lambda <- lambda
        out$n.ahead <- max(df_cv_ctrl$obj.ahead)
        out
    })

    jms <- match.call()
    jms <- lapply(params_janela, function(params) {
        jms[[1]] <- quote(janelamovel)
        jms$cv_control <- NULL
        jms <- c(as.list(jms), params)
        jms$error_func <- NULL
        jms$tipo <- "ss_reg_din"
        as.call(jms)
    })
    for(i in seq(jms)) jms[[i]] <- eval(jms[[i]], parent.frame(), parent.frame())

    erros <- sapply(jms, function(jm) {
        out <- sapply(jm, function(prev) {
            erro <- serie - prev[, 1]
            df_cv_ctrl$error_func(erro[df_cv_ctrl$obj.ahead])
        })
        mean(out)
    })

    final_lambda <- df_cv_ctrl$lambda[which.min(erros)]

    return(final_lambda)
}
