####################################################################################################
# MODELO REGRESSAO DINAMICA SIMPLES SEMI-MULTIVARIADO
####################################################################################################

#' Modelos \code{ss_reg_din_pm}
#' 
#' Estimação e métodos de modelos da classe \code{ss_reg_din_pm}
#' 
#' @name modelos_ss_reg_din_pm
NULL

# ESTIMACAO ----------------------------------------------------------------------------------------

#' \bold{Estimação}:
#' 
#' Todos os argumentos deste método de modelagem possuem a mesma definição dos argumentos homonimos
#' em \code{\link{ss_reg_din}}, portanto refira-se a esta pagina para mais detalhes. Do ponto de 
#' vista do usuário, \code{ss_reg_din_pm} opera exatamente como \code{ss_reg_din}.
#' 
#' A diferença entre estes métodos consite na forma como a regressão é montada. Na versão padrão de
#' regressão dinâmica, a série temporal é tratada em sua forma univariada original. Isto pode levar
#' a problemas na previsão especialmente quando em horizontes mais longos (caso de remoção de viés
#' D1 ou mais).
#' 
#' \code{ss_reg_din_pm} manipula a série para um formato multivariado em no \code{R^s} em que cada 
#' observação corresponde a uma sazonalidade da série original (no caso de dados horários, a série
#' univariada vira uma série multivariada de dimensão 24 em que cada observação é um dia completo). 
#' O dado de variáveis explicativas \code{regdata} é quebrado em blocos de \code{s} linhas, sendo 
#' cada bloco utilizado como matriz \code{Z_t} correspondente a cada observação multivariada.
#' 
#' Usualmente modelos multivariados com variáveis exógenas tem um vetor de variáveis explicativas e
#' uma matriz de coeficientes de regressão a ser estimada. Nesta configuração, cada dimensão da
#' variável dependente é uma função separada do mesmo conjunto de variáveis explicativas. Ao 
#' formular o modelo do modo que \code{ss_reg_din_pm} faz, os coeficientes são compartilhados entre
#' todas a dimensões da variável explicativa, de modo que sua atualização é feita contemplando todas
#' as estações conjuntamente.
#' 
#' Este modelo foi concebido no contexto da correção de viés, especificamente para aplicação com 
#' a regressão usando produto tensor de splines.
#' 
#' A opção \code{vardin}, neste caso, continua representando heterocedasticidade no sentido de que
#' cada estação possui uma variância própria, mas em termos mais precisos o modelo formulado é 
#' homocedástico. Isto porque, ao se tornar multivariado, o choque \code{epsilon_t} passa a ser 
#' vetorial com uma matriz de variâncias e covariâncias diagonal (na qual os elementos são 
#' diferentes).
#' 
#' @param serie série com sazonalidade a ser ajustada
#' @param regdata \code{data.frame}-like contendo variáveis explicativas
#' @param formula opcional, fórmula da regressão. Se for omitido, todas as variaveis em 
#'     \code{regdata} serão utilizadas
#' @param vardin booleano indicando se deve ser estimado modelo com heterocedasticidade.
#' @param ... nao possui uso, existe apenas para consistencia com a generica
#' 
#' @return Objeto da classe \code{modprev} e subclasse \code{ss_reg_din_pm}, uma lista de dois 
#'     elementos: \code{modelo} e \code{serie} contendo o modelo estimado e a série passada
#' 
#' @rdname modelos_ss_reg_din_pm

ss_reg_din_pm <- function(serie, regdata, formula, vardin = FALSE, estatica = FALSE, ...) {

    if(missing(regdata)) stop("Forneca a variavel explicativa atraves do parametro 'regdata'")
    if(any(is.na(regdata))) warning("Ha NAs em 'regdata'")

    if(missing(formula)) formula <- expandeformula(regdata)

    if(frequency(serie) == 1) stop("'serie' nao possui sazonalidade")

    if((length(serie) %% frequency(serie)) != 0) {
        stop("'serie' nao possui numero inteiro de periodos de sazonalidade")
    }

    serie_m <- univar2multivar(serie)
    mats <- expande_sist_mats(serie_m, regdata, formula)

    mod <- SSModel(serie_m ~ SSMcustom(mats$Z, mats$T, mats$R, mats$Q) - 1, H = mats$H)

    updH <- ifelse(vardin, updH_heter_trig_pm, updH_homoc_pm)
    upfunc <- function(par, mod) updH(par, updQ_pm(par, mod), freq = frequency(serie))

    start <- rep(0,
        1 +          # variancia de epsilon se vardin = FALSE; intercept da funcao de variancia c.c.
        2 * vardin +         # coeficientes harmonicos caso vardin = TRUE
        sum(is.na(mod["Q"])) # coeficientes de regressao
    )
    fit <- fitSSM(mod, start, upfunc, method = "BFGS")

    if(fit$optim.out$convergence < 0) fit$model$Z[] <- NA

    mod_atrs <- list(formula = formula, vardin = vardin, freq = frequency(serie))
    out <- new_modprevU(fit$model, serie, "ss_reg_din_pm", mod_atrs)

    return(out)
}

univar2multivar <- function(serie) {
    serie_m <- matrix(serie, ncol = frequency(serie), byrow = TRUE)
    serie_m <- ts(serie_m, start = start(serie), frequency = 1)
    return(serie_m)
}

multivar2univar <- function(serie) {
    serie_u <- c(t(serie))
    serie_u <- ts(serie_u, start = c(start(serie), 1), frequency = ncol(serie))
    return(serie_u)
}

expande_sist_mats <- function(serie_m, regdata, formula) {

    regdata <- model.frame(formula, data = regdata, na.action = na.pass)
    regdata <- cbind(b0 = 1, regdata)
    nvars <- ncol(regdata) - 1

    freq <- ncol(serie_m)
    pers <- nrow(serie_m)

    ff <- function(...) abind::abind(..., along = 3)

    Z <- split(regdata, rep(seq_len(pers), each = freq))
    Z <- do.call(ff, Z)

    T <- diag(nvars + 1)
    T <- do.call(ff, lapply(seq_len(pers), function(i) return(T)))

    R <- matrix(rep(0, nvars), nrow = 1)
    R <- rbind(R, diag(nvars))
    R <- array(R, c(dim(R), 1))

    Q <- diag(NA_real_, nvars)
    Q <- array(Q, c(dim(Q), 1))

    H <- diag(NA_real_, freq)
    H <- array(H, c(dim(H), 1))

    out <- list(Z = Z, T = T, R = R, Q = Q, H = H)

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
#' @param object objeto com classes \code{c("ss_reg_din_pm", "modprev")} contendo modelo
#' @param newdata \code{data.frame}-like contendo variéveis explicativas fora da amostra
#' @param n.ahead número de passos à frente para previsão. Este argumento não é necessario, caso não
#'     seja informado a previsão sera feita tantos passos à frente quanto amostras em \code{newdata}
#' @param ... demais argumentos passados a \link[KFAS]{\code{predict.SSModel}}
#' 
#' @return \code{predict} serie temporal multivariada contendo valor esperado e desvio padrao de
#'     previsão \code{n.ahead} passos à frente;
#' 
#' @rdname modelos_ss_reg_din_pm
#' 
#' @export

predict.ss_reg_din_pm <- function(object, newdata, n.ahead, ...) {
    modelo <- object$modelo

    if(missing(newdata)) stop("Forneca a variavel explicativa para previsao atraves do parametro 'newdata'")

    if(!missing(n.ahead)) {
        regobs <- min(n.ahead, nrow(newdata))
        newdata <- newdata[seq(regobs), , drop = FALSE]
    }

    # Como extserie nao tem sazonalidade, update vai lancar um aviso que nao tem utilidade aqui
    extserie <- ts(rep(NA_real_, nrow(newdata)), frequency = frequency(object$serie))
    extmod   <- suppressWarnings(update.ss_reg_din_pm(object, extserie, newdata)$modelo)

    prev <- predict(modelo, newdata = extmod, se.fit = TRUE, filtered = TRUE, ...)
    prev <- lapply(seq_len(2), function(i) {
        mm <- sapply(seq_along(prev), function(s) prev[[s]][, i, drop = FALSE])
        ts(mm, start = start(prev[[1]]))
    })
    prev <- lapply(prev, multivar2univar)
    prev <- cbind(prev[[1]], prev[[2]])
    colnames(prev) <- c("prev", "sd")

    return(prev)
}

#' \bold{Update}:
#' 
#' A atualização de modelos \code{ss_reg_din_pm} sempre vai checar se o modelo passado foi estimado
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
#' @rdname modelos_ss_reg_din_pm
#' 
#' @export

update.ss_reg_din_pm <- function(object, newseries, newregdata, refit = FALSE, ...) {

    mod_atrs <- attr(object, "mod_atrs")

    if(refit) {
        formula <- mod_atrs$formula
        vardin  <- mod_atrs$vardin
        object  <- estimamodelo(newseries, "ss_reg_din_pm", regdata = newregdata, formula = formula,
            vardin = vardin)
    } else {

        modelo <- object$modelo

        if(missing(newregdata)) stop("Forneca nova variavel explicativa atraves do parametro 'newregdata'")
        if(frequency(newseries) == 1) stop("'newseries' nao possui sazonalidade")
        if((length(newseries) %% frequency(newseries)) != 0) {
            stop("'newseries' nao possui numero inteiro de periodos de sazonalidade")
        }

        Hmat <- modelo["H"]
        Qmat <- modelo["Q"]

        newseries_m <- univar2multivar(newseries)
        mats <- expande_sist_mats(newseries_m, newregdata, mod_atrs$formula)

        modelo <- SSModel(newseries_m ~ SSMcustom(mats$Z, mats$T, mats$R, Qmat) - 1, H = Hmat)

        object <- new_modprevU(modelo, newseries, "ss_reg_din_pm", mod_atrs)
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

updH_homoc_pm <- function(par, mod, ...) {
    diag(mod["H"][, , 1]) <- exp(par[1])
    return(mod)
}

#' @rdname update_funs

updH_heter_trig_pm <- function(par, mod, freq, ...) {
    uH   <- cos((seq_len(freq) - 1) * 2 * pi / freq)
    vH   <- sin((seq_len(freq) - 1) * 2 * pi / freq)

    parH <- head(par, 3)
    diag(mod["H"][, , 1]) <- exp(par[1] + parH[2] * uH + parH[3] * vH)

    return(mod)
}

#' @rdname update_funs

updQ_pm <- function(par, mod, ...) {
    nQ <- dim(mod["Q"])[1]
    parQ <- tail(par, nQ)

    mod["Q"][matrix(c(seq_len(nQ), seq_len(nQ), rep(1, nQ)), ncol = 3)] <- exp(parQ)

    return(mod)
}
