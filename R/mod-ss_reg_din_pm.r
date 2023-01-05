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
    nvars <- ncol(model.matrix(formula, data = regdata)) - 1 # model.matrix inclui intercept

    if(missing(formula)) formula <- expandeformula(regdata)

    freq <- frequency(serie)
    if(freq == 1) stop("'serie' nao possui sazonalidade")

    sobra <- nrow(regdata) %% freq
    if(sobra != 0) stop("'serie' nao possui numero inteiro de periodos de sazonalidade")
    pers <- nrow(regdata) %/% freq

    if(vardin & (frequency(serie) == 1)) {
        warning("'vardin' e TRUE mas 'serie' nao possui sazonalidade -- ignorando 'vardin'")
        vardin <- FALSE
    }

    serie_m <- ts(matrix(serie, ncol = freq, byrow = TRUE), start = start(serie), frequency = 1)
    mats <- expande_sist_mats(regdata, freq, pers, nvars)

    mod <- SSModel(serie_m ~ SSMcustom(mats$Z, mats$T, mats$R, mats$Q) - 1, H = mats$H)

    updH <- ifelse(vardin, updH_heter_trig_pm, updH_homoc_pm)
    upfunc <- function(par, mod) updH(par, updQ_pm(par, mod), freq = freq)

    start <- rep(0,
        1 +          # variancia de epsilon se vardin = FALSE; intercept da funcao de variancia c.c.
        2 * vardin + # coeficientes harmonicos caso vardin = TRUE
        nvars        # coeficientes de regressao
    )
    fit <- fitSSM(mod, start, upfunc, method = "BFGS")

    if(fit$optim.out$convergence < 0) fit$model$Z[] <- NA

    mod_atrs <- list(formula = formula, vardin = vardin, freq = freq)
    out <- new_modprevU(fit$model, serie, "ss_reg_din_pm", mod_atrs)

    return(out)
}

expande_sist_mats <- function(regdata, freq, pers, nvars) {

    ff <- function(...) abind::abind(..., along = 3)

    Z <- split(cbind(b0 = 1, regdata), rep(seq_len(pers), each = freq))
    Z <- do.call(ff, Z)

    T <- diag(nvars + 1)
    T <- do.call(ff, lapply(seq_len(pers), function(i) return(T)))

    R <- matrix(rep(0, nvars), nrow = 1)
    R <- rbind(R, diag(nvars))

    Q <- diag(NA_real_, nvars)

    H <- diag(NA_real_, freq)

    out <- list(Z = Z, T = T, R = R, Q = Q, H = H)

    return(out)
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
    diag(mod["Q"][, , 1]) <- exp(parQ)
    return(mod)
}
