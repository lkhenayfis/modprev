####################################################################################################
# MODELO REGRESSAO ESTATICA SIMPLES
####################################################################################################

#' Modelos \code{gam}
#' 
#' Estimação e métodos de modelos da classe \code{gam}
#' 
#' Modelos de regressao linear estaticas comuns, estimados atraves da funcao \code{\link[stats]{lm}}
#' 
#' @name modelos_gam
#' 
#' @import mgcv
NULL

# ESTIMACAO ----------------------------------------------------------------------------------------

#' \bold{Estimação}:
#' 
#' Para estimação deste tipo de modelo, além de ser fornecido o arumento \code{serie} também deve 
#' ser necessariamente informado \code{regdata}, um \code{data.frame}-like contendo as variáveis 
#' explicativas necessárias. 
#' 
#' Opcionalmente pode ser informado o argumento \code{formula} contendo a especificação do modelo 
#' aditivo no formato compativel (veja \code{\link[mgcv]{gam}}), porem sem o LHS. Caso 
#' \code{formula} seja omitido, todas as variaveis serao utilizadas conjuntamente numa thin plate
#' regression spline com as configurações padrão do pacote, i.e. se existem as colunas
#' \code{c("V1", "V2", "V3")}, formula sera \code{~ s(V1, V2, V3)}.
#' 
#' Na execucao em janela movel, o vetor de pesos deve ser passado com o tamanho igual ao da janela.
#' O mesmo vetor sera aplicado em todas as janelas avaliadas.
#' 
#' @param serie série para ajustar
#' @param regdata \code{data.frame}-like contendo variáveis explicativas
#' @param formula opcional, fórmula da regressão. Se for omitido, todas as variaveis em 
#'     \code{regdata} serão utilizadas
#' @param pesos opcional, vetor de pesos para cada observacao no ajuste. Sera normalizado 
#'     internamente
#' @param ... nao possui uso, existe apenas para consistencia com a generica
#' 
#' @return Objeto da classe \code{modprev} e subclasse \code{gam}, uma lista de dois 
#'     elementos: \code{modelo} e \code{serie} contendo o modelo estimado e a série passada
#' 
#' @rdname modelos_gam

GAM <- function(serie, regdata, formula, pesos = rep(1, length(serie)), ...) {

    if (missing(regdata)) stop("Forneca a variavel explicativa atraves do parametro 'regdata'")

    if (missing(formula)) formula <- expandeformula(regdata, "gam")
    formula <- update(formula, Y ~ .)

    if (!is.ts(serie)) serie <- ts(serie)
    aux_tsp <- tsp(serie)

    pesos <- pesos / sum(pesos)

    regdata <- cbind(Y = as.numeric(serie), regdata)
    fit <- mgcv::gam(formula, data = regdata)

    mod_atrs <- list(formula = formula, tsp = aux_tsp, pesos = pesos)

    new_modprevU(fit, serie, "GAM", mod_atrs)
}
