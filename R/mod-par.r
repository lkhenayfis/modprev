####################################################################################################
# MODELO PAR(p)[-A]
####################################################################################################

#' Modelos \code{parp}
#' 
#' Estimação e métodos de modelos da classe \code{parp}
#' 
#' Modelos periódicos autorregressivos, possivelmente com parcela anual, similares aos do GEVAZP
#' 
#' @name modelos_parp
NULL

# ESTIMACAO ----------------------------------------------------------------------------------------

#' \bold{Estimação}:
#' 
#' Modelos PAR(p) são necessariamente ajustados a séries temporais com sazonalidade. Desta forma, o
#' argumento \code{serie} deve ser um objeto \code{ts} com sazonalidade ou então o período de 
#' sazonalidade deve ser informado através de \code{s}. Caso nenhuma destas condições seja atendida
#' a função aborta com erro.
#' 
#' A identificação de ordem destes modelos pode ser feita de forma automática, informando o  
#' argumento \code{p = "auto"}. A seleção da ordem de cada modelo é realizada a partir das PACFs 
#' periódicas, buscando o primeiro lag, a partir do primeiro, dentro do intervalo de confiança para
#' não significância, até o lag \code{max.p} correspondente. Para consistência com o GEVAZP, a 
#' identificação automática usa funções deautocorrelação parcial periódica quando \code{A12 = FALSE} 
#' e condicional do contrário.
#' 
#' @param serie série para ajustar
#' @param s periodo de sazonalidade, caso \code{serie} nao seja uma serie temporal com sazonalidade
#' @param p vetor de comprimento igual a sazonalidade contendo as ordens de cada modelo periódico;
#'     caso tenha comprimento menor que \code{s}, sera reciclado. Ver Detalhes
#' @param max.p vetor de comprimento igual a sazonalidade contendo as máximas ordens para 
#'     identificação automática; será reciclado se necessário. Ignorado se \code{p = "auto"}
#' @param A12 booleano indicando se a parcela anual deve ser incorporada à estimação
#' @param ... nao possui uso, existe apenas para consistencia com a generica
#' 
#' @return Objeto da classe \code{modprev} e subclasse \code{parp}, uma lista de dois 
#'     elementos: \code{modelo} e \code{serie} contendo o modelo estimado e a série passada
#' 
#' @rdname modelos_parp

parp <- function(serie, s = frequency(serie), p = "auto", A12 = FALSE, max.p = 6, ...) {

    if (s == 1) stop("'serie' nao possui sazonalidade -- informe uma serie sazonal ou um periodo 's'")
    if ((frequency(serie) == 1) && (s > 1)) serie <- ts(serie, frequency = s)

    if (length(p) < s) p <- rep(p, s)
    if (length(max.p) < s) max.p <- rep(max.p, s)

    anyauto <- any(p == "auto")

    if (anyauto) {
        auto_p <- which(p == "auto")
        auto_p <- sapply(auto_p, idordem, serie = serie, max.p = max.p, A12 = A12)
        p[p == "auto"] <- auto_p
    }

    mods <- lapply(seq_len(s), function(m) fitparp(serie, m, p[m], A12))
}

# AUXILIARES ---------------------------------------------------------------------------------------

#' Estima PAR(p)s Individuais
#' 
#' Função interna para estimação de cada um dos modelos periódicos pelo método Yule-Walker

fitparp <- function(serie, m, p, A12) {
    NA
}

#' Identificação De Ordem Automática
#' 
#' Função interna para identificação da ordem de um modelo periódico individual
#' 
#' @param serie serie temporal
#' @param m o mes para o qual identificar a ordem
#' @param A12 booleano indicando o uso de parcela A
#' 
#' @return escalar inteiro correspondendo à ordem identificada

idordem <- function(serie, m, max.p, A12) {
    idfun <- if (A12) idordem_parp_a else idordem_parp
    p <- idfun(serie, m, max.p)
    return(p)
}

idordem_parp <- function(serie, m, max.p) {
    NA_integer_
}

idordem_parp_a <- function(serie, m, max.p) {
    NA_integer_
}

#' Autocorrelação Parcial/Condicional Periódica
#' 
#' Calcula e opcionalmente plota a ACF parcial ou condicional de uma determinada série sazonal
#' 
#' @param serie serie temporal da qual calcular autocorrelações
#' @param lag.max inteiro indicando o maior lag para o qual se calcular a autocorrelação
#' @param plot booleano indicando se deve ser feito o plot das autocorrelações calculadas

perpacf <- function(serie, lag.max = 6, plot = FALSE, ...) {
    NA
}

percacf <- function(serie, lag.max = 6, plot = FALSE, ...) {
    NA
}
