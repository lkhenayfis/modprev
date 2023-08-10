############################## FUNCOES UTILITARIAS PARA USO NO PACOTE ##############################

# GERAIS -------------------------------------------------------------------------------------------

#' Desloca Vetor De \code{i} Posições
#' 
#' Desloca os valores de um vetor mantendo o mesmo comprimento original
#' 
#' @param v vetor a ser manipulado
#' @param i posições para deslocar -- se negativo, para a esquerda, positivo para a direita
#' 
#' @examples 
#' 
#' vec <- seq(10)
#' vec_1 <- modprev:::shift(vec, -2) # c(3, 4, 5, 6, 7, 8, 9, 10, 1, 2)
#' vec_1 <- modprev:::shift(vec, 2)  # c(9, 10, 1, 2, 3, 4, 5, 6, 7, 8)
#' 
#' @return Vetor \code{v} com elementos deslocados \code{i} posições
#' 
#' @importFrom utils head tail

shift <- function(v, i) {

    direcao <- sign(i)
    desloc  <- abs(i)
    sizevec <- length(v)

    if(desloc >= sizevec) desloc <- desloc %% sizevec

    if(direcao == -1) {
        c(v[(desloc + 1):sizevec], head(v, desloc))
    } else {
        c(tail(v, desloc), v[1:(sizevec - desloc)])
    }
}

# MANIPULACAO DE INDICE TEMPORAL -------------------------------------------------------------------

#' Índice De Tempo Por Delta Em Série Temporal
#' 
#' Calcula índice no sistema de tempo de um \code{ts} especificada para um dado delta
#' 
#' Função interna utilizada no escopo da janela rolante. Calcula o índice no tempo \code{delta} 
#' instantes após um início \code{ini}, num sistema com sazonalidade de \code{freq} períodos.
#' 
#' @param ini índice temporal no formato (x, y) de série temporal indicando início da janela 
#' @param delta passos de tempo a deslocar a partir de \code{ini}. Pode ser positivo ou negativo
#' @param freq frequência/sazonalidade da série
#' 
#' @examples 
#' 
#' # exemplo com uma serie bimestral (frequencia = 6)
#' tempo <- c(2, 4)
#' freq  <- 6 
#' 
#' # deslocando + 1
#' tempo_desloc <- modprev:::deltats(tempo, 1, freq) # c(2, 5)
#' 
#' # deslocando + 5, de modo que vire o ano
#' tempo_desloc <- modprev:::deltats(tempo, 5, freq) # c(3, 3)
#' 
#' # deslocando - 5
#' tempo_desloc <- modprev:::deltats(tempo, -5, freq) # c(1, 5)
#' 
#' @return O índice no sistema de tempo \code{ts} indicando o final da janela

deltats <- function(ini, delta, freq) {

    if(delta > 0) {
        aux <- ts(seq_len(delta + 1), start = ini, frequency = freq)
        out <- end(aux)
    } else if(delta < 0) {
        aux <- ts(seq_len(abs(delta) + 1), end = ini, frequency = freq)
        out <- start(aux)
    } else {
        out <- ini
    }

    return(out)
}

# AUXILIAR PARA MODELOS COM VARIAVEL EXPLICATIVA ---------------------------------------------------

expandeformula <- function(data) {
    warning("'formula' nao foi passado -- usando todas as variaveis de forma aditiva")
    formula <- colnames(data)
    formula <- paste0(colnames(data), collapse = " + ")
    formula <- paste0(" ~ ", formula)
    formula <- as.formula(formula)

    return(formula)
}

# UTILITARIAS DE PAR(p) ----------------------------------------------------------------------------

#' Calculo do desvio padrao normalizado por 1/n
#'
#' Funcao alternativa para calcular desvio padrao atraves do estimador "n"
#' 
#' @param vec [vetor numerico] serie cujo desvio padrao se deseja calcular
#' 
#' @return [escalar numerico] desvio padrao calculado

sd2 <- function(...) {

    pars <- list(...)
    n <- length(pars[[1]])

    sqrt((n - 1) / n) * sd(...)
}

#' Calculo da covariancia normalizada por 1/n
#'
#' Funcao alternativa para calcular covariancia atraves do estimador "n"
#' 
#' @param mat [matriz numerica] serie cuja covariancia se deseja calcular
#' 
#' @return [escalar numerico] covariancia calculada

cov2 <- function(...) {

    pars <- list(...)
    n <- nrow(as.matrix(pars[[1]]))

    (n - 1) / n * cov(...)
}

#' Normaliza Serie Sazonalmente
#' 
#' Funcao interna para padronizar uma serie por periodo de sazonalidade
#' 
#' @param serie serie temporal com sazonalidade para normalizar
#' @param est string indicando qual estimador utilizar para desvio padrao
#'     - "n" estimador normalizando somatorios por n
#'     - "n-1" estimador normalizando somatorios por (n - 1)
#' @param truncdat inteiro indicando o numero de casas decimais para arredondamento do dado 
#'     fornecido. Padrao -1 = nao arredondar
#' @param truncpar inteiro indicando o numero de casas decimais para arredondamento das medias e sd 
#'     para normalizacao. Padrao -1 = nao arredondar
#' 
#' @return [matriz numerica] matriz contendo dat normalizado

scale_by_season <- function(serie, est = "n", truncdat = -1, truncpar = -1) {

    attr0 <- attributes(serie)
    dat <- matrix(as.numeric(serie), ncol = frequency(serie), byrow = TRUE)

    fstd <- ifelse(est == "n-1", sd, sd2)
    STD  <- apply(dat, 2, fstd, na.rm = TRUE)
    MED  <- colMeans(dat, na.rm = TRUE)
    if (truncpar > 0) {
        MED <- round(MED, truncpar)
        STD <- round(STD, truncpar)
    }
    if (truncdat > 0) {
        dat <- round(dat, truncdat)
    }

    out <- sapply(seq_len(ncol(dat)), function(i) (dat[, i] - MED[i]) / STD[i])
    out <- c(t(out))
    attributes(out) <- attr0
    attr(out, "medias") <- MED
    attr(out, "desvpads") <- STD
    return(out)
}

#' Médias Móveis Sazonais
#' 
#' Calcula a médias móveis rolantes para uso nos modelos PAR(p) com parcela anual
#' 
#' As médias móveis aqui calculadas são realizadas com largura de janela igual ao período de 
#' sazonalidade da série. As observações na série de média correspondem às médias dos \code{s} meses
#' anteriores à observação correspondente em \code{serie}
#' 
#' @param serie a serie da qual calcular media movel
#' 
#' @return serie temporal com mesmos atributos que \code{serie} contendo as médias

medias_sazo <- function(serie) {

    attr0 <- attributes(serie)
    N <- length(serie)

    out <- rep(NA_real_, N)
    ini <- frequency(serie) + 1
    for (t in seq(ini, N)) out[t] <- mean(serie[(t - 12):(t - 1)])

    attributes(out) <- attr0

    return(out)
}