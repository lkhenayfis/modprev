############################## FUNCOES UTILITARIAS PARA USO NO PACOTE ##############################

# GERAIS -------------------------------------------------------------------------------------------

#' Desloca Vetor De \code{i} Posicoes
#' 
#' Desloca os valores de um vetor mantendo o mesmo comprimento original
#' 
#' @param v vetor a ser manipulado
#' @param i posicoes para deslocar -- se negativo, para a esquerda, positivo para a direita
#' 
#' @return vetor \code{v} com elementos deslocados \code{i} posicoes

shift <- function(vec, i) {

    direcao <- sign(i)
    desloc  <- abs(i)
    sizevec <- length(vec)

    if(desloc >= sizevec) desloc <- desloc %% sizevec

    if(direcao == -1) {
        c(vec[(desloc + 1):sizevec], head(vec, desloc))
    } else {
        c(tail(vec, desloc), vec[1:(sizevec - desloc)])
    }
}

# MANIPULACAO DE INDICE TEMPORAL -------------------------------------------------------------------

#' Índice De Tempo Por Delta Em Série Temporal
#' 
#' Calcula índice no sistema de tempo de um \code{ts} especificada para um dado delta
#' 
#' Função interna utilizada no escopo da janela rolante. Calcula o índice temporal do final de uma
#' janela iniciada em \code{ini} com largura \code{delta}.
#' 
#' @param ini indice temporal no formato (x, y) de serie temporal indicando inicio da janela 
#' @param delta numero de observacoes que a janela deve conter 
#' @param freq frequencia/sazonalidade da serie
#' 
#' @return Se forncecidos \code{ini} e \code{delta}, o índice no sistema de tempo \code{ts}
#'     indicando o final da janela

deltats <- function(ini, delta, freq) {

    if(delta > 0) {
        aux <- ts(seq_len(delta + 1), start = ini, freq = freq)
        out <- end(aux)
    } else if(delta < 0) {
        aux <- ts(seq_len(abs(delta) + 1), end = ini, freq = freq)
        out <- start(aux)
    } else {
        out <- ini
    }

    return(out)
}
