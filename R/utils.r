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
