####################################################################################################
# MODELO VPAR(p)[-A]
####################################################################################################

#' Modelos \code{vpar}
#' 
#' Estimação e métodos de modelos da classe \code{vpar}
#' 
#' Modelos vetoriais periódicos autorregressivos, possivelmente com parcela anual, similares aos do
#' GEVAZP
#' 
#' @name modelos_vpar
NULL

# ESTIMACAO ----------------------------------------------------------------------------------------

#' \bold{Estimação}:
#' 
#' Assim como modelos PAR, os VPAR são necessariamente ajustados a séries temporais sazonais, com a
#' diferença de que estes são feitos para séries multivariadas. Em termos de identificação e uso dos
#' argumentos da função, todos funcionam exatamente tal qual aquelas de \code{\link{par}}, exceto 
#' por \code{p}.
#' 
#' Há apenas um argumento novo, \code{diag}, correspondendo a forma como o aspecto multivariado
#' é incorporado ao modelo. Caso \code{diag = TRUE}, o modelo e esitmado tal que suas matrizes
#' de parâmetros são bloco diagonais, isto é, cada série depende apenas de seus próprios lags. 
#' Apenas o ruído permanece multivariado, considerando possíveis dependências entre séries.
#' 
#' Caso \code{diag = FALSE} as matrizes de parâmetros são consideradas cheias, introduzindo
#' dependência no sinal entre séries. Como este modelo tende a ficar sobrecarregado de parâmetros,
#' ele é estimado através do método MSGLasso realizando seleção de séries e então dos lags a serem
#' utilizados.
#' 
#' Assim, o argumento \code{p} funciona de forma diferente entre os modos diagonal ou não. No 
#' primeiro ele se comporta tal qual em \code{\link{par}}, com uma adição. Caso seja informado um
#' escalar ou vetor de tamanho igual a sazonalidade de \code{serie}, a mesma informação será usada
#' para cada dimensão da série multivariada. Alternativamente pode ser fornecida uma lista de 
#' \code{p}s e, então, cada elemento da lista será usado para uma dimensão da série. Quando se 
#' estima o modelo não diagonal, \code{p} não tem uso pois a identificação dos lags será feita
#' automaticamente atráves do MSGLasso
#' 
#' @param serie série para ajustar
#' @param s periodo de sazonalidade, caso \code{serie} nao seja uma serie temporal com sazonalidade
#' @param p vetor de comprimento igual a sazonalidade contendo as ordens de cada modelo periódico;
#'     caso tenha comprimento menor que \code{s}, sera reciclado. Ver Detalhes
#' @param max.p vetor de comprimento igual a sazonalidade contendo as máximas ordens para 
#'     identificação automática; será reciclado se necessário. Ignorado se \code{p = "auto"}
#' @param A12 booleano indicando se a parcela anual deve ser incorporada à estimação
#' @param diag booleano indicando o tipo de modelo a ser ajustado. Ver Detalhes
#' @param ... nao possui uso, existe apenas para consistencia com a generica
#' 
#' @return Objeto da classe \code{modprev} e subclasse \code{vpar}, uma lista de dois 
#'     elementos: \code{modelo} e \code{serie} contendo o modelo estimado e a série passada
#' 
#' @rdname modelos_vpar

vpar <- function(serie, s = frequency(serie), p = "auto", A12 = FALSE, max.p = 11, diag = FALSE, ...) {

    M <- ncol(serie)

    if(!is.list(p)) p <- lapply(seq_len(M), function(i) p)
    if(!is.list(A12)) A12 <- lapply(seq_len(M), function(i) A12)
    if(!is.list(max.p)) max.p <- lapply(seq_len(M), function(i) max.p)

    vpar_fun <- ifelse(diag, vpar_diag, vpar_full)
    vpar_fun(serie, s, p, A12, max.p, ...)
}
