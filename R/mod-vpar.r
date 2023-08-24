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

    attrs <- list(p = p, A12 = A12, max.p = max.p, diag = diag)

    if (s == 1) stop("'serie' nao possui sazonalidade -- informe uma serie sazonal ou um periodo 's'")
    if ((frequency(serie) == 1) && (s > 1)) serie <- ts(serie, frequency = s)

    if (!is.list(p)) p <- lapply(seq_len(M), function(i) p)
    if (!is.list(max.p)) max.p <- sapply(seq_len(M), function(i) max.p)

    M <- ncol(serie)

    vpar_fun <- ifelse(diag, vpar_diag, vpar_full)
    coefs    <- vpar_fun(serie, s, p, A12, max.p, ...)

    scale_serie <- attributes(scale_by_season(serie))[c("medias", "desvpads")]
    attrs <- c(attrs, list(scale_serie = scale_serie))

    if (A12) {
        scale_medias <- attributes(scale_by_season(medias_sazo(serie)))[c("medias", "desvpads")]
        attrs <- c(attrs, list(scale_A12 = scale_serie))
    }

    classe <- ifelse(A12, "vparA", "vpar")
    new_modprevU(list(coefs = coefs), serie, classe, attrs)
}

vpar_diag <- function(serie, s, p, A12, max.p, ...) {
    M <- ncol(serie)
    mods <- lapply(seq_len(M), function(m) {
        par(serie[, m], p = p[[m]], A12 = A12, max.p = max.p[m])$modelo$coefs
    })
    mods <- lapply(seq_len(s), function(i) lapply(mods, function(mod) mod[[i]]))
    mods <- lapply(seq_along(mods), function(i) {
        mod <- mods[[i]]
        outer <- lapply(seq_along(mod), function(j) {
            coef <- mod[[j]]
            n <- length(coef)
            inner <- matrix(0, M, n)
            inner[j, ] <- coef
            dimnames(inner) <- list(colnames(serie), paste0(colnames(serie)[j], "_", seq_len(n)))
            inner
        })
        do.call(cbind, outer)
    })
    return(mods)
}

# AUXILIARES ---------------------------------------------------------------------------------------

build_reg_mat <- function(serie, m, max.p) {
    N <- length(serie)
    s <- frequency(serie)

    serie <- shift(c(serie), s - m)
    serie[seq_len(s - m)] <- NA

    sysmat <- matrix(serie, ncol = s, byrow = TRUE)

    ymat <- sysmat[, s, drop = FALSE]
    xmat <- sysmat[, (s - 1):(s - max.p), drop = FALSE]

    return(list(ymat, xmat))
}
