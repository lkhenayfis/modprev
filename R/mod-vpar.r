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

    M <- ncol(serie)

    if (!is.list(p)) p <- lapply(seq_len(M), function(i) p)
    if (!is.list(max.p)) max.p <- sapply(seq_len(M), function(i) max.p)

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

    mods <- parsecoef_diag(mods, A12, serie)

    return(mods)
}

vpar_full <- function(serie, s, p, A12, max.p, ...) {

    M    <- ncol(serie)
    prep <- prep_msglasso(M, max.p, A12)

    serie  <- scale_by_season(serie)
    medias <- if (A12) scale_by_season(medias_sazo(serie)) else NULL

    sysmats <- lapply(seq_len(s), function(m) {
        lmats <- lapply(seq_len(M), function(i) build_reg_mat(serie[, i], m, max.p[i], medias[, i]))
        ymat <- Reduce(cbind, lapply(lmats, "[[", 1))
        xmat <- Reduce(cbind, lapply(lmats, "[[", 2))
        list(ymat, xmat)
    })

    lambda1s <- exp(seq(log(.001), log(1), length.out = 20))
    lambdaGs <- exp(seq(log(.001), log(1), length.out = 20))

    ncores <- max(length(sysmats), detectCores() - 2)
    clst <- parallel::makeCluster(ncores, "FORK")

    CVs <- parLapply(clst, sysmats, function(sms) {
        Y  <- sms[[1]]
        X  <- sms[[2]]
        cv <- with(prep, MSGLasso.cv(X, Y, grpWTs, Pen_L, Pen_G, PQgrps, GRgrps, lambda1s, lambdaGs))
        lambda1 <- cv$lams.c[which.min(as.vector(cv$rss.cv))][[1]]$lam1
        lambdaG <- cv$lams.c[which.min(as.vector(cv$rss.cv))][[1]]$lam3
        return(list(lambda1, lambdaG))
    })

    mods <- parLapply(clst, seq_along(sysmats), function(i) {
        lam1 <- CVs[[i]][1]
        lamG <- CVs[[i]][2]
        Y    <- sysmats[[i]][[1]]
        X    <- sysmats[[i]][[2]]
        with(prep, MSGLasso(X, Y, grpWTs, Pen_L, Pen_G, PQgrps, GRgrps, grp_Norm0, lam1, lamG))$Beta
    })

    stopCluster(clst)

    mods <- parsecoef_full(mods, max.p, A12, serie)

    return(mods)
}

# METODOS ------------------------------------------------------------------------------------------

#' @param object objeto com classes \code{c("vpar"|"vparA", "modprev")} contendo modelo
#' @param n.ahead número de passos à frente para previsão
#' @param ... nao possui uso, existe apenas para consistencia com a generica
#' 
#' @return \code{predict} retorna uma lista de séries temporais multivariadas contendo valor
#'     esperado e desvio padrão da previsão \code{n.ahead} passos à frente;
#'     
#' 
#' @rdname modelos_vpar
#' 
#' @export

predict.vpar <- function(object, n.ahead, ...) {

    mod_atrs <- attr(object, "mod_atrs")

    M <- ncol(object$serie)

    pred  <- matrix(0, n.ahead, M, dimnames = list(NULL, colnames(serie)))
    coefs <- object$modelo$coefs
    ords  <- sapply(coefs, ncol) / M
    serie <- scale_by_season(object$serie)

    meds <- mod_atrs$scale_serie$medias
    sds  <- mod_atrs$scale_serie$desvpads

    inds <- rep(seq_along(coefs), length.out = n.ahead)
    for (t in seq_len(n.ahead)) {
        m <- inds[t]
        coef_m <- coefs[[m]]
        ord_m  <- ords[m]

        ultval <- tail(serie, ord_m)
        ultval <- matrix(t(ultval[ord_m:1, ]))

        tp1   <- coef_m %*% ultval
        serie <- rbind(serie, t(tp1))

        tp1 <- tp1 * sds[m, ] + meds[m, ]
        pred[t, ] <- t(tp1)
    }
    pred <- list("pred" = pred, "sd" = matrix(NA_real_, n.ahead, M, dimnames = list(NULL, colnames(serie))))

    tsp <- tsp(object$serie)
    tsp[1] <- tsp[2] + 1 / tsp[3]
    pred <- lapply(pred, function(m) ts(m, start = tsp[1], frequency = tsp[3]))

    return(pred)
}

#' @rdname modelos_vpar
#' 
#' @export

predict.vparA <- function(object, n.ahead, ...) {

    mod_atrs <- attr(object, "mod_atrs")

    M <- ncol(object$serie)

    serie <- object$serie
    serie_s <- scale_by_season(serie)
    freq    <- frequency(serie)

    pred  <- matrix(0, n.ahead, M)
    coefs <- object$modelo$coefs
    ords  <- sapply(coefs, ncol) / M

    meds <- mod_atrs$scale_serie$medias
    sds  <- mod_atrs$scale_serie$desvpads

    meds_a <- mod_atrs$scale_A12$medias
    sds_a  <- mod_atrs$scale_A12$desvpads

    inds <- rep(seq_along(coefs), length.out = n.ahead)
    for (t in seq_len(n.ahead)) {
        m <- inds[t]
        coef_m <- coefs[[m]]
        ord_m  <- ords[m] - 1 # menos parcela A

        ultmed <- (colMeans(tail(serie, freq)) - meds_a[m, ]) / sds_a[m, ]

        ultval <- tail(serie_s, ord_m)
        ultval <- rbind(ultmed, ultval)
        ultval <- matrix(t(ultval[(ord_m + 1):1, ]))

        tp1     <- coef_m %*% ultval
        serie_s <- rbind(serie_s, t(tp1))

        tp1 <- tp1 * sds[m, ] + meds[m, ]
        serie <- rbind(serie, t(tp1))
        pred[t, ] <- tp1
    }
    pred <- list("pred" = pred, "sd" = matrix(NA_real_, n.ahead, M, dimnames = list(NULL, colnames(serie))))

    tsp <- tsp(object$serie)
    tsp[1] <- tsp[2] + 1 / tsp[3]
    pred <- lapply(pred, function(m) ts(m, start = tsp[1], frequency = tsp[3]))

    return(pred)
}

#' @param newseries nova série com a qual atualizar o modelo
#' @param refit booleano indicando se o modelo deve ou não ser reajustado
#' @param ... para \code{update} nao tem uso, existe apenas para consistência com a genérica
#' 
#' @return \code{update} retorna modelo com novos dados e, caso \code{refit == TRUE}, reajustado. 
#'     Contrário à função de estimação, \code{update} já retorna o objeto da classe \code{modprev};
#' 
#' @rdname modelos_par
#' 
#' @export

update.vpar <- function(object, newseries, refit = FALSE, ...) {
    mod_atrs <- attr(object, "mod_atrs")
    if (refit) {
        object <- estimamodelo(newseries, "vpar", periodico = FALSE, p = mod_atrs$p,
            A12 = mod_atrs$A12, max.p = mod_atrs$max.p)
    } else {
        newseries_s <- scale_by_season(newseries)
        mod_atrs$scale_serie <- attributes(newseries_s)[c("medias", "desvpads")]

        object$serie <- newseries
        attr(object, "mod_atrs") <- mod_atrs
    }
    return(object)
}

#' @rdname modelos_par
#' 
#' @export

update.vparA <- function(object, newseries, refit = FALSE, ...) {
    object <- update.vpar(object, newseries, refit, ...)

    # sem refit tambem precisa atualizar os parametros de scale da serie media movel
    if (!refit) {
        mod_atrs <- attr(object, "mod_atrs")
        newmedias   <- medias_sazo(newseries)
        newmedias   <- scale_by_season(newmedias)
        mod_atrs$scale_A12 <- attributes(newmedias)[c("medias", "desvpads")]
        attr(object, "mod_atrs") <- mod_atrs
    }

    return(object)
}

# AUXILIARES ---------------------------------------------------------------------------------------

build_reg_mat <- function(serie, m, max.p, medias = NULL) {
    N <- length(serie)
    s <- frequency(serie)

    serie <- shift(c(serie), s - m)
    serie[seq_len(s - m)] <- NA

    sysmat <- matrix(serie, ncol = s, byrow = TRUE)

    ymat <- sysmat[, s, drop = FALSE]
    xmat <- sysmat[, (s - 1):(s - max.p), drop = FALSE]

    if (!is.null(medias)) {
        valmes <- rep(FALSE, s)
        valmes[m] <- TRUE
        medias <- medias[valmes]
        xmat <- unname(cbind(xmat, medias))
    }

    fulllin <- complete.cases(xmat) & complete.cases(ymat)
    ymat <- scale(ymat[fulllin, ])
    xmat <- scale(xmat[fulllin, ])

    return(list(ymat, xmat))
}

prep_msglasso <- function(M, max.p, A12) {

    P <- sum(max.p) + A12 * M # dimensao da matriz de variaveis explicativas
    Q <- M                    # dimensao da matriz de variaveis dependentes
    G <- M                    # numero de grupos de VARIAVEIS EXPLICATIVAS
    R <- 1                    # numero de grupos de VARIAVEIS DEPENDENTES

    gmax <- 1                # maximo numero de grupos que uma mesma variavel faz parte
    cmax <- max(max.p) + A12 # maximo numero de variaveis num grupo

    # variaveis nas quais comecam e terminam cada grupo de regressores
    refs <- cumsum(c(1, max.p) + c(0, rep(A12, M))) - 1
    xgrpstart <- refs[-(M + 1)]
    xgrpend   <- refs[-1] - 1

    # matrizes auxiliares do pacote
    PQgrps <- FindingPQGrps(P, Q, G, R, gmax, xgrpstart, xgrpend, 0, M - 1)$PQgrps
    GRgrps <- FindingGRGrps(P, Q, G, R, cmax, xgrpstart, xgrpend, 0, M - 1)$GRgrps
    grpWTs <- Cal_grpWTs(P, Q, G, R, gmax, PQgrps)$grpWTs

    Pen_L <- matrix(rep(1, P * Q), P, Q, byrow = TRUE)
    Pen_G <- matrix(rep(1, G * R), G, R, byrow = TRUE)
    grp_Norm0 <- matrix(rep(0, G * R), nrow = G, byrow = TRUE)

    out <- list(PQgrps = PQgrps, GRgrps = GRgrps, grpWTs = grpWTs, Pen_L = Pen_L, Pen_G = Pen_G,
        grp_Norm0 = grp_Norm0)

    return(out)
}

parsecoef_diag <- function(mods, A12, serie) {
    mods <- lapply(mods, function(mod) {
        sup.p <- max(sapply(mod, length)) - A12
        ords <- sapply(mod, length) - A12
        mod <- lapply(seq_along(mod), function(j) {
            x <- mod[[j]][seq_len(ords[j])]
            x <- c(x, rep(0, sup.p - length(x)))
            if (A12) c(x, tail(mod[[j]], 1)) else x
        })
        mod <- lapply(seq_len(sup.p + A12), function(p) {
            inner <- sapply(mod, "[[", p)
            diag(inner)
        })
        mod <- do.call(cbind, mod)
        rownames(mod) <- colnames(serie)
        cnames <- c(seq_len(sup.p), ifelse(A12, "A", NA))
        cnames <- cnames[!is.na(cnames)]
        colnames(mod) <- c(outer(colnames(serie), cnames, function(a, b) paste0(a, "_", b)))
        return(mod)
    })
    return(mods)
}

parsecoef_full <- function(mods, max.p, A12, serie) {
    M <- ncol(serie)
    sup.p <- max(max.p) + A12
    refs  <- cumsum(c(1, max.p + A12))
    start <- refs[-(M + 1)]
    end   <- refs[-1] - 1
    mods <- lapply(mods, function(mod) {
        mod <- t(mod)
        mod <- lapply(seq_len(M), function(i) mod[, start[i]:end[i]])
        mod <- lapply(mod, function(mat) cbind(mat, matrix(0, M, sup.p - ncol(mat))))
        mod <- lapply(seq_len(sup.p), function(i) sapply(mod, function(mat) mat[, i]))
        mod <- do.call(cbind, mod)
        rownames(mod) <- colnames(serie)
        cnames <- c(seq_len(sup.p - A12), ifelse(A12, "A", NA))
        cnames <- cnames[!is.na(cnames)]
        colnames(mod) <- c(outer(colnames(serie), cnames, function(a, b) paste0(a, "_", b)))
        mod
    })
    return(mods)
}