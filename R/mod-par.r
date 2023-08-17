####################################################################################################
# MODELO PAR(p)[-A]
####################################################################################################

#' Modelos \code{par}
#' 
#' Estimação e métodos de modelos da classe \code{par}
#' 
#' Modelos periódicos autorregressivos, possivelmente com parcela anual, similares aos do GEVAZP
#' 
#' @name modelos_par
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
#' @return Objeto da classe \code{modprev} e subclasse \code{par}, uma lista de dois 
#'     elementos: \code{modelo} e \code{serie} contendo o modelo estimado e a série passada
#' 
#' @rdname modelos_par

par <- function(serie, s = frequency(serie), p = "auto", A12 = FALSE, max.p = 11, ...) {

    attrs <- list(p = p, A12 = A12, max.p = max.p)

    if (s == 1) stop("'serie' nao possui sazonalidade -- informe uma serie sazonal ou um periodo 's'")
    if ((frequency(serie) == 1) && (s > 1)) serie <- ts(serie, frequency = s)

    if (length(p) < s) p <- rep(p, length.out = s)
    if (length(max.p) < s) max.p <- rep(max.p, length.out = s)

    serie0 <- serie

    anyauto <- any(p == "auto")

    medias <- medias_sazo(serie)
    medias <- scale_by_season(medias)
    serie  <- scale_by_season(serie)

    if (anyauto) {
        auto_p <- which(p == "auto")
        auto_p <- sapply(auto_p, function(ap) idordem(serie, ap, max.p[ap], A12, medias))
        p[p == "auto"] <- auto_p
        p <- as.numeric(p)
    }

    coefs  <- lapply(seq_len(s), function(m) fitpar(serie, medias, m, p[m], A12))
    classe <- ifelse(A12, "parA", "par")
    attrs  <- c(attrs, attributes(serie)[c("medias", "desvpads")])
    new_modprevU(list(coefs = coefs), serie0, classe, attrs)
}

# METODOS ------------------------------------------------------------------------------------------

#' @param object objeto com classes \code{c("par"|"parA", "modprev")} contendo modelo
#' @param n.ahead número de passos à frente para previsão
#' @param ... nao possui uso, existe apenas para consistencia com a generica
#' 
#' @return \code{predict} retorna uma série temporal multivariada contendo valor esperado e desvio 
#'     padrão da previsão \code{n.ahead} passos à frente;
#' 
#' @rdname modelos_par
#' 
#' @export

predict.par <- function(object, n.ahead, ...) {

    mod_atrs <- attr(object, "mod_atrs")

    pred  <- double(n.ahead)
    coefs <- object$modelo$coefs
    ords  <- sapply(coefs, length)
    serie <- scale_by_season(object$serie)

    meds <- mod_atrs$medias
    sds  <- mod_atrs$desvpads

    inds <- rep(seq_along(coefs), length.out = n.ahead)
    for (t in seq_len(n.ahead)) {
        m <- inds[t]
        coef_m <- coefs[[m]]
        ord_m  <- ords[m]
        ultval <- tail(serie, ord_m)

        tp1   <- sum(coef_m * ultval)
        serie <- c(serie, tp1)

        tp1 <- tp1 * sds[m] + meds[m]
        pred[t] <- tp1
    }
    pred <- cbind("pred" = pred, "sd" = rep(NA_real_, n.ahead))

    tsp <- tsp(object$serie)
    tsp[1] <- tsp[2] + 1 / tsp[3]
    pred <- ts(pred, start = tsp[1], frequency = tsp[3])

    return(pred)
}

#' @rdname modelos_par
#' 
#' @export

predict.parA <- function(object, n.ahead, ...) {

    mod_atrs <- attr(object, "mod_atrs")

    serie <- object$serie
    serie_s <- scale_by_season(serie)
    freq    <- frequency(serie)

    pred  <- double(n.ahead)
    coefs <- object$modelo$coefs
    ords  <- sapply(coefs, length)

    meds <- mod_atrs$medias
    sds  <- mod_atrs$desvpads

    inds <- rep(seq_along(coefs), length.out = n.ahead)
    for (t in seq_len(n.ahead)) {
        m <- inds[t]
        ord_m  <- ords[m] - 1 # menos a parcela A
        coef_m <- coefs[[m]]

        ultmed <- (mean(tail(serie, freq)) - meds[m]) / sds[m]
        ultval <- c(tail(serie_s, ord_m), ultmed)

        tp1 <- sum(coef_m * ultval)
        serie_s <- c(serie_s, tp1)

        tp1 <- tp1 * sds[m] + meds[m]
        serie   <- c(serie, tp1)
        pred[t] <- tp1
    }
    pred <- cbind("pred" = pred, "sd" = rep(NA_real_, n.ahead))

    tsp <- tsp(object$serie)
    tsp[1] <- tsp[2] + 1 / tsp[3]
    pred <- ts(pred, start = tsp[1], frequency = tsp[3])

    return(pred)
}

# AUXILIARES ---------------------------------------------------------------------------------------

#' Estima PAR(p)s Individuais
#' 
#' Função interna para estimação de cada um dos modelos periódicos pelo método Yule-Walker

fitpar <- function(serie, medias, m, p, A12) {
    if (A12) {
        fit <- percacf(serie, medias, m, p)
        SIGMA <- fit$SIGMA
        ind <- c(2:(p + 1), nrow(SIGMA))
        rho <- SIGMA[ind, 1]
        RHO <- SIGMA[ind, ind]
        phi <- solve(RHO, rho)
        names(phi) <- paste0("phi_", seq_along(phi))
        names(phi)[length(phi)] <- "phi_A"
    } else {
        fit <- perpacf(serie, m, p)
        phi <- solve(fit$RHO[1:p, 1:p], fit$rho[1:p])
        names(phi) <- paste0("phi_", seq_along(phi))
    }

    return(phi)
}

#' Identificação De Ordem Automática
#' 
#' Função interna para identificação da ordem de um modelo periódico individual
#' 
#' @param serie serie temporal
#' @param m o mes para o qual identificar a ordem
#' @param max.p maxima ordem para testar na identificacao
#' @param A12 booleano indicando o uso de parcela de medias moveis
#' @param medias opcional, serie temporal de medias moveis. So tem uso caso \code{A12 = TRUE}
#' 
#' @return escalar inteiro correspondendo à ordem identificada

idordem <- function(serie, m, max.p, A12 = FALSE, medias = NULL) {
    if (A12) {
        phis <- percacf(serie, medias, m, max.p, FALSE)
    } else {
        phis <- perpacf(serie, m, max.p, FALSE)
    }
    conf <- qnorm((1 + .95) / 2) / sqrt(phis$n.used)
    ordem <- phis$phi >= conf
    ordem <- max(which(ordem))
    if (length(ordem) == 0) ordem <- 1
    return(ordem)
}

#' Autocorrelação Parcial/Condicional Periódica
#' 
#' Calcula e opcionalmente plota a ACF parcial ou condicional de uma determinada série sazonal
#' 
#' @param serie serie temporal da qual calcular autocorrelações
#' @param medias serie temporal com mesmas características de \code{serie} contendo as medias moveis
#' @param m inteiro indicando o periodo da sazonalidade para o qual calcular autocorrelacoes
#' @param lag.max inteiro indicando o maior lag para calcular a autocorrelação, limitado em 11
#' @param plot booleano indicando se deve ser feito o plot das autocorrelações calculadas

perpacf <- function(serie, m, lag.max = 6, plot = FALSE) {

    serie <- matrix(serie, ncol = frequency(serie), byrow = TRUE)

    N   <- nrow(serie)
    RHO <- diag(1, lag.max, lag.max)

    for (i in seq_len(lag.max)) {

        # Identifica a coluna correspondente ao lag i do mes m
        col1 <- ifelse((m - i) < 1, m - i + 12, m - i)

        for (j in seq_len(lag.max)[-seq_len(i)]) {

            # Identifica a serie lag
            col2 <- ifelse((col1 - (j - i)) < 1, col1 - (j - i) + 12, col1 - (j - i))

            if (col1 < col2) {
                vec1 <- serie[2:N, col1]
                vec2 <- serie[1:(N - 1), col2]
                RHO[i, j] <- 1 / N * sum(vec1 * vec2) / (sd2(serie[, col1]) * sd2(serie[, col2]))
                RHO[j, i] <- RHO[i, j]
            } else {
                vec1 <- serie[, col1]
                vec2 <- serie[, col2]
                RHO[i, j] <- mean(vec1 * vec2) / (sd2(vec1) * sd2(vec2))
                RHO[j, i] <- RHO[i, j]
            }
        }
    }

    # Calcula o vetor de correlacoes lagged do mes m
    rho <- double(lag.max)
    for (i in 1:lag.max) {
        col2 <- ifelse((m - i) < 1, m - i + 12, m - i)

        if (m < col2) {
            vec1 <- serie[2:N, m]
            vec2 <- serie[1:(N - 1), col2]
            rho[i] <- 1 / N * sum(vec1 * vec2) / (sd2(serie[, m]) * sd2(serie[, col2]))
        } else {
            vec1 <- serie[, m]
            vec2 <- serie[, col2]
            rho[i] <- mean(vec1 * vec2) / (sd2(vec1) * sd2(vec2))
        }
    }

    phi <- sapply(seq_len(lag.max), function(lag) solve(RHO[1:lag, 1:lag], rho[1:lag])[lag])
    phi <- list(phi = phi, n.used = nrow(serie), m = m, lag.max = lag.max, rho = rho, RHO = RHO)
    class(phi) <- c("parcial", "modprev_acf")

    if (plot) print(plot(phi))

    return(phi)
}

#' @rdname perpacf

percacf <- function(serie, medias, m, lag.max = 6, plot = FALSE) {

    serie    <- matrix(serie, ncol = frequency(serie), byrow = TRUE)
    medias_m <- matrix(medias, ncol = frequency(medias), byrow = TRUE)[-1, m]
    N <- nrow(serie)

    # Identifica coluna do lag do mes m e intermediarias
    cols <- m - 1:lag.max
    cols <- cols + 12 * (cols < 1)
    cols <- c(m, cols)

    SIGMA <- diag(1, lag.max + 2, lag.max + 2)

    # Preenche o resto da matriz
    for (i in seq_along(cols)) {

        # Identifica a coluna correspondente ao lag i do mes m
        col1 <- cols[i]

        # Calcula as correlacoes para preencher a matriz SIGMA
        for (j in seq_along(cols)[-seq_len(i)]) {

            # Identifica a serie lag
            col2 <- cols[j]

            # Calcula a covariancia e salva na posicao adequada da matriz
            vec1 <- serie[(1 + (col1 < col2)):N, col1]
            vec2 <- serie[1:(N - (col1 < col2)), col2]
            SIGMA[i, j] <- (N^-1) * sum(vec1 * vec2)
            SIGMA[j, i] <- SIGMA[i, j]
        }

        # Calcula covariancia com medias
        SIGMA[i, ncol(SIGMA)] <- (N^-1) * sum(serie[(1 + (i <= m)):(N - (i > m)), col1] * c(medias_m))
        SIGMA[nrow(SIGMA), i] <- SIGMA[i, ncol(SIGMA)]
    }

    # Calcula correlacoes condicionais
    phi <- double(lag.max)
    for (lag in seq_len(lag.max)) {

        # Indicies das submatrizes
        ind11 <- c(1, lag + 1)
        ind22 <- c((2:lag) * (lag > 1), ncol(SIGMA))

        # Checa se existe algo para condicionar
        if (all(ind22 == 0)) {
            phi[lag] <- SIGMA[1, lag + 1] / sqrt(SIGMA[1, 1] * SIGMA[lag + 1, lag + 1])
            next
        }

        # Calcula correlacoes condicionais
        SIG11 <- SIGMA[ind11, ind11]
        SIG12 <- SIGMA[ind11, ind22, drop = FALSE]
        SIG22 <- SIGMA[ind22, ind22, drop = FALSE]
        COND  <- SIG11 - SIG12 %*% solve(SIG22) %*% t(SIG12)

        # Registra resultado
        phi[lag] <- COND[1, 2] / sqrt(COND[1, 1] * COND[2, 2])
    }

    phi <- list(phi = phi, n.used = nrow(serie), m = m, lag.max = lag.max, SIGMA = SIGMA)
    class(phi) <- c("condicional", "modprev_acf")

    # Plota lags pedidos
    if (plot) print(plot(phi))

    # Retorna vetor de autocorrelacoes condicionadas
    return(phi)
}
