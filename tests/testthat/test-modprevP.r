
gera_dados_p_sX <- function(p = 4, n = 80, seed = 1234) {
    set.seed(seed)
    coefs <- seq(.5, .9, length.out = p)
    ns <- as.numeric(table(rep(seq_len(p), length.out = n)))
    ss <- lapply(seq_len(p), function(i) as.numeric(arima.sim(list(ar = coefs[i]), ns[i])))
    ss2 <- lapply(seq_len(max(ns)), function(t) {
        sapply(seq_len(p), function(i) {
            tryCatch(ss[[i]][[t]], error = function(e) return(NA))
        })
    })
    ss2 <- unlist(ss2)
    ss2 <- ts(ss2[!is.na(ss2)], frequency = p)
    return(list(ss, ss2))
}

# ESTIMACOES ---------------------------------------------------------------------------------------

test_that("Estimacao de Modelos Periodicos -- S/ Variavel Explicativa", {

    ll <- gera_dados_p_sX()
    serie_0 <- ll[[1]]
    serie_p <- ll[[2]]

    # SARIMA -------------------------------------------------------------------

    modp     <- estimamodelo(serie_p, "sarima", periodico = TRUE)
    expect_equal(class(modp), c("modprevP", "modprev"))
    expect_equal(names(modp$modelos), as.character(seq_len(4)))

    compmods <- lapply(serie_0, forecast::auto.arima, allowdrift = FALSE)

    compara_coef <- mapply(function(cm, m) all(coef(cm) == coef(m$modelo)), compmods, modp$modelos)
    expect_true(all(compara_coef))

    compara_serie <- mapply(function(cm, m) all(as.numeric(cm$x) == as.numeric(m$modelo$x)),
        compmods, modp$modelos)
    expect_true(all(compara_serie))

    # SS AR1 SAZO --------------------------------------------------------------

    modp <- estimamodelo(serie_p, "ss_ar1_saz", periodico = TRUE)
    expect_equal(class(modp), c("modprevP", "modprev"))

    compmods <- lapply(serie_0, estimamodelo, tipo = "ss_ar1_saz")

    compara_Q <- mapply(function(cm, m) all(cm$modelo["Q"] == m$modelo["Q"]), compmods, modp$modelos)
    expect_true(all(compara_Q))

    compara_H <- mapply(function(cm, m) all(cm$modelo["H"] == m$modelo["H"]), compmods, modp$modelos)
    expect_true(all(compara_H))

    compara_serie <- mapply(function(cm, m) all(as.numeric(cm$x) == as.numeric(m$modelo$x)),
        compmods, modp$modelos)
    expect_true(all(compara_serie))

    # SERIE COMECANDO EM S = 3 -------------------------------------------------

    serie_p2 <- window(serie_p, 1.5)
    modp <- estimamodelo(serie_p2, "sarima", TRUE)
    expect_equal(names(modp$modelos), as.character(seq_len(4)))

    serie_02 <- serie_0
    serie_02[[1]] <- serie_02[[1]][-1]
    serie_02[[2]] <- serie_02[[2]][-1]
    compmods <- lapply(serie_02, forecast::auto.arima, allowdrift = FALSE)

    compara_coef <- mapply(function(cm, m) all(coef(cm) == coef(m$modelo)), compmods, modp$modelos)
    expect_true(all(compara_coef))
})

test_that("Estimacao de Modelos Periodicos -- C/ Variavel Explicativa", {

    serie_0 <- split(datregdin$obs, rep(seq_len(4), length.out = 200))
    varex_0 <- split(datregdin$varex, rep(seq_len(4), length.out = 200))
    serie_p <- ts(datregdin$obs, frequency = 4)
    varex_p <- datregdin$varex

    form <- ~ V1 + V2 + V3

    # REGRESSAO ESTATICA -------------------------------------------------------

    modp <- estimamodelo(serie_p, "reg_lin", periodico = TRUE, formula = form, regdata = varex_p)
    expect_equal(class(modp), c("modprevP", "modprev"))

    compmods <- mapply(function(s, d) estimamodelo(s, "reg_lin", regdata = d, formula = form),
        serie_0, varex_0, SIMPLIFY = FALSE)

    compara_coef <- mapply(function(cm, m) all(coef(cm) == coef(m$modelo)), compmods, modp$modelos)
    expect_true(all(compara_coef))

    compara_serie <- mapply(function(cm, m) all(as.numeric(cm$x) == as.numeric(m$modelo$x)),
        compmods, modp$modelos)
    expect_true(all(compara_serie))

    # REGRESSAO DINAMICA -------------------------------------------------------

    modp <- estimamodelo(serie_p, "ss_reg_din", periodico = TRUE, formula = form, regdata = varex_p)
    expect_equal(class(modp), c("modprevP", "modprev"))

    compmods <- mapply(function(s, d) estimamodelo(s, "ss_reg_din", regdata = d, formula = form),
        serie_0, varex_0, SIMPLIFY = FALSE)

    compara_Q <- mapply(function(cm, m) all(cm$modelo["Q"] == m$modelo["Q"]), compmods, modp$modelos)
    expect_true(all(compara_Q))

    compara_H <- mapply(function(cm, m) all(cm$modelo["H"] == m$modelo["H"]), compmods, modp$modelos)
    expect_true(all(compara_H))

    compara_serie <- mapply(function(cm, m) all(as.numeric(cm$x) == as.numeric(m$modelo$x)),
        compmods, modp$modelos)
    expect_true(all(compara_serie))
})

# PREVISAO -----------------------------------------------------------------------------------------

test_that("Previsao de Modelos Periodicos -- S/ Variavel Explicativa", {

    ll <- gera_dados_p_sX()
    serie_0 <- ll[[1]]
    serie_p <- ll[[2]]

    ll2 <- gera_dados_p_sX(n = 82)
    serie_02 <- ll2[[1]]
    serie_p2 <- ll2[[2]]

    # SARIMA -------------------------------------------------------------------

    # Previsao "simples" -- partindo da primeira estacao

    modp     <- estimamodelo(serie_p, "sarima", periodico = TRUE)
    compmods <- lapply(serie_0, forecast::auto.arima, allowdrift = FALSE)

    prevs <- predict.modprevP(modp, n.ahead = 8)
    comp_prevs <- c(t(sapply(compmods, function(m) as.numeric(forecast(m, h = 2)$mean))))

    expect_equal(tsp(prevs), c(21, 22.75, 4))
    expect_equal(as.numeric(prevs[, 1]), comp_prevs)

    prevs <- predict.modprevP(modp, n.ahead = 10)
    comp_prevs <- c(t(sapply(compmods, function(m) as.numeric(forecast(m, h = 3)$mean))))[1:10]

    expect_equal(tsp(prevs), c(21, 23.25, 4))
    expect_equal(as.numeric(prevs[, 1]), comp_prevs)

    # Comecando de uma estacao qualquer

    modp     <- estimamodelo(serie_p2, "sarima", periodico = TRUE)
    compmods <- lapply(serie_02, forecast::auto.arima, allowdrift = FALSE)

    prevs <- predict.modprevP(modp, n.ahead = 8)
    comp_prevs <- sapply(compmods, function(m) as.numeric(forecast(m, h = 3)$mean))
    comp_prevs <- c(t(comp_prevs))

    expect_equal(tsp(prevs), c(21.5, 23.25, 4))
    expect_equal(as.numeric(prevs[, 1]), comp_prevs[c(3:10)])

    prevs <- predict.modprevP(modp, n.ahead = 10)
    comp_prevs <- sapply(compmods, function(m) as.numeric(forecast(m, h = 3)$mean))
    comp_prevs <- c(t(comp_prevs))

    expect_equal(tsp(prevs), c(21.5, 23.75, 4))
    expect_equal(as.numeric(prevs[, 1]), comp_prevs[c(3:12)])
})

test_that("Previsao de Modelos Periodicos -- C/ Variavel Explicativa", {

    # series teste com inicio em S = 1
    serie_0 <- split(head(datregdin$obs, 160), rep(seq_len(4), length.out = 160))
    varex_0_i <- split(head(datregdin$varex, 160), rep(seq_len(4), length.out = 160))
    varex_0_o <- split(tail(datregdin$varex, 40), rep(seq_len(4), length.out = 40))

    serie_p <- ts(head(datregdin$obs, 160), frequency = 4)
    varex_p_i <- head(datregdin$varex, 160)
    varex_p_o <- tail(datregdin$varex, 40)

    # series teste com inicio em S = 3
    serie_02 <- split(head(datregdin$obs, 163), rep(seq_len(4), length.out = 163))
    varex_0_i2 <- split(head(datregdin$varex, 163), rep(seq_len(4), length.out = 163))
    varex_0_o2 <- split(tail(datregdin$varex, 37), rep(c(4, 1, 2, 3), length.out = 37))

    serie_p2 <- ts(head(datregdin$obs, 163), frequency = 4)
    varex_p_i2 <- head(datregdin$varex, 163)
    varex_p_o2 <- tail(datregdin$varex, 37)

    form <- ~ V1 + V2 + V3

    # REGRESSAO ESTATICA -------------------------------------------------------

    # Previsao "simples" -- partindo da primeira estacao

    modp     <- estimamodelo(serie_p, "reg_lin", periodico = TRUE, formula = form, regdata = varex_p_i)
    compmods <- mapply(function(s, d) estimamodelo(s, "reg_lin", regdata = d, formula = form),
        serie_0, varex_0_i, SIMPLIFY = FALSE)

    prevs <- predict.modprevP(modp, n.ahead = 8, newdata = varex_p_o)
    comp_prevs <- mapply(compmods, varex_0_o, FUN = function(mod, nd) {
        predict(mod, newdata = nd, n.ahead = 2)
    }, SIMPLIFY = FALSE)
    comp_prevs <- do.call(rbind, comp_prevs)[c(1, 3, 5, 7, 2, 4, 6, 8), ]

    expect_equal(tsp(prevs), c(41, 42.75, 4))
    expect_equal(as.numeric(prevs), as.numeric(comp_prevs))

    prevs <- predict.modprevP(modp, n.ahead = 10, newdata = varex_p_o)
    comp_prevs <- mapply(compmods, varex_0_o, FUN = function(mod, nd) {
        predict(mod, newdata = nd, n.ahead = 3)
    }, SIMPLIFY = FALSE)
    comp_prevs <- do.call(rbind, comp_prevs)[c(1, 4, 7, 10, 2, 5, 8, 11, 3, 6), ]

    expect_equal(tsp(prevs), c(41, 43.25, 4))
    expect_equal(as.numeric(prevs), as.numeric(comp_prevs))

    # Comecando de uma estacao qualquer

    modp     <- estimamodelo(serie_p2, "reg_lin", periodico = TRUE, formula = form, regdata = varex_p_i2)
    compmods <- mapply(function(s, d) estimamodelo(s, "reg_lin", regdata = d, formula = form),
        serie_02, varex_0_i2, SIMPLIFY = FALSE)

    prevs <- predict.modprevP(modp, n.ahead = 8, newdata = varex_p_o2)
    comp_prevs <- mapply(compmods, varex_0_o2, FUN = function(mod, nd) {
        predict(mod, newdata = nd, n.ahead = 2)
    }, SIMPLIFY = FALSE)
    comp_prevs <- do.call(rbind, comp_prevs)[c(7, 1, 3, 5, 8, 2, 4, 6), ]

    expect_equal(tsp(prevs), c(41.75, 43.5, 4))
    expect_equal(as.numeric(prevs), as.numeric(comp_prevs))

    prevs <- predict.modprevP(modp, n.ahead = 10, newdata = varex_p_o2)
    comp_prevs <- mapply(compmods, varex_0_o2, FUN = function(mod, nd) {
        predict(mod, newdata = nd, n.ahead = 3)
    }, SIMPLIFY = FALSE)
    comp_prevs <- do.call(rbind, comp_prevs)[c(10, 1, 4, 7, 11, 2, 5, 8, 12, 3), ]

    expect_equal(tsp(prevs), c(41.75, 44, 4))
    expect_equal(as.numeric(prevs), as.numeric(comp_prevs))
})

# ATUALIZACAO --------------------------------------------------------------------------------------

test_that("Atualizacao de Modelos Periodicos -- S/ Variavel Explicativa _ Init S = 1", {

    ll <- gera_dados_p_sX(n = 160)
    ll[[1]] <- lapply(seq(ll[[1]]), function(i) ts(ll[[1]][[i]], start = 1 + (i - 1) * .25, freq = 1))

    serie_0 <- lapply(seq(ll[[1]]), function(i) window(ll[[1]][[i]],
        start = 1 + (i - 1) * .25, end = 20 + (i - 1) * .25))
    serie_p <- window(ll[[2]], 1, 20.75)

    serie_02 <- lapply(seq(ll[[1]]), function(i) window(ll[[1]][[i]], start = 21 + (i - 1) * .25))
    serie_p2 <- window(ll[[2]], 21)

    # SARIMA -------------------------------------------------------------------

    modp     <- estimamodelo(serie_p, "sarima", periodico = TRUE)
    modp_upd <- update.modprevP(modp, newseries = serie_p2, refit = FALSE)

    equal_mods <- mapply(modp$modelos, modp_upd$modelos, FUN = function(m, mu) {
        identical(coef(m$modelo), coef(mu$modelo))
    })
    expect_true(all(equal_mods))

    upd_ss <- mapply(modp_upd$modelos, serie_02, FUN = function(m, s2) {
        all(m$serie == s2)
    })
    expect_true(all(upd_ss))

    expect_equal(attr(modp_upd, "mod_atrs")$tsp, c(21, 40.75, 4))
})

test_that("Atualizacao de Modelos Periodicos -- S/ Variavel Explicativa _ Init S = 2", {

    ll <- gera_dados_p_sX(n = 160)
    ll[[1]] <- lapply(seq(ll[[1]]), function(i) ts(ll[[1]][[i]], start = 1 + (i - 1) * .25, freq = 1))

    posit <- c(21.25, 21.25, 21, 21)
    serie_0 <- lapply(seq(ll[[1]]), function(i) window(ll[[1]][[i]],
        start = 1 + (i - 1) * .25, end = posit[i]))
    serie_p <- window(ll[[2]], 1, 21.25)

    serie_02 <- lapply(seq(ll[[1]]), function(i) window(ll[[1]][[i]], posit[i] + .25))
    serie_p2 <- window(ll[[2]], 21.5)

    # SARIMA -------------------------------------------------------------------

    # SEM REFIT

    modp     <- estimamodelo(serie_p, "sarima", periodico = TRUE)
    modp_upd <- update.modprevP(modp, newseries = serie_p2, refit = FALSE)

    equal_mods <- mapply(modp$modelos, modp_upd$modelos, FUN = function(m, mu) {
        identical(coef(m$modelo), coef(mu$modelo))
    })
    expect_true(all(equal_mods))

    upd_ss <- mapply(modp_upd$modelos, serie_02, FUN = function(m, s2) {
        all(m$serie == s2)
    })
    expect_true(all(upd_ss))

    expect_equal(attr(modp_upd, "mod_atrs")$tsp, c(21.5, 40.75, 4))

    # COM REFIT

    modp_upd <- update.modprevP(modp, newseries = serie_p2, refit = TRUE)
    compmods <- lapply(serie_02, forecast::auto.arima, allowdrift = FALSE)

    equal_mods <- mapply(modp_upd$modelos, compmods, FUN = function(m, mu) {
        identical(coef(m$modelo), coef(mu))
    })
    expect_true(all(equal_mods))

    upd_ss <- mapply(modp_upd$modelos, serie_02, FUN = function(m, s2) {
        all(m$serie == s2)
    })
    expect_true(all(upd_ss))

    expect_equal(attr(modp_upd, "mod_atrs")$tsp, c(21.5, 40.75, 4))
})