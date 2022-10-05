
gera_dados_p_sX <- function(p = 10, n = 20, seed = 1234) {
    set.seed(seed)
    coefs <- seq(.5, .9, length.out = p)
    ss <- lapply(seq_len(p), function(i) as.numeric(arima.sim(list(ar = coefs[i]), n)))
    ss2 <- lapply(seq_len(n), function(t) sapply(seq_len(p), function(i) ss[[i]][[t]]))
    return(list(ss, ss2))
}

# ESTIMACOES ---------------------------------------------------------------------------------------

test_that("Estimacao de Modelos Periodicos -- S/ Variavel Explicativa", {

    ll <- gera_dados_p_sX()
    serie_0 <- ll[[1]]
    serie_p <- ts(unlist(ll[[2]]), frequency = 10)

    # SARIMA -------------------------------------------------------------------

    modp     <- estimamodelo(serie_p, "sarima", periodico = TRUE)
    expect_equal(class(modp), c("modprevP", "modprev"))

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

})

test_that("Estimacao de Modelos Periodicos -- S/ Variavel Explicativa", {

    serie_0 <- split(datregdin$obs, rep(seq_len(10), length.out = 200))
    varex_0 <- split(datregdin$varex, rep(seq_len(10), length.out = 200))
    serie_p <- ts(datregdin$obs, frequency = 10)
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