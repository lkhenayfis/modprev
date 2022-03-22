####################################################################################################
# MODELO ESPACO DE ESTADOS AR(1) + SAZO
####################################################################################################

#' Modelos \code{ss_ar1_saz}
#' 
#' Estimacao e metodos de modelos da classe \code{ss_ar1_saz}
#' 
#' @name modelos_ss_ar1_saz
NULL

# ESTIMACAO ----------------------------------------------------------------------------------------

#' @param serie serie para ajustar
#' 
#' @return \code{ss_ar1_saz} retorna modelo espaco de estados AR(1) + SAZ estimado
#' 
#' @rdname modelos_ss_ar1_saz

ss_ar1_saz <- function(serie, ...) {

    Z <- matrix(c(1, 1), 1)
    T <- matrix(c(1, 0, 0, 0), 2)
    R <- matrix(c(0, 1), 2)

    if(frequency(serie) == 1) {
        mod <- SSModel(serie ~ -1 +
            SSMcustom(Z = Z, T = T, R = R, a1 = c(1, 0), Q = NA),
            H = 0)
    } else {
        mod <- SSModel(serie ~ -1 +
            SSMcustom(Z = Z, T = T, R = R, a1 = c(1, 0), Q = NA) +
            SSMseasonal(period = frequency(serie), sea.type = "dummy", Q = NA),
            H = 0)
    }
    upfunc <- function(par, model) {
        model["Z", "custom"][1] <- par[1]
        model["T", "custom"][2, 2] <- par[2] / sqrt(1 + par[2]^2)
        model["Q", etas = "custom"] <- exp(par[3])
        model["Q", etas = "seasonal"] <- exp(par[4])
        model["P1", "custom"][2, 2] <- exp(par[3]) / (1 - (par[2] / sqrt(1 + par[2]^2))^2)
        model
    }
    fit <- fitSSM(mod, inits = c(mean(serie), 0, 0, 0), updatefn = upfunc, method = "BFGS")

    if(abs(logLik(fit$model)) < 1e-10) {
        fit$model$Z[] <- NA
    }

    return(fit$model)
}

# METODOS ------------------------------------------------------------------------------------------

#' @param object objeto com classes \code{c("ss_ar1_saz", "mod_eol")} contendo modelo
#' @param n.ahead numero de passos a frente para previsao
#' 
#' @return \code{predict} serie temporal multivariada contendo valor esperado e desvio padrao de
#'     previsao;
#' 
#' @rdname modelos_ss_ar1_saz
#' 
#' @export

predict.ss_ar1_saz <- function(object, n.ahead, ...) {
    modelo <- object$modelo

    prev <- predict(modelo, n.ahead = n.ahead, se.fit = TRUE, filter = TRUE, ...)
    colnames(prev) <- c("prev", "sd")

    return(prev)
}

#' @details 
#' 
#' \bold{Update}:
#' 
#' A atualizacao de modelos \code{ss_ar1_saz} sempre vai checar se o modelo passado foi estimado
#' corretamente. Como modelos em espaco de estados dependem bastante de inicializacao, as vezes nao
#' da pra estimar direito. Nesses casos ele tenta reestimar o modelo independentemente de 
#' \code{refit}
#' 
#' @param newseries nova serie com a qual atualizar o modelo
#' @param refit booleano indicando se o modelo deve ou nao ser reajustado
#' 
#' @return \code{update} retorna modelo com novos dados e, caso \code{refit == TRUE}, reajustado;
#' 
#' @rdname modelos_ss_ar1_saz
#' 
#' @export

update.ss_ar1_saz <- function(object, newseries, refit = FALSE, ...) {

    if(refit) {
        object <- estimamodelo(newseries, "ss_ar1_saz")
    } else {
        modelo <- object$modelo

        # Se modelo nao convergiu, tenta reestimar
        if(all(is.na(modelo$Z))) return(estimamodelo(newseries, tipo = "ss_ar1_saz")$modelo)

        # Do contrario, atualiza normalmente
        modelo$y <- newseries
        attr(modelo$y, "dim") <- c(length(newseries), 1)
        attr(modelo, "n") <- as.integer(length(newseries))

        object$modelo <- modelo
    }

    return(object)
}