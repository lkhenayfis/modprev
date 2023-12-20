####################################################################################################
# MODELO ESPACO DE ESTADOS AR(1) + SAZO
####################################################################################################

#' Modelos \code{ss_ar1_saz}
#' 
#' Estimação e métodos de modelos da classe \code{ss_ar1_saz}
#' 
#' Esta especificação corresponde a um modelo em espaço de estados com um estado AR1 e outro de 
#' sazonalidade correspondente à frequência de \code{serie}. O coeficiente autoregressivo será 
#' estimado sob restrição de estacionariedade, assumindo valores necessariamente entre -1 e 1.
#' 
#' @name modelos_ss_ar1_saz
NULL

# ESTIMACAO ----------------------------------------------------------------------------------------

#' @param serie serie para ajustar
#' @param ... nao possui uso, existe apenas para consistencia com a generica
#' 
#' @return Objeto da classe \code{modprev} e subclasse \code{ss_ar1_saz}, uma lista de dois 
#'     elementos: \code{modelo} e \code{serie} contendo o modelo estimado e a série passada
#' 
#' @rdname modelos_ss_ar1_saz

ss_ar1_saz <- function(serie, ...) {

    arrZ <- matrix(c(1, 1), 1)
    arrT <- matrix(c(1, 0, 0, 0), 2)
    arrR <- matrix(c(0, 1), 2)

    if(frequency(serie) == 1) {
        mod <- SSModel(serie ~ -1 +
                SSMcustom(Z = arrZ, T = arrT, R = arrR, a1 = c(1, 0), Q = NA),
            H = 0)
    } else {
        mod <- SSModel(serie ~ -1 +
                SSMcustom(Z = arrZ, T = arrT, R = arrR, a1 = c(1, 0), Q = NA) +
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

    out <- new_modprevU(fit$model, serie, "ss_ar1_saz")

    return(out)
}

# METODOS ------------------------------------------------------------------------------------------

#' \bold{Predict}:
#' 
#' A previsão destes modelos é feita com alguns argumetnos opcionais já passados por padrão que
#' não podem ser modificados. Estes são: \code{se.fit = TRUE, filter = TRUE}. O primeiro retorna
#' além do valor previsto o desvio padrão associado, o segundo garante que são retornados os valores
#' advindos da distribuição preditiva e não de suavização. Considerando estes fatores, \code{...} 
#' não pode conter \code{n.ahead} ou estes dois outros, ou então ocorrerá erro.
#' 
#' @param object objeto com classes \code{c("ss_ar1_saz", "modprev")} contendo modelo
#' @param n.ahead número de passos à frente para previsão
#' @param ... demais argumentos passados a \code{\link[KFAS]{predict.SSModel}}. Ver Detalhes
#' 
#' @return \code{predict} retorna uma série temporal multivariada contendo valor esperado e desvio 
#'     padrao da previsão \code{n.ahead} passos à frente;
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
#' A atualização de modelos \code{ss_ar1_saz} sempre vai checar se o modelo passado foi estimado
#' corretamente. Como modelos em espaçoo de estados dependem bastante de inicialização, às vezes não
#' dá para estimar direito. Nesses casos ele tenta reestimar o modelo independentemente de 
#' \code{refit}
#' 
#' @param newseries nova série com a qual atualizar o modelo
#' @param refit booleano indicando se o modelo deve ou não ser reajustado
#' @param ... existe apenas para consistência com a genérica
#' 
#' @return \code{update} retorna modelo com novos dados e, caso \code{refit == TRUE}, reajustado. 
#'     Contrário à função de estimação, \code{update} já retorna o objeto da classe \code{modprev};
#' 
#' @rdname modelos_ss_ar1_saz
#' 
#' @export

update.ss_ar1_saz <- function(object, newseries, refit = FALSE, ...) {

    if(refit) {
        object <- estimamodelo(newseries, "ss_ar1_saz")
    } else {
        modelo <- object$modelo

        # Se modelo não convergiu, tenta reestimar
        if(all(is.na(modelo$Z))) return(estimamodelo(newseries, tipo = "ss_ar1_saz")$modelo)

        # Do contrario, atualiza normalmente
        modelo$y <- newseries
        attr(modelo$y, "dim") <- c(length(newseries), 1)
        attr(modelo, "n") <- as.integer(length(newseries))

        newseries  <- if(is.ts(newseries)) newseries else ts(newseries)

        object <- new_modprevU(modelo, newseries, "ss_ar1_saz")
    }

    return(object)
}