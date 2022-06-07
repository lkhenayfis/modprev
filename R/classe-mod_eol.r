####################################################################################################
# SUPERCLASSE ABSTRATA DOS DIFERENTES TIPOS DE MODELOS
####################################################################################################

# CONSTRUTOR ---------------------------------------------------------------------------------------

#' Ajuste De Modelos
#' 
#' Wrapper de estimação para múltiplos modelos com interface e saída unificadas
#' 
#' Esta função facilita a estimação de diversos tipos de modelos com uma única interface e, mais 
#' importante ainda, única estrutura de saída. O tipo de modelo estimado para a série passada 
#' através de \code{serie} é selecionado através do argumento \code{tipo}, podendo ser um de
#' 
#' \describe{
#'     \item{\code{sarima}}{SARIMA(p, d, q)(P, D, Q)}
#'     \item{\code{ss_ar1_saz}}{Espaço de estados composto por processo AR(1) + Sazonalidade}
#'     \item{\code{ss_reg_din}}{Regressão univariada dinâmica}
#' }
#' 
#' Deve ser notado que no caso dos modelos com sazonalidade, o argumento \code{serie} \emph{DEVE SER
#' UM OBJETO SERIE TEMPORAL COM PERIODO ESPECIFICADO}. Isto e necessario para que a função possa 
#' automaticamente lidar com diversos tipos de séries sem a necessidade de demais argumentos.
#' 
#' No caso de modelos com variaveis explicativas deve ser fornecido um parametro \code{regdata} na
#' forma de uma matriz ou data.frame contendo apenas as colunas com variáveis a serem utilizadas. Se
#' houver apenas uma variável explicativa, pode ser passada como um vetor ou série temporal.
#' 
#' @param serie série para ajustar
#' @param tipo tipo de modelo a ser ajustado. Ver Detalhes
#' @param ... demais parâmetros passados para as funções de fit específicas de cada modelo
#' 
#' @examples 
#' 
#' # ajustando tipo SARIMA
#' mod_sarima <- estimamodelo(AirPassengers, tipo = "sarima")
#' 
#' # caso a serie nao possua sazonalidade explicita, o modelo sera ajustado sem isso
#' ss <- arima.sim(200, model = list(ar = .8))
#' mod_sarima_semsazo <- estimamodelo(ss, tipo = "sarima")
#' 
#' \dontrun{
#' # estima so um AR(1) sem sazonalidade
#' coef(mod_sarima_semsazo)
#' }
#' 
#' # ajustando uma regressao dinamica (com dado dummy interno do pacote)
#' serie <- window(datregdin[[1]], 1, 100)
#' varex <- datregdin[1:100, 2, drop = FALSE]
#' mod_regdin <- estimamodelo(serie, regdata = varex, tipo = "ss_reg_din")
#' 
#' @return objeto da classe mod_eol contendo modelo (classe dependente do modelo ajustato), serie
#'     ajustada e, caso \code{out_sample > 0}, a parte reservada para comparação
#' 
#' @family metodos_mod_eol
#' 
#' @export

estimamodelo <- function(serie, tipo, ...) UseMethod("estimamodelo")

#' @export

estimamodelo.numeric <- function(serie, tipo, ...) estimamodelo.ts(ts(serie), tipo, ...)

#' @export

estimamodelo.ts <- function(serie, tipo = c("sarima", "ss_ar1_saz", "ss_reg_din"), ...) {

    tipo <- match.arg(tipo)

    # originalmente isso foi implementado com um eval de match.call trocando o nome da funcao, mas
    # tem uns problemas pra resolver de scoping. Eventualmente essa melhoria precisa ser feita
    fit_mod <- switch(tipo,
        "sarima" = sarima(serie, ...),
        "ss_ar1_saz" = ss_ar1_saz(serie, ...),
        "ss_reg_din" = ss_reg_din(serie, ...)
    )

    out <- new_mod_eol(fit_mod, serie, tipo)

    return(out)
}

new_mod_eol <- function(fit, serie, tipo) {
    new <- list(modelo = fit, serie = serie)
    class(new) <- c(tipo, "mod_eol")

    return(new)
}

# METODOS -----------------------------------------------------------------------------------------

#' Previsão De Modelos \code{mod_eol}
#' 
#' Wrapper para previsão e plot de modelos ajustados por \code{estimamodelo}
#' 
#' No caso de modelos com variáveis explicativas deve ser fornecido um parâmetro \code{newdata} na
#' forma de uma matriz ou data.frame contendo apenas as colunas com variáveis a serem utilizadas. Se
#' houver apenas uma variável explicativa, pode ser passada como um vetor ou série temporal. Nestes
#' modelos, se o argumento \code{n.ahead} também tiver sido fornecido, \code{newdata} será reduzida 
#' às \code{n.ahead} primeiras observações.
#' 
#' @param object modelo ajustado através de \code{\link{estimamodelo}}
#' @param n.ahead numero de passos a frente para prever
#' @param ... existe apenas para consistência com a genérica
#' 
#' @examples
#' 
#' mod <- estimamodelo(AirPassengers, tipo = "sarima")
#' 
#' \dontrun{
#' predict(mod, n.ahead = 24)
#' predict(mod)
#' }
#' 
#' # em modelos de regressao, deve ser passada a variavel explicativa via newdata
#' 
#' serie <- window(datregdin[[1]], 1, 200)
#' varex <- datregdin[1:200, 2, drop = FALSE]
#' mod_regdin <- estimamodelo(serie, regdata = varex, tipo = "ss_reg_din")
#' 
#' \dontrun{
#' newdata <- datregdin[201:230, 2, drop = FALSE]
#' predict(mod_regdin, newdata = newdata)
#' }
#' 
#' @return série temporal multivariada contendo a previsão e o desvio padrão associado
#' 
#' @family metodos_mod_eol
#' 
#' @export

predict.mod_eol <- function(object, n.ahead, ...) {
    stop(paste0("Modelo do tipo ", class(object)[1], " nao possui metodo 'predict'"))
}

#' Atualizacao De Modelos mod_eol
#' 
#' Wrapper para atualizar e possivelmete reajustar modelos \code{mod_eol}
#' 
#' O padrão desta função é simplesmente substituir \code{newseries} no modelo ajustado \code{fit}, 
#' isto é, mantendo todos os hiperparâmetros estimados originalmente. Através do argumento
#' \code{refit} é possível realizar o reajuste do modelo para a nova série.
#' 
#' No caso de modelos com variaveis explicativas deve ser fornecido um parametro \code{newregdata} 
#' na forma de uma matriz ou data.frame contendo apenas as colunas com variáveis a serem utilizadas.
#' Se houver apenas uma variável explicativa, pode ser passada como um vetor ou série temporal.
#' 
#' @param object modelo ajustado atraves de \code{estimamodelo}
#' @param newseries nova serie para associar ao modelo
#' @param refit booleano indicando se o modelo deve ser reajustado
#' @param ... existe apenas para consistência com a genérica
#' 
#' @examples 
#' 
#' serie1 <- window(datregdin[[1]], 1, 300)
#' serie2 <- window(datregdin[[1]], 501, 800)
#' 
#' mod_orig  <- estimamodelo(serie1, tipo = "sarima")
#' mod_upd   <- update(mod_orig, serie2, refit = FALSE)
#' mod_refit <- update(mod_orig, serie2, refit = TRUE)
#' 
#' \dontrun{
#' # comparando os casos
#' coef(mod_orig$modelo)
#' coef(mod_upd$modelo)
#' coef(mod_refit$modelo)
#' }
#' 
#' @return modelo com novos dados e possivelmente reajustado
#' 
#' @family metodos_mod_eol
#' 
#' @export

update.mod_eol <- function(object, newseries, refit = FALSE, ...) {
    stop(paste0("Modelo do tipo ", class(object)[1], " nao possui metodo 'update'"))
}
