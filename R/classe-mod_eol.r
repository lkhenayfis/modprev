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
#'     \item{\code{ss_reg_din}}{Regressão dinâmica}
#' }
#' 
#' Os modelos \code{sarima} são, por natureza, univariados, enquanto ambas as estruturas em espaço
#' de estados podem ser estimadas para séries multivariadas. Este comportamento não é testado nem
#' suportado explicitamente até a presente versão. Consulte as páginas de ajuda de cada 
#' especificação para maiores detalhes acerca de sua estimação, previsão e etc.
#' 
#' A principio \code{serie} pode ser um vetor simples, porém isto não é recomendável por uma série
#' de razões. A principal delas diz respeito à sazonalidade: os modelos \code{sarima} e 
#' \code{ss_ar1_saz} podem ou não conter dinâmicas sazonais, e esta decisão será tomada com base na
#' frequência da série obtida por \code{frequency(serie)}. Embora seja possível estimar modelos 
#' sazonais com séries sem este atributo, são necessárias uma série de implementações extras e por
#' vezes suposições pouco intuitivas acerca do dado. Desta forma este pacote \bold{NECESSITA} que
#' \code{serie} seja uma série temporal com sazonalidade para estimação de modelos com tal dinâmica.
#' Por fim, deve ser notado que se \code{serie} for um vetor, sera convertido internamente para 
#' \code{ts} com \code{start = c(1, 1), frequency = 1}.
#' 
#' \bold{Modelos com variáveis explicativas:}
#' 
#' No caso de modelos que contenham variáveis explicativas, dois argumentos extras se tornam 
#' relevantes: \code{regdata} e \code{formula}. O primeiro deve ser um \code{data.frame}-like 
#' contendo as variáveis utilizadas na modelagem. \code{formula} é um argumento opcional 
#' especificando uma função linear das variáveis em \code{regdata}, sem LHS. Caso este seja omitido, 
#' será estimada uma função simples (somente termos aditivos) de todas as variáveis contidas em 
#' \code{regdata}. Estes argumentos devem ser passados em \code{...}
#' 
#' @param serie série para ajustar
#' @param tipo tipo de modelo a ser ajustado. Ver Detalhes
#' @param ... demais parâmetros passados para as funções de fit específicas de cada modelo. Ver
#'     Detalhes
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
#' # Visualizacao -----------------------------------------
#' 
#' \dontrun{
#' plot(mod_regdin)
#' }
#' 
#' @return Objeto da classe mod_eol e subclasse igual a \code{tipo}, uma lista de dois elementos:
#'     \code{modelo} e \code{serie} contendo o modelo estimado e a série passada
#' 
#' @family Metodos mod_eol
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

#' Contrutor Interno De \code{mod_eol}
#' 
#' Função interna, não deve ser chamada diretamente pelo usuário
#' 
#' @param fit modelo estimado
#' @param serie serie para qual o modelo foi estimado
#' @param tipo string indicando espcificação do modelo
#' 
#' @return Objeto da classe mod_eol e subclasse igual a \code{tipo}, uma lista de dois elementos:
#'     \code{modelo} e \code{serie} contendo o modelo estimado e a série passada 

new_mod_eol <- function(fit, serie, tipo) {
    new <- list(modelo = fit, serie = serie)
    class(new) <- c(tipo, "mod_eol")

    return(new)
}

# METODOS -----------------------------------------------------------------------------------------

#' Previsão De Modelos \code{mod_eol}
#' 
#' Wrapper para previsão de modelos ajustados por \code{\link{estimamodelo}}
#' 
#' A previsão destes modelos funciona da forma mais padrão, com \code{object} contendo um modelo
#' estimado por \code{\link{estimamodelo}} e \code{n.ahead} um inteiro indicando número de passos à
#' frente para prever. Em modelos de séries temporais simples, isto é tudo.
#' 
#' \bold{Modelos com variáveis explicativas:}
#' 
#' No caso de modelos com variáveis explicativas deve ser fornecido um parâmetro \code{newdata} na
#' forma de code{data.frame}-like contendo as variáveis necessárias para o modelo. Usualmente a 
#' previsão será feita tantos passos à frente quanto há observações em \code{newdata}, porém se
#' \code{n.ahead} for fornecido será usado com precedência sobre o número de observações novas.
#' 
#' @param object modelo ajustado através de \code{\link{estimamodelo}}
#' @param n.ahead número de passos à frente para prever
#' @param ... existe apenas para consistência com a genérica
#' 
#' @examples
#' 
#' mod <- estimamodelo(AirPassengers, tipo = "sarima")
#' pred <- predict(mod, n.ahead = 24)
#' 
#' # em modelos de regressao, deve ser passada a variavel explicativa via newdata
#' 
#' serie <- window(datregdin[[1]], 1, 200)
#' varex <- datregdin[1:200, 2, drop = FALSE]
#' mod_regdin <- estimamodelo(serie, regdata = varex, tipo = "ss_reg_din")
#' 
#' newdata <- datregdin[201:230, 2, drop = FALSE]
#' pred <- predict(mod_regdin, newdata = newdata)
#' 
#' @return série temporal multivariada contendo a previsão e o desvio padrão associado para os
#'     passos de tempo \code{1:n.ahead}
#' 
#' @family Metodos mod_eol
#' 
#' @export

predict.mod_eol <- function(object, n.ahead, ...) {
    stop(paste0("Modelo do tipo ", class(object)[1], " nao possui metodo 'predict'"))
}

#' Atualizacao De Modelos \code{mod_eol}
#' 
#' Wrapper para atualizar e possivelmete reajustar modelos \code{mod_eol}
#' 
#' O padrão desta função é simplesmente substituir \code{newseries} no modelo ajustado 
#' \code{object}, isto é, mantendo todos os hiperparâmetros estimados originalmente. Através do 
#' argumento \code{refit} é possível realizar o reajuste do modelo para a nova série.
#' 
#' \bold{Modelos com variáveis explicativas:}
#' 
#' No caso de modelos com variáveis explicativas, deve ser fornecido um argumento \code{newregdata} 
#' na forma de um \code{data.frame}-like contendo as variáveis explicativas necessárias associadas
#' às novas observações em \code{newseries}.
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
#' layout(matrix(1:3))
#' plot(mod_orig)
#' plot(mod_upd)
#' plot(mod_refit)
#' }
#' 
#' # Com variaveis explicativas ---------------------------
#' 
#' serie <- window(datregdin[[1]], 1, 100)
#' varex <- datregdin[1:100, 2, drop = FALSE]
#' mod_orig <- estimamodelo(serie, "ss_reg_din", regdata = varex)
#' 
#' newserie <- window(datregdin[[1]], 101, 200)
#' newvarex <- datregdin[101:200, 2, drop = FALSE]
#' 
#' mod_upd   <- update(mod_orig, newserie, newregdata = newvarex, refit = FALSE)
#' mod_refit <- update(mod_orig, newserie, newregdata = newvarex, refit = TRUE)
#' 
#' \dontrun{
#' layout(matrix(1:3))
#' plot(mod_orig)
#' plot(mod_upd)
#' plot(mod_refit)
#' }
#' 
#' @return modelo com novos dados, possivelmente reajustado
#' 
#' @family Metodos mod_eol
#' 
#' @export

update.mod_eol <- function(object, newseries, refit = FALSE, ...) {
    stop(paste0("Modelo do tipo ", class(object)[1], " nao possui metodo 'update'"))
}
