####################################################################################################
# ESTIMACAO DE MODELOS
####################################################################################################

#' Ajuste De Modelos
#' 
#' Wrapper de estimação para múltiplos modelos com interface e saída unificadas
#' 
#' Esta função facilita a estimação de diversos tipos de modelos com uma única interface e, mais 
#' importante ainda, única estrutura de saída. O tipo de modelo estimado para a série passada 
#' através de \code{serie} é selecionado através do argumento \code{tipo}, podendo ser um de
#' 
#' \describe{
#'     \item{\code{reg_lin}}{Regressao linear comum}
#'     \item{\code{reg_quant}}{Regressao linear quantílica}
#'     \item{\code{GAM}}{Modelos Aditivos Generalizados}
#'     \item{\code{BOOST}}{Modelagem por boosting de modelos simples}
#'     \item{\code{sarima}}{SARIMA(p, d, q)(P, D, Q)}
#'     \item{\code{sarimax}}{SARIMAX(p, d, q)(P, D, Q)}
#'     \item{\code{ss_ar1_saz}}{Espaço de estados composto por processo AR(1) + Sazonalidade}
#'     \item{\code{ss_reg_din}}{Regressão dinâmica}
#' }
#' 
#' Os modelos \code{sarima} são, por natureza, univariados, enquanto ambas as estruturas em espaço
#' de estados podem ser estimadas para séries multivariadas. Este comportamento não é testado nem
#' suportado explicitamente até a presente versão. Consulte as páginas de ajuda de cada 
#' especificação para maiores detalhes acerca de sua estimação, previsão e etc.
#' 
#' Modelos \code{sarimax} podem ser estimados especificando \code{tipo} como \code{"sarima"} ou 
#' \code{"sarimax"}. No primeiro caso, se for informado um argumento \code{regdata} no momento da 
#' estimacção, o código redireciona para \code{sarimax} automaticamente. 
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
#' \bold{Modelos periódicos:}
#' 
#' Se o argumento \code{periodico} for \code{TRUE}, serao estimados S modelos individuais, onde S 
#' representa o número de estações em \code{serie}. Desta forma, assim como para a estimação de 
#' modelos com sazonalidade, a estimação de modelos periódicos \bold{NECESSITA} que \code{serie} 
#' seja um objeto de serie temporal com sazonalidade. Do contrário será estimado um modelo único 
#' para a série completa.
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
#' @param periodico booleano indicando se o modelo é global ou periódico. Ver Detalhes
#' @param ... demais parâmetros passados para as funções de fit específicas de cada modelo. Ver
#'     Detalhes
#' 
#' @examples 
#' 
#' # SARIMA -----------------------------------------------
#' 
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
#' # SS AR(1) + Sazonalidade ------------------------------
#' 
#' mod_ss <- estimamodelo(AirPassengers, tipo = "ss_ar1_saz")
#' 
#' # caso a serie nao possua sazonalidade explicita, o modelo sera ajustado sem isso
#' ss <- arima.sim(200, model = list(ar = .8))
#' mod_ss_semsazo <- estimamodelo(ss, tipo = "ss_ar1_saz")
#' 
#' # Regressao linear -------------------------------------
#' 
#' serie <- window(datregdin$obs, 1, 100)
#' varex <- datregdin$varex[1:100, , drop = FALSE]
#' 
#' # sem passar uma formula, todas as variaveis sao utilizadas de forma aditiva
#' # e um aviso sera levantado
#' mod_regstat <- estimamodelo(serie, "reg_lin", regdata = varex)
#' 
#' mod_regstat <- estimamodelo(serie, "reg_lin", regdata = varex, formula = ~ V1 + V2 * V3)
#' 
#' # Regressao dinamica -----------------------------------
#' 
#' serie <- window(datregdin$obs, 1, 100)
#' varex <- datregdin$varex[1:100, , drop = FALSE]
#' 
#' mod_regdin <- estimamodelo(serie, regdata = varex, tipo = "ss_reg_din", formula = ~ V1 + V2)
#' 
#' # explicitando uma formula
#' mod_regdin2 <- estimamodelo(serie, "ss_reg_din", formula = ~ V1 + V2 * V3, regdata = varex)
#' 
#' \dontrun{
#' layout(matrix(1:2))
#' plot(mod_regdin, main = "obs ~ V1 + V2")
#' plot(mod_regdin2, main = "obs ~ V1 + V2 * V3")
#' }
#' 
#' # Visualizacao -----------------------------------------
#' 
#' \dontrun{
#' plot(mod_regdin)
#' }
#' 
#' @return Objeto da classe modprev e subclasse igual a \code{tipo}, uma lista de dois elementos:
#'     \code{modelo} e \code{serie} contendo o modelo estimado e a série passada
#' 
#' @seealso \code{\link{janelamovel}} para backtest dos modelos em horizonte rolante
#' 
#' @family Metodos modprev
#' 
#' @export

estimamodelo <- function(serie, tipo, periodico = FALSE, ...) UseMethod("estimamodelo")

#' @export

estimamodelo.numeric <- function(serie, tipo, periodico = FALSE, ...) estimamodelo.ts(ts(serie), tipo, ...)

#' @export

estimamodelo.ts <- function(serie, tipo, periodico = FALSE, ...) {

    fitfun <- ifelse(periodico, estimamodelo_P, estimamodelo_U)
    mc <- match.call()
    mc[[1]] <- fitfun
    mc$periodico <- NULL

    eval(mc, parent.frame())
}

# METODOS -----------------------------------------------------------------------------------------

#' Previsão De Modelos \code{modprev}
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
#' \bold{Modelos periódicos:}
#' 
#' Nos casos de modelos periódicos que necessitam variaveis explicativas, alguns cuidados devem ser
#' tomados. O usuario informara um argumento \code{newdata} contendo um \code{data.frame}-like unico
#' contendo todas as variaveis explicativas fora da amostra. Se assume que as linhas neste dado 
#' correspondem as variaveis explicativas nos tempos apos o ultimo da serie, cronologicamente.
#' 
#' Isto significa que, se a serie orignal era mensal terminando em junho/2020, o programa assume que 
#' \code{newdata} tem, na primeira linha, as variaveis explicativas para julho/2020, na segunda, 
#' para ago/2020 e assim por diante. Esta suposicao se sustenta no fato de que o pacote 
#' \code{modprev} foi feito para modelagem de  series temporais em principio.
#' 
#' Alternativamente, o usuario pode passar \code{newdata} como uma lista de \code{data.frame}-likes;
#' neste caso se assume que cada elemento da lista corresponde a uma estacao do dado, em ordem. 
#' Observe que, se a serie comeca em maio, entao MAIO CORRESPONDE A PRIMEIRA ESTACAO.
#' 
#' @param object modelo ajustado através de \code{\link{estimamodelo}}
#' @param n.ahead número de passos à frente para prever
#' @param ... Opcionalmente, pode ser passado o argumento \code{newdata} \code{data.frame}-like 
#'     contendo variaveis explicativas fora da amostra para modelos que necessitem
#' 
#' @examples
#' 
#' mod <- estimamodelo(AirPassengers, tipo = "sarima")
#' pred <- predict(mod, n.ahead = 24)
#' 
#' # em modelos de regressao, deve ser passada a variavel explicativa via newdata
#' 
#' serie <- window(datregdin$obs, 1, 100)
#' varex <- datregdin$varex[1:100, , drop = FALSE]
#' mod_regdin <- estimamodelo(serie, "ss_reg_din", regdata = varex)
#' 
#' newdata <- datregdin$varex[101:130, , drop = FALSE]
#' pred <- predict(mod_regdin, newdata = newdata)
#' 
#' @return série temporal multivariada contendo a previsão e o desvio padrão associado para os
#'     passos de tempo \code{1:n.ahead}
#' 
#' @family Metodos modprev
#' 
#' @export

predict.modprev <- function(object, n.ahead, ...) {
    stop(paste0("Modelo do tipo ", class(object)[1], " nao possui metodo 'predict'"))
}

#' Atualizacao De Modelos \code{modprev}
#' 
#' Wrapper para atualizar e possivelmete reajustar modelos \code{modprev}
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
#' @param ... Opcionalmente, pode ser passado o \code{newregdata}, um \code{data.frame}-like 
#'     contendo variaveis explicativas pareadas com \code{newseries} para modelos que as necessitem
#' 
#' @examples 
#' 
#' serie1 <- window(AirPassengers, c(1949, 1), c(1954, 12))
#' serie2 <- window(AirPassengers, c(1955, 1), c(1960, 12))
#' 
#' mod_orig  <- estimamodelo(serie1, tipo = "sarima")
#' mod_upd   <- update(mod_orig, serie2, refit = FALSE)
#' mod_refit <- update(mod_orig, serie2, refit = TRUE)
#' 
#' \dontrun{
#' layout(matrix(1:3))
#' plot(mod_orig, main = "original")
#' plot(mod_upd, main = "update s/ refit")
#' plot(mod_refit, main = "update c/ refit")
#' }
#' 
#' # Com variaveis explicativas ---------------------------
#' 
#' serie <- window(datregdin$obs, 1, 100)
#' varex <- datregdin$varex[1:100, , drop = FALSE]
#' mod_orig <- estimamodelo(serie, "ss_reg_din", regdata = varex, formula = ~ V1 + V2 * V3)
#' 
#' newserie <- window(datregdin$obs, 101, 200)
#' newvarex <- datregdin$varex[101:200, , drop = FALSE]
#' 
#' mod_upd   <- update(mod_orig, newserie, newregdata = newvarex, refit = FALSE)
#' mod_refit <- update(mod_orig, newserie, newregdata = newvarex, refit = TRUE)
#' 
#' @return modelo com novos dados, possivelmente reajustado
#' 
#' @family Metodos modprev
#' 
#' @export

update.modprev <- function(object, newseries, refit = FALSE, ...) {
    stop(paste0("Modelo do tipo ", class(object)[1], " nao possui metodo 'update'"))
}
