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
#' @family Metodos modprev
#' 
#' @seealso \code{\link{janelamovel}} para backtest dos modelos em horizonte rolante
#' 
#' @export

estimamodelo <- function(serie, tipo, periodico = FALSE, ...) UseMethod("estimamodelo")

#' @export

estimamodelo.numeric <- function(serie, tipo, periodico = FALSE, ...) estimamodelo.ts(ts(serie), tipo, ...)

#' @export

estimamodelo.ts <- function(serie, tipo, periodico = FALSE, ...) {

    if(periodico) {
        out <- estimamodelo_P(serie, tipo, ...)
    } else {
        out <- estimamodelo_U(serie, tipo, ...)
    }

    return(out)
}
