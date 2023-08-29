#' Pacote \code{modprev}
#' 
#' Interface Unificada Para Estimacao E Uso De Modelos
#' 
#' Arcabouco unificado para estimacao, previsão, simulacao e etc de diversos tipos de modelos, 
#' incluindo aqueles com variaveis explicativas. Alem de wrappers para metodos tradicionais de 
#' modelos estatisticos, e implementada uma funcao para modelagem e previsão em horizonte rolante, 
#' facilitando testes com novos modelos. As funcoes do pacote operam em grande parte como wrappers 
#' em torno dos metodos especificos de cada tipo de modelo, visando apresentar uma interface comum 
#' para uso de todos eles, tal que a adicao de novos modelos seja simples.
#' 
#' @docType package
#' @name modprev
#' 
#' @import forecast KFAS stats quantreg MSGLasso
#' 
#' @importFrom graphics grid lines title
NULL