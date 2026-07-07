####################################################################################################
# SUPERCLASSE ABSTRATA DOS DIFERENTES TIPOS DE MODELOS
####################################################################################################

# CONSTRUTOR ---------------------------------------------------------------------------------------

#' Estimacao De Modelos Unicos
#' 
#' Wrapper para estimacao de modelos unicos
#' 
#' Esta funcao nao deve ser chamada diretamente pelo usuario, mas sim internamente por 
#' \code{\link{estimamodelo}} quando o argumento \code{periodico == FALSE}. Descricoes mais 
#' detalhadas da estimacao de modelos, periodicos ou nao, devem ser buscadas em 
#' \code{\link{estimamodelo}} diretamente.
#' 
#' @param serie serie para ajustar
#' @param tipo tipo de modelo a ser ajustado. Ver Detalhes
#' @param ... demais parâmetros passados para as funções de fit específicas de cada modelo. Ver
#'     Detalhes
#' 
#' @return Objeto da classe modprevU e subclasse igual a \code{tipo}, uma lista de dois elementos:
#'     \code{modelo} e \code{serie} contendo o modelo estimado e a série passada
#' 
#' @family Metodos modprevU

estimamodelo_U <- function(serie, tipo, ...) {
    spec <- get_model(tipo)
    spec$fit_fn(serie, ...)
}

#' Contrutor Interno De \code{modprevU}
#' 
#' Função interna, não deve ser chamada diretamente pelo usuário
#' 
#' \code{atrs} Existe para permitir que outras informacoes, nao necessariamente contidas no objeto 
#' do modelo (como por exemplo a formula de regressao nos modelos de regressao dinamica), sejam 
#' passadas adiante para os metodos de cada modelagem. A lista aqui passada sera adicionada ao 
#' objeto \code{modprevU} de saida como um atributo chamado "mod_atrs".
#' 
#' @param fit modelo estimado
#' @param serie serie para qual o modelo foi estimado
#' @param tipo string indicando espcificação do modelo
#' @param atrs lista nomeada contendo atributos extras pertinentes ao modelo. Ver Detalhes
#' 
#' @return Objeto da classe \code{modprevU} e subclasse igual a \code{tipo}, uma lista de dois 
#'     elementos: \code{modelo} e \code{serie} contendo o modelo estimado e a série passada. 
#'     Adicionalmente, se \code{atrs} for passada, um atributo "mod_atrs" contendo o argumento
#' 
#' @export

new_modprevU <- function(fit, serie, tipo, atrs) {
    new <- list(modelo = fit, serie = serie)
    class(new) <- c(tipo, "modprevU", "modprev")

    if (!missing("atrs")) attr(new, "mod_atrs") <- atrs

    new
}

# METODOS -----------------------------------------------------------------------------------------

#' Previsão De Modelos \code{modprevU}
#' 
#' Wrapper para previsão de modelos unicos
#' 
#' @param object modelo ajustado através de \code{\link{estimamodelo_U}}
#' @param n.ahead número de passos à frente para prever
#' @param ... Opcionalmente, pode ser passado o argumento \code{newdata} \code{data.frame}-like 
#'     contendo variaveis explicativas fora da amostra para modelos que necessitem
#' 
#' @return série temporal multivariada contendo a previsão e o desvio padrão associado para os
#'     passos de tempo \code{1:n.ahead}
#' 
#' @family Metodos modprevU
#' 
#' @export

predict.modprevU <- function(object, n.ahead, ...) {
    stop(paste0("Modelo do tipo ", class(object)[1], " nao possui metodo 'predict'"))
}

#' Simulacao De Modelos \code{modprevU}
#'
#' Wrapper para simulacao de modelos unicos
#'
#' @param object modelo ajustado através de \code{\link{estimamodelo_U}}
#' @param nsim número de simulações a serem geradas
#' @param seed opcionalmente, semente para geração das simulações
#' @param n.ahead número de passos à frente para simular
#' @param ... Opcionalmente, pode ser passado o argumento \code{newdata} \code{data.frame}-like
#'     contendo variaveis explicativas fora da amostra para modelos que necessitem
#'
#' @return série temporal multivariada contendo \code{nsim} simulações para os passos de tempo
#'     \code{1:n.ahead}
#'
#' @family Metodos modprevU
#'
#' @export

simulate.modprevU <- function(object, nsim = 1, seed = NULL, n.ahead, ...) {
    stop(paste0("Modelo do tipo ", class(object)[1], " nao possui metodo 'simulate'"))
}

#' Atualizacao De Modelos \code{modprevU}
#' 
#' Wrapper para atualizar e possivelmete reajustar modelos \code{modprevU}
#'  
#' @param object modelo ajustado atraves de \code{estimamodelo_U}
#' @param newseries nova serie para associar ao modelo
#' @param refit booleano indicando se o modelo deve ser reajustado
#' @param ... Opcionalmente, pode ser passado o \code{newregdata}, um \code{data.frame}-like 
#'     contendo variaveis explicativas pareadas com \code{newseries} para modelos que as necessitem
#' 
#' @return modelo com novos dados, possivelmente reajustado
#' 
#' @family Metodos modprevU
#' 
#' @export

update.modprevU <- function(object, newseries, refit = FALSE, ...) {
    stop(paste0("Modelo do tipo ", class(object)[1], " nao possui metodo 'update'"))
}
