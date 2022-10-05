####################################################################################################
# SUPERCLASSE ABSTRATA DOS DIFERENTES TIPOS DE MODELOS
####################################################################################################

# CONSTRUTOR ---------------------------------------------------------------------------------------

#' Estimacao De Modelos Unicos
#' 
#' Wrapper para estimacao de modelos unicos
#' 
#' Esta funcao nao deve ser chamada diretamente pelo usuario, mas sim internamente por 
#' \code{\link{estimamodelos}} quando o argumento \code{periodico == FALSE}. Descricoes mais 
#' detalhadas da estimacao de modelos, periodicos ou nao, devem ser buscadas em 
#' \code{\link{estimamodelos}} diretamente.
#' 
#' @param serie serie para ajustar
#' @param tipo tipo de modelo a ser ajustado. Ver Detalhes
#' @param ... demais parâmetros passados para as funções de fit específicas de cada modelo. Ver
#'     Detalhes
#' 
#' @examples 
#' 
#' @return Objeto da classe modprevU e subclasse igual a \code{tipo}, uma lista de dois elementos:
#'     \code{modelo} e \code{serie} contendo o modelo estimado e a série passada
#' 
#' @family Metodos modprevU

estimamodelo_U <- function(serie, tipo, ...) {

    args_tipo <- names(formals(tipo))
    tipo <- str2lang(paste0("modprev:::", tipo))

    mc <- match.call()
    mc <- mc[c(TRUE, names(mc)[-1] %in% args_tipo)]
    mc[[1]] <- tipo

    out <- eval(mc, envir = parent.frame(), enclos = parent.frame())

    return(out)
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

new_modprevU <- function(fit, serie, tipo, atrs) {
    new <- list(modelo = fit, serie = serie)
    class(new) <- c(tipo, "modprevU", "modprev")

    if(!missing("atrs")) attr(new, "mod_atrs") <- atrs

    return(new)
}

# METODOS -----------------------------------------------------------------------------------------

#' Previsão De Modelos \code{modprevU}
#' 
#' Wrapper para previsão de modelos unicos
#' 
#' @param object modelo ajustado através de \code{\link{estimamodeloU}}
#' @param n.ahead número de passos à frente para prever
#' @param ... existe apenas para consistência com a genérica
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

#' Atualizacao De Modelos \code{modprevU}
#' 
#' Wrapper para atualizar e possivelmete reajustar modelos \code{modprevU}
#'  
#' @param object modelo ajustado atraves de \code{estimamodeloU}
#' @param newseries nova serie para associar ao modelo
#' @param refit booleano indicando se o modelo deve ser reajustado
#' @param ... existe apenas para consistência com a genérica
#' 
#' @return modelo com novos dados, possivelmente reajustado
#' 
#' @family Metodos modprevU
#' 
#' @export

update.modprevU <- function(object, newseries, refit = FALSE, ...) {
    stop(paste0("Modelo do tipo ", class(object)[1], " nao possui metodo 'update'"))
}
