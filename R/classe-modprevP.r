####################################################################################################
# CLASSE ABSTRATA CONTENDO MODELOS PERIODICOS
####################################################################################################

# CONSTRUTOR ---------------------------------------------------------------------------------------

#' Estimacao De Modelos Periodicos
#' 
#' Wrapper para estimacao de modelos periodicos
#' 
#' Esta funcao nao deve ser chamada diretamente pelo usuario, mas sim internamente por 
#' \code{\link{estimamodelos}} quando o argumento \code{periodico == TRUE}. Descricoes mais 
#' detalhadas da estimacao de modelos, periodicos ou nao, devem ser buscadas em 
#' \code{\link{estimamodelos}} diretamente.
#' 
#' Modelos periodicos sao estimados e utilizados em cima do arcabouço ja desenvolvido para os 
#' modelos unicos. Esta classe nada mais e do que um wrapper em torno de uma lista de modelos, um
#' para cada estacao da serie, estimados por \code{\link{estimamodelo_U}}. Todos os metodos sao
#' desenvolvidos em torno daqueles ja existentes para cada arcabouco de modeloagem, sendo a classe
#' \code{modprevP} um simples coordenador do uso destes S modelos periodicos.
#' 
#' @param serie série para ajustar
#' @param tipo tipo de modelo a ser ajustado. Ver Detalhes
#' @param ... demais parâmetros passados para as funções de fit específicas de cada modelo
#' 
#' @return Objeto da classe modprev e subclasse igual a \code{tipo}, uma lista de dois elementos:
#'     \code{modelo} e \code{serie} contendo o modelo estimado e a série passada

estimamodelo_P <- function(serie, tipo, ...) {

    freq <- tsp(serie)
    if(freq == 1) stop("'serie' nao possui sazonalidade -- nao pode ser modelada periodicamente")

    aux_tsp <- tsp(serie)
    seasons <- as.numeric(cycle(serie))
    l_series <- split(serie, seasons)
    l_series <- lapply(seq(l_series), function(i) {
        ts(l_series[[i]], start = aux_tsp[1] + (i - 1) * (1 / aux_tsp[3]), delta = 1)
    })

    args <- list(...)

    if("regdata" %in% ...names()) {
        l_regdata <- split(regdata, seasons)
        args$regdata <- NULL
    } else {
        # caso nao tenha sido passado regdata, usa uma lista vazia. Nos modelos em que regdata e
        # necessaria isso vai dar erro (como deveria, pois um arg obrigatorio esta faltando) e nos
        # que nao, esse argumento nem e usado
        l_regdata <- vector("list", length(l_series))
    }

    out <- mapply(l_series, l_regdata, FUN = function(serie, regdata) {
        args <- c(list(serie, tipo, regdata), args)
        do.call(estimamodelo_U, args)
    }, SIMPLIFY = FALSE)

    new_modprevP(out)
}

#' Construtor Interno De \code{modprevP}
#' 
#' Função interna, não deve ser chamada diretamente pelo usuário
#' 
#' \code{atrs} Existe para permitir que outras informacoes, nao necessariamente contidas no objeto 
#' do modelo (como por exemplo a formula de regressao nos modelos de regressao dinamica), sejam 
#' passadas adiante para os metodos de cada modelagem. A lista aqui passada sera adicionada ao 
#' objeto \code{modprev} de saida como um atributo chamado "mod_atrs".
#' 
#' @param fits lista de modelos parciais estimados, cada um um objeto \code{modprevU}
#' @param serie serie para qual o modelo periodico foi estimado
#' @param mod_atrs lista nomeada contendo atributos extras pertinentes ao modelo. Ver Detalhes
#' 
#' @return Objeto da classe \code{modprev} e subclasse igual a \code{modprevP}, uma lista de dois 
#'     elementos: \code{modelos} e \code{serie} contendo os modelos parciais estimados e a série
#'     passada. Adicionalmente, se \code{atrs} for passada, um atributo "mod_atrs" contendo o
#'     argumento homônimo.

new_modprevP <- function(fits, serie, mod_atrs) {
    new <- list(modelos = fits, serie = serie)
    class(new) <- c("modprevP", "modprev")

    if(!missing("atrs")) attr(new, "mod_atrs") <- atrs

    return(new)
}