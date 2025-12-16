####################################################################################################
# CLASSE ABSTRATA CONTENDO MODELOS PERIODICOS
####################################################################################################

# CONSTRUTOR ---------------------------------------------------------------------------------------

#' Estimacao De Modelos Periodicos
#' 
#' Wrapper para estimacao de modelos periodicos
#' 
#' Esta funcao nao deve ser chamada diretamente pelo usuario, mas sim internamente por 
#' \code{\link{estimamodelo}} quando o argumento \code{periodico == TRUE}. Descricoes mais 
#' detalhadas da estimacao de modelos, periodicos ou nao, devem ser buscadas em 
#' \code{\link{estimamodelo}} diretamente.
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

    aux_tsp <- tsp(serie)
    if (aux_tsp[3] == 1) stop("'serie' nao possui sazonalidade -- nao pode ser modelada periodicamente")

    seasons <- as.numeric(cycle(serie))
    seasons <- factor(seasons, unique(seasons))

    l_series <- split(serie, seasons)
    l_series <- mapply(seq(l_series), l_series, FUN = function(n, v) {
        ts(v, start = aux_tsp[1] + (n - 1) * (1 / aux_tsp[3]), deltat = 1)
    }, SIMPLIFY = FALSE)
    names(l_series) <- levels(seasons)

    args <- list(...)

    if ("regdata" %in% ...names()) {
        l_regdata <- split(args$regdata, seasons)
        args$regdata <- NULL
    } else {
        # caso nao tenha sido passado regdata, usa uma lista vazia. Nos modelos em que regdata e
        # necessaria isso vai dar erro (como deveria, pois um arg obrigatorio esta faltando) e nos
        # que nao, esse argumento nem e usado
        l_regdata <- vector("list", length(l_series))
    }

    fits <- mapply(l_series, l_regdata, FUN = function(serie, regdata) {
        args <- c(list(serie = serie, tipo = tipo, regdata = regdata), args)
        do.call(estimamodelo_U, args)
    }, SIMPLIFY = FALSE)
    fits <- fits[order(as.numeric(names(fits)))]

    mod_atrs <- list(tsp = aux_tsp)

    new_modprevP(fits, serie, mod_atrs)
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
#' @param atrs lista nomeada contendo atributos extras pertinentes ao modelo. Ver Detalhes
#' 
#' @return Objeto da classe \code{modprev} e subclasse igual a \code{modprevP}, uma lista de dois 
#'     elementos: \code{modelos} e \code{serie} contendo os modelos parciais estimados e a série
#'     passada. Adicionalmente, se \code{atrs} for passada, um atributo "mod_atrs" contendo o
#'     argumento homônimo.

new_modprevP <- function(fits, serie, atrs) {
    new <- list(modelos = fits, serie = serie)
    class(new) <- c("modprevP", "modprev")

    if (!missing("atrs")) attr(new, "mod_atrs") <- atrs

    return(new)
}

# METODOS ------------------------------------------------------------------------------------------

#' Previsao De Modelos Periodicos
#' 
#' Wrapper para previsao de cada modelo individual e reorganizacao em serie unica
#' 
#' Nos casos de modelos que necessitam variaveis explicativas, alguns cuidados devem ser tomados. O
#' usuario informara um argumento \code{newdata} contendo um \code{data.frame}-like unico contendo
#' todas as variaveis explicativas fora da amostra. Se assume que as linhas neste dado correspondem
#' as variaveis explicativas nos tempos apos o ultimo da serie, cronologicamente.
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
#' @param object modelo periodico com o qual realizar a previsao
#' @param n.ahead número de passos à frente para previsão. Este argumento não é necessario, caso não
#'     seja informado a previsão sera feita tantos passos à frente quanto amostras em \code{newdata}
#' @param ... Opcionalmente, pode ser passado o argumento \code{newdata} \code{data.frame}-like 
#'     contendo variaveis explicativas fora da amostra para modelos que necessitem
#' 
#' @return série temporal multivariada contendo a previsão e o desvio padrão associado para os
#'     passos de tempo \code{1:n.ahead}
#' 
#' @export

predict.modprevP <- function(object, n.ahead, ...) {

    nmods <- length(object$modelos)

    aux_tsp <- attr(object, "mod_atrs")$tsp
    tp1     <- aux_tsp[2] + 1 / aux_tsp[3]

    args <- list(...)

    has_newdata <- "newdata" %in% ...names()
    newdata_list <- has_newdata && class(args$newdata) == "list"

    if (has_newdata && !newdata_list) {

        # assumindo que newdata e uma continuacao cronologica da serie
        aux_split <- ts(seq_len(nrow(args$newdata)), start = tp1, frequency = aux_tsp[3])

        newdata <- split(args$newdata, cycle(aux_split))
        args$newdata <- NULL

    } else if (has_newdata && newdata_list) {
        names(newdata) <- seq_along(newdata)
    } else if (!has_newdata) {
        newdata <- structure(vector("list", nmods), names = seq_len(nmods))
    }

    submodels <- as.numeric(names(newdata))

    if (missing("n.ahead")) {
        v_h <- lapply(newdata, nrow)
    } else {
        aux_split <- ts(seq_len(n.ahead), start = tp1, frequency = aux_tsp[3])
        v_h <- split(seq_len(n.ahead), cycle(aux_split))
        v_h <- sapply(v_h, length)
    }

    prevs <- mapply(object$modelos[submodels], newdata, v_h[submodels], FUN = function(mod, nd, h) {
        args <- c(list(object = mod, n.ahead = h, newdata = nd), args)
        do.call(predict, args)
    }, SIMPLIFY = FALSE)

    prev_times <- unlist(lapply(prevs, time))

    prevs <- do.call(rbind, lapply(prevs, unclass))
    prevs <- ts(prevs[order(prev_times), ], start = tp1, frequency = aux_tsp[3])

    return(prevs)
}

#' Update De Modelos Periodicos
#' 
#' Wrapper para atualizar e possivelmente reajustar modelos periodicos
#' 
#' @param object modelo ajustado atraves de \code{estimamodelo_U}
#' @param newseries nova serie para associar ao modelo
#' @param refit booleano indicando se o modelo deve ser reajustado
#' @param ... Opcionalmente, pode ser passado o \code{newregdata}, um \code{data.frame}-like 
#'     contendo variaveis explicativas pareadas com \code{newseries} para modelos que as necessitem
#' 
#' @return modelo com novos dados, possivelmente reajustado
#' 
#' @export

update.modprevP <- function(object, newseries, refit = FALSE, ...) {

    nmods <- length(object$modelos)

    args <- list(...)

    aux_tsp <- tsp(newseries)

    seasons <- as.numeric(cycle(newseries))
    seasons <- factor(seasons, unique(seasons))

    l_newseries <- split(newseries, seasons)
    l_newseries <- mapply(seq(l_newseries), l_newseries, FUN = function(n, v) {
        ts(v, start = aux_tsp[1] + (n - 1) * (1 / aux_tsp[3]), deltat = 1)
    }, SIMPLIFY = FALSE)
    names(l_newseries) <- levels(seasons)

    has_newregdata <- "newregdata" %in% ...names()
    newregdata_list <- has_newregdata && class(args$newregdata) == "list"

    if (has_newregdata && !newregdata_list) {

        newregdata <- split(args$newregdata, seasons)
        args$newregdata <- NULL

    } else if (has_newregdata && newregdata_list) {
        names(newregdata) <- seq_along(newregdata)
    } else if (!has_newregdata) {
        newregdata <- structure(vector("list", nmods), names = seq_len(nmods))
    }

    submodels <- as.numeric(names(l_newseries))
    ord <- order(submodels)

    mods <- mapply(object$modelos, l_newseries[ord], newregdata[ord], FUN = function(mod, ns, nrd) {
        args <- c(list(object = mod, newseries = ns, newregdata = nrd, refit = refit), args)
        do.call(update, args)
    }, SIMPLIFY = FALSE)

    mod_atrs <- list(tsp = aux_tsp)

    new_modprevP(mods, newseries, mod_atrs)
}
