########################################### JANELA MOVEL ###########################################

#' Previsão Em Horizonte Rolante
#'
#' Função para realizar previsões e reajustes em janela móvel
#'
#' A partir da versão 2.0.0, esta função requer um objeto de configuração criado
#' por \code{\link{jm_config}}. O uso de argumentos individuais não é mais suportado.
#'
#' Os argumentos \code{serie} e \code{tipo} tem exatamente o mesmo efeito daqueles descritos em
#' \code{\link{estimamodelo}}.
#'
#' O objeto \code{config} controla o comportamento da janela móvel. Veja \code{\link{jm_config}}
#' para detalhes sobre os parâmetros de configuração.
#'
#' \bold{Modelos com variáveis explicativas:}
#'
#' Caso o modelo escolhido use de variáveis explicativas, é necessário que o \code{regdata} seja
#' passado na forma de um \code{data.frame}-like contendo as variáveis a serem utilizadas.
#'
#' Este argumento \emph{DEVE CONTER AS VARIÁVEIS EXPLICATIVAS CORRESPONDENTES A TODAS AS OBSERVAÇÕES
#' DA SÉRIE MAIS \code{n.ahead} À FRENTE}. A primeira parte dessa restrição é natural, pois são
#' necessárias as variáveis explicativas insample para ajustes do modelo. As observações
#' \code{n.ahead} passos à frente do final da série são necessárias apenas para a previsão das
#' últimas janelas.
#'
#' @param serie serie temporal pela qual passar a janela movel
#' @param tipo tipo de modelo a ser ajustado. Ver \code{\link{estimamodelo}}.
#' @param config Objeto de configuração criado por \code{\link{jm_config}}. Obrigatório.
#' @param ... demais argumentos pertinentes a estimação de cada \code{tipo}. Para
#'     modelos \code{modprevS}, deve incluir \code{newdata_list}. Veja
#'     \code{\link{estimamodelo}} para mais detalhes.
#'
#' @examples
#'
#' # Janela rolante de 60 observações, prevendo 12 passos
#' cfg <- jm_config(janela = 60, passo = 6, n.ahead = 12)
#' jm_sarima <- janelamovel(AirPassengers, "sarima", config = cfg)
#'
#' # Janela expansível começando na posição 1
#' cfg <- jm_config(janela = c(1, 60), passo = 6, n.ahead = 12, verbose = 1)
#' jm_sarima <- janelamovel(AirPassengers, "sarima", config = cfg)
#'
#' # Com variáveis explicativas
#' serie <- window(datregdin$obs, 1, 190)
#' varex <- datregdin$varex
#' cfg <- jm_config(janela = 100, passo = 10, n.ahead = 5)
#' jm_regdin <- janelamovel(serie, "ss_reg_din", config = cfg, regdata = varex)
#'
#' @return lista contendo previsoes de 1 a n.ahead passos à frente para cada janela
#'
#' @seealso \code{\link{jm_config}} para criação de objetos de configuração
#'
#' @export

janelamovel <- function(serie, tipo, config, ...) {

    validate_jm_config(config)

    args <- list(...)

    if (!is.ts(serie)) serie <- ts(serie)

    has_regdata <- "regdata" %in% ...names()
    if (!has_regdata) {
        regdata <- NULL
    } else {
        regdata <- args$regdata
        args$regdata <- NULL
    }

    if (has_regdata && (length(serie) + config$n.ahead > nrow(regdata))) {
        warning("'regdata' deve conter 'length(serie) + n.ahead' observacoes -- reduzindo 'serie'")

        reduz <- length(serie) + config$n.ahead - nrow(regdata)
        serie <- window(serie, start(serie), deltats(end(serie), -reduz, frequency(serie)))
    }

    verb_func <- verbose_fun(config$verbose)
    janelas <- expandejanelas(serie, config$janela, config$passo)
    v_refit <- expanderefit(janelas, config$refit.cada)

    # variavel auxiliar para fazer o subset de newdata pareado com serie
    aux <- ts(seq_len(length(serie) + config$n.ahead), start = start(serie), frequency = frequency(serie))

    # Estima um modelo inicial que vai ser usado na primeira janela e atualizado dali em diante
    ij <- janelas[[1]]
    iserie   <- window(serie, ij[[1]], ij[[2]])
    iregdata <- regdata[window(aux, ij[[1]], ij[[2]]), , drop = FALSE]
    mod <- c(list(quote(estimamodelo), serie = iserie, tipo = tipo, regdata = iregdata), args)
    mod <- eval(as.call(mod), parent.frame(), parent.frame())

    retfun <- whichreturn(config$full.output)

    jm <- vector("list", length(janelas))
    for (i in seq_along(janelas)) {

        verb_func(janelas[[i]][[1]], janelas[[i]][[2]], v_refit[i])

        ij <- janelas[[i]]
        iserie   <- window(serie, ij[[1]], ij[[2]])
        iregdata <- regdata[window(aux, ij[[1]], ij[[2]]), , drop = FALSE]
        mod      <- update(mod, iserie, newregdata = iregdata, refit = v_refit[i])

        ijn <- list(deltats(ij[[2]], 1, frequency(aux)), deltats(ij[[2]], config$n.ahead, frequency(aux)))
        inewdata <- regdata[window(aux, ijn[[1]], ijn[[2]]), , drop = FALSE]
        pred <- predict(mod, config$n.ahead, newdata = inewdata)

        jm[[i]] <- retfun(pred, mod, inewdata)
    }

    return(jm)
}

# HELPERS ------------------------------------------------------------------------------------------

#' Tipo De \code{return} Em \code{janelamovel}
#' 
#' Seleciona se a funcao de \code{return} no lapply devolve tudo ou so a previsao
#' 
#' @param full.output booleano indicando o tipo de retorno
#' 
#' @return funcao de retorno para usar no lapply de \code{janelamovel}

whichreturn <- function(full.output) ifelse(full.output, fullreturn, simplereturn)

fullreturn   <- function(pred, mod, regdata) return(list(pred, mod, regdata))
simplereturn <- function(pred, ...) return(pred)

#' Selecao Da Funcao De Log
#' 
#' Retorna uma funcao para prints informativos durante a execucao
#' 
#' @param verbose inteiro indicando o nivel de detalhe a ser informado:
#'     0 = nada, 1: toda vez que reajusta modelo, 2: todo horizonte de previsão e reajuste
#' 
#' @return funcao para uso interno na janela movel que printa o estado de execucao

verbose_fun <- function(verbose) {

    verb_func <- switch(as.character(verbose),
        "0" = function(...) NULL,
        "1" = function(i, f, r) if (r) cat("\t Prevendo serie [", i, "] -> [", f, "]\n") else NULL,
        "2" = function(i, f, r) {
            if (r) {
                cat("REFIT -- Prevendo serie [", i, "] -> [", f, "]\n")
            } else {
                cat("\t Prevendo serie [", i, "] -> [", f, "]\n")
            }
        }
    )

    return(verb_func)
}

#' Janelas Para Execucao Em Janela Movel
#' 
#' Interpreta os argumentos passados a \code{\link{janelamovel}} a respeito da largura da janela
#' 
#' \code{janela} pode ser ou um escalar ou um vetor de dois elementos inteiros. No primeiro caso, 
#' entende-se que deverao ser rodadas janelas de tamanho \code{janela}. Quando este argumento e um
#' vetor entende-se que todas as janelas devem iniciar no instante de tempo \code{janela[1]} com 
#' largura inicial \code{janela[2]}. Essencialmente estas duas formas permitem a execucao em janela
#' rolante ou expansivel, respectivamente.
#' 
#' O argumento \code{passo} permite especificar quantas novas observacoes sao incorporadas entre 
#' janelas adjacentes. No caso de janela rolante, cada janela incorpora \code{passos} novos pontos e
#' abandona os \code{passos} valores mais antigos, de modo que todas tem o mesmo tamanho; janelas 
#' expansiveis vao simplesmente agregando novas observacoes. Deve ser observado que, no caso de 
#' janelas rolantes, sempre havera uma janela contendo o final da serie, mesmo que passos seja um
#' número tal que da penultima para a ultima janela ocorra um intervalo menor que \code{passos}.
#' 
#' @param serie a serie sobre a qual sera passada a janela movel
#' @param janela escalar ou vetor especificando a janela. Ver Detalhes
#' @param passo saltos entre cada janela. Ver Detalhes
#' 
#' @return lista na qual cada elemento e uma lista de dois elementos, contendo o instante inicial e
#'     final de \code{serie} a considerar em cada janela, no sistema de tempo \code{ts}. Deve ser 
#'     notado que os instantes definem uma janela fechada no inicio e aberta no final

expandejanelas <- function(serie, janela, passo) {

    N <- length(serie)
    S <- frequency(serie)
    INI <- start(serie)

    if (length(janela) > 1) {
        inifix <- TRUE
    } else {
        inifix <- FALSE
        janela <- c(1, janela)
    }

    v_ends <- seq(janela[2], N - (janela[1] - 1), by = passo)

    ultimo <- N - (janela[1] - 1)
    if (tail(v_ends, 1) != ultimo) v_ends <- c(v_ends, ultimo)

    if (inifix) {
        ini <- deltats(INI, janela[1] - 1, S)
        janelas <- lapply(v_ends, function(i) {
            end <- deltats(ini, i - 1, S)
            list(ini, end)
        })
    } else {
        janelas <- lapply(v_ends, function(i) {
            ini <- deltats(INI, i - janela[2], S)
            end <- deltats(INI, i - 1, S)
            list(ini, end)
        })
    }

    return(janelas)
}

#' Indices De Tempo Para Reajustar Modelo
#' 
#' Interpreta os argumentos passados a \code{\link{janelamovel}} a respeito dos reajustes
#' 
#' O vetor retornado sempre tera a primeira posicao igual a \code{FALSE}, pois no ambito da 
#' janelamovel a primeira janela sempre corresponde a primeira estimacao do modelo, uma que e feita
#' por fora da janela movel.
#' 
#' @param janelas lista de inicios e finais de janelas retornada por \code{\link{expandejanelas}}
#' @param refit.cada inteiro indicando de quantos em quantos instantes o modelo deve ser reajustado
#' 
#' @return vetor logico do mesmo tamanho de \code{janelas} indicando em quais janelas o modelo deve
#'     ser reestimado

expanderefit <- function(janelas, refit.cada) {

    nj <- length(janelas)
    if ((length(refit.cada) == 1) && is.na(refit.cada)) refit.cada <- length(janelas)

    if (length(refit.cada) == 1) {
        v_refit <- seq(1, nj, by = refit.cada)
    } else {
        stop("'refit.cada' como vetor de indices ainda nao implementado")
    }

    v_refit <- v_refit[v_refit != 1]
    v_refit <- seq_along(janelas) %in% v_refit

    return(v_refit)
}