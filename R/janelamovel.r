########################################### JANELA MOVEL ###########################################

#' Previsão Em Horizonte Rolante
#' 
#' Função para realizar previsões e reajustes em janela móvel
#' 
#' Para cada janela de tamanho \code{largura} na \code{serie} fornecida será realizada uma previsão
#' \code{n.ahead} passos à frente, retornadas em uma lista. Isto significa que a última previsão 
#' realizada cobrirá até \code{n.ahead} além da última observação em \code{serie}.
#' 
#' O comportamento dessa função é mais fortemente impactado por \code{refit.cada}. Através deste 
#' argumento é possível indicar observações ou intervalos de tempo nos quais o modelo será 
#' reestimado. Nas demais janelas será feita apenas a atualização das informações.
#' 
#' Caso o modelo escolhido necessite de variáveis explicativas, é necessário que o argumento 
#' \code{regdata} seja passado na forma de uma matriz ou data.frame contendo apenas as colunas com 
#' variáveis a serem utilizadas. Se houver apenas uma variável explicativa, pode ser passada como um
#' vetor ou série temporal.
#' 
#' Este argumento \emph{DEVE CONTER AS VARIÁVEIS EXPLICATIVAS CORRESPONDENTES A TODAS AS OBSERVAÇÕES
#' DA SÉRIE MAIS \code{n.ahead} À FRENTE}. A primeira parte dessa restrição é natural, pois são 
#' necessárias as variáveis explicativas insample para ajustes do modelo. As observações 
#' \code{n.ahead} passos à frente do final da série são necessárias apenas para a previsão das 
#' últimas janelas.
#' 
#' @param serie serie temporal pela qual passar a janela movel
#' @param tipo tipo de modelo a ser ajustado, caso \code{objeto} seja uma serie temporal
#' @param largura numero de observacoes na janela movel
#' @param n.ahead numero de passos a frente para prever a cada passo
#' @param refit.cada escalar ou vetor inteiro. Se escalar, reajusta o modelo a cada 
#'     \code{refit.cada} observacoes. Se vetor, reajusta apos cada indice de \code{refit.cada}
#' @param verbose Escalar inteiro indicando quanta informacao a ser emitida durante rodada. 
#'     0 = nenhuma, 1: toda vez que reajusta modelo, 2: todo horizonte de previsao e reajuste
#' @param ... demais parametros pertinentes a cada tipo específico de modelagem
#' 
#' @examples 
#' 
#' \dontrun{
#' serie <- ts(window(datregdin[[1]], 1, 250))
#' jm <- janelamovel(serie, "ss_ar1_saz", 200, refit.cada = 10)
#' 
#' serie <- ts(window(datregdin[[1]], 1, 720), freq = 48)
#' jm <- janelamovel(serie, "ss_ar1_saz", 480, refit.cada = 48)
#' 
#' serie <- ts(window(datregdin[[1]], 1, 250))
#' varex <- datregdin[1:255, 2, drop = FALSE]
#' jm <- janelamovel(serie, "ss_reg_din", 200, 5L, refit.cada = 10, regdata = varex)
#' 
#' # como esta funcao pode demorar um tempo para rodar (especialmente com multiplos refits), pode
#' # ser conveniente mandar o log para um arquivo
#' sink("log_janelamovel.txt")
#' jm <- janelamovel(serie, "ss_ar1_saz", 200, refit.cada = 10, verbose = 2)
#' sink()
#' }
#' 
#' @return lista contendo previsoes de 1 a n.ahead passos a frente para cada janela
#' 
#' @export

janelamovel <- function(serie, tipo, janela, passo = 1L, n.ahead = 1L, refit.cada = NA, verbose = 0, ...) {
    args <- list(...)
    if("largura" %in% names(args)) {
        warning("'largura' nao e mais suportado -- use 'janela' no lugar")

        janela <- args$largura
    }

    tipo <- structure(tipo, class = tipo)
    JANELAMOVEL(tipo, serie, janela, passo, n.ahead, refit.cada, verbose, ...)
}

# BACKEND ------------------------------------------------------------------------------------------

JANELAMOVEL <- function(tipo, serie, janela, passo, n.ahead, refit.cada, verbose, ...) {
    UseMethod("JANELAMOVEL")
}

JANELAMOVEL.default <- function(tipo, serie, janela, passo, n.ahead, refit.cada, verbose, ...) {

    if(!is.ts(serie)) serie <- ts(serie)
    verb_func <- verbose_fun(verbose)
    janelas <- expandejanelas(serie, janela, passo)
    v_refit <- expanderefit(janelas, refit.cada)

    # Estima um modelo inicial que vai ser usado na primeira janela e atualizado dali em diante
    iserie <- window(serie, janelas[[1]][[1]], janelas[[1]][[2]])
    mod    <- estimamodelo(serie = iserie, tipo = tipo)

    jm <- lapply(seq(janelas), function(i) {

        verb_func(janelas[[i]][[1]], janelas[[i]][[2]], v_refit[i])

        iserie <- window(serie, janelas[[i]][[1]], janelas[[i]][[2]])
        mod    <- update(mod, iserie, refit = v_refit[i])

        predict(mod, n.ahead = n.ahead)
    })

    # Retorna
    return(jm)
}

JANELAMOVEL.ss_reg_din <- function(tipo, serie, janela, passo, n.ahead, refit.cada, verbose,
    formula, regdata, ...) {

    if(!is.ts(serie)) serie <- ts(serie)

    if(length(serie) + n.ahead > nrow(regdata)) {
        warning("'regdata' deve conter 'length(serie) + n.ahead' observacoes -- reduzindo 'serie'")

        reduz <- length(serie) + n.ahead - nrow(regdata)
        serie <- window(serie, start(serie), deltats(end(serie), -reduz, frequency(serie)))
    }

    verb_func <- verbose_fun(verbose)
    janelas <- expandejanelas(serie, janela, passo)
    v_refit <- expanderefit(janelas, refit.cada)

    aux <- ts(seq_len(nrow(regdata)), start = start(serie), frequency = frequency(serie))

    # Estima um modelo inicial que vai ser usado na primeira janela e atualizado dali em diante
    ij <- janelas[[1]]
    iserie   <- window(serie, ij[[1]], ij[[2]])
    iregdata <- regdata[window(aux, ij[[1]], ij[[2]]), , drop = FALSE]
    mod      <- estimamodelo(serie = iserie, tipo = tipo, regdata = iregdata)

    jm <- lapply(seq(janelas), function(i) {

        verb_func(janelas[[i]][[1]], janelas[[i]][[2]], v_refit[i])

        ij <- janelas[[i]]
        iserie   <- window(serie, ij[[1]], ij[[2]])
        iregdata <- regdata[window(aux, ij[[1]], ij[[2]]), , drop = FALSE]
        mod      <- update(mod, iserie, newregdata = iregdata, refit = v_refit[i])

        ijn <- list(deltats(ij[[2]], 1, frequency(aux)), deltats(ij[[2]], n.ahead, frequency(aux)))
        inewdata <- regdata[window(aux, ijn[[1]], ijn[[2]]), , drop = FALSE]
        predict(mod, newdata = inewdata)
    })

    # Retorna
    return(jm)
}

# HELPERS ------------------------------------------------------------------------------------------

#' Selecao Da Funcao De Log
#' 
#' Retorna uma funcao para prints informativos durante a execucao
#' 
#' @param verbose inteiro indicando o nivel de detalhe a ser informado:
#'     0 = nada, 1: toda vez que reajusta modelo, 2: todo horizonte de previsao e reajuste
#' 
#' @return funcao para uso interno na janela movel que printa o estado de execucao

verbose_fun <- function(verbose) {

    verb_func <- switch(as.character(verbose),
        "0" = function(...) NULL,
        "1" = function(i, f, r) if(r) cat("\t Prevendo serie [", i, "] -> [", f, "]\n") else NULL,
        "2" = function(i, f, r) if(r) {
            cat("REFIT -- Prevendo serie [", i, "] -> [", f, "]\n")
        } else {
            cat("\t Prevendo serie [", i, "] -> [", f, "]\n")
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
#' numero tal que da penultima para a ultima janela ocorra um intervalo menor que \code{passos}.
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

    if(length(janela) > 1) {
        inifix <- TRUE
    } else {
        inifix <- FALSE
        janela <- c(1, janela)
    }

    v_ends <- seq(janela[2], N - (janela[1] - 1), by = passo)

    ultimo <- N - (janela[1] - 1)
    if(tail(v_ends, 1) != ultimo) v_ends <- c(v_ends, ultimo)

    if(inifix) {
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
    if((length(refit.cada) == 1) && is.na(refit.cada)) refit.cada <- length(janelas)

    if(length(refit.cada) == 1) {
        v_refit <- seq(1, nj, by = refit.cada)
    } else {
        stop("'refit.cada' como vetor de indices ainda nao implementado")
    }

    v_refit <- v_refit[v_refit != 1]
    v_refit <- seq_along(janelas) %in% v_refit

    return(v_refit)
}