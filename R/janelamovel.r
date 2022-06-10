########################################### JANELA MOVEL ###########################################

#' Previsão Em Horizonte Rolante
#' 
#' Função para realizar previsões e reajustes em janela móvel
#' 
#' Os argumentos \code{serie} e \code{tipo} tem exatamente o mesmo efeito daqueles descritos em 
#' \code{\link{estimamodelo}}.
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
#' O comportamento dessa função é mais fortemente impactado por \code{refit.cada}. Através deste 
#' argumento é possível indicar intervalos de tempo nos quais o modelo será reestimado. Nas demais 
#' janelas será feita apenas a atualização das informações. Caso \code{refit.cada = NA} (o padrão),
#' o modelo só será ajustado uma vez e atualizado a cada nova janela.
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
#' @param janela especificação da janela móvel ou expansível
#' @param passo inteiro de saltos temporais entre cada janela. Ver Detalhes
#' @param n.ahead número de passos à frente para prever a cada passo
#' @param refit.cada escalar indicando de quantas em quantas observacoes o modelo deve ser 
#'     reajustado
#' @param verbose Escalar inteiro indicando quanta informacao a ser emitida durante rodada. 
#'     0 = nenhuma, 1: toda vez que reajusta modelo, 2: todo horizonte de previsão e reajuste
#' @param ... não possui uso até a versão corrente
#' 
#' @examples 
#' 
#' # janela rolante de cinco anos, prevendo um ano a frente e rolando de seis em seis meses
#' jm_sarima <- janelamovel(AirPassengers, "sarima", 60, passo = 6, n.ahead = 12)
#' 
#' # mesma configuracao, porem janela extensivel (inicio fixo no primeiro mes)
#' jm_sarima <- janelamovel(AirPassengers, "sarima", c(1, 60), passo = 6, n.ahead = 12)
#' 
#' # VARIAVEIS EXPLICATIVAS -----------------------------------------
#' 
#' # serie deve conter n.ahead menos observacoes que a variavel explicativa
#' serie <- window(datregdin$obs, 1, 190)
#' varex <- datregdin$varex
#' jm_regdin <- janelamovel(serie, "ss_reg_din", 100, passo = 10, n.ahead = 5, regdata = varex)
#' 
#' # MODO VERBOSE ---------------------------------------------------
#' 
#' \dontrun{
#' jm_sarima <- janelamovel(AirPassengers, "sarima", 60, 2, 12, verbose = 0)
#' jm_sarima <- janelamovel(AirPassengers, "sarima", 60, 2, 12, verbose = 1, refit.cada = 12)
#' jm_sarima <- janelamovel(AirPassengers, "sarima", 60, 2, 12, verbose = 2, refit.cada = 3)
#' }
#' 
#' @return lista contendo previsoes de 1 a n.ahead passos à frente para cada janela
#' 
#' @export

janelamovel <- function(serie, tipo, janela, passo = 1L, n.ahead = 1L, refit.cada = NA, verbose = 0, ...) {

    args <- list(...)
    if("largura" %in% names(args)) {
        warning("'largura' nao e mais suportado -- use 'janela' no lugar")
        janela <- args$largura
    }
    if("refit_cada" %in% names(args)) {
        warning("'refit_cada' nao e mais suportado -- use 'refit.cada' no lugar")
        refit.cada <- refit_cada
    }

    if(!is.ts(serie)) serie <- ts(serie)

    has_regdata <- "regdata" %in% ...names()
    if(!has_regdata) {
        regdata <- NULL
    } else {
        regdata <- args$regdata
        args$regdata <- NULL
    }

    if(has_regdata && (length(serie) + n.ahead > nrow(regdata))) {
        warning("'regdata' deve conter 'length(serie) + n.ahead' observacoes -- reduzindo 'serie'")

        reduz <- length(serie) + n.ahead - nrow(regdata)
        serie <- window(serie, start(serie), deltats(end(serie), -reduz, frequency(serie)))
    }

    verb_func <- verbose_fun(verbose)
    janelas <- expandejanelas(serie, janela, passo)
    v_refit <- expanderefit(janelas, refit.cada)

    # variavel auxiliar para fazer o subset de newdata pareado com serie
    aux <- ts(seq_len(length(serie) + n.ahead), start = start(serie), frequency = frequency(serie))

    # Estima um modelo inicial que vai ser usado na primeira janela e atualizado dali em diante
    ij <- janelas[[1]]
    iserie   <- window(serie, ij[[1]], ij[[2]])
    iregdata <- regdata[window(aux, ij[[1]], ij[[2]]), , drop = FALSE]
    mod <- c(list(quote(estimamodelo), serie = iserie, tipo = tipo, regdata = iregdata), args)
    mod <- eval(as.call(mod), parent.frame(), parent.frame())

    jm <- lapply(seq(janelas), function(i) {

        verb_func(janelas[[i]][[1]], janelas[[i]][[2]], v_refit[i])

        ij <- janelas[[i]]
        iserie   <- window(serie, ij[[1]], ij[[2]])
        iregdata <- regdata[window(aux, ij[[1]], ij[[2]]), , drop = FALSE]
        mod      <- update(mod, iserie, newregdata = iregdata, refit = v_refit[i])

        ijn <- list(deltats(ij[[2]], 1, frequency(aux)), deltats(ij[[2]], n.ahead, frequency(aux)))
        inewdata <- regdata[window(aux, ijn[[1]], ijn[[2]]), , drop = FALSE]
        predict(mod, n.ahead, newdata = inewdata)
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
#'     0 = nada, 1: toda vez que reajusta modelo, 2: todo horizonte de previsão e reajuste
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
#' número tal que da penultima para a ultima janela ocorra um intervalo menor que \code{passos}.
#' 
#' @param serie a serie sobre a qual sera passada a janela movel
#' @param janela escalar ou vetor especificando a janela. Ver Detalhes
#' @param passo saltos entre cada janela. Ver Detalhes
#' 
#' @return lista na qual cada elemento e uma lista de dois elementos, contendo o instante inicial e
#'     final de \code{serie} a considerar em cada janela, no sistema de tempo \code{ts}. Deve ser 
#'     notado que os instantes definem uma janela fechada no inicio e aberta no final
#' 
#' @importFrom utils tail

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