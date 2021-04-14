####################################################################################################
# FUNCOES DIVERSAS E AUXILIARES
####################################################################################################

library(data.table)

#' Extrator de janela do verificado
#' 
#' Funcao para facilitar isolamento de uma parte da serie do verificado para ajuste
#' 
#' @param dat [data.table] dia inicial da janela formato numerico aaaammdd
#' @param data_ini [escalar inteiro] dia inicial da janela formato numerico aaaammdd
#' @param hora_ini [escalar string] hora inicial da janela formato "HH:MM". Padrao "00:00"
#' @param data_fim [escalar inteiro] dia final da janela formato numerico aaaammdd
#' @param hora_fim [escalar string] hora final da janela formato "HH:MM". Padrao "23:00"
#' @param num_dias [escalar inteiro] opcional. Se fornecido em conjunto com data_ini e hora_ini 
#'     calcula data_fim e hora_fim somando num_dias. Se fornecido com data_fim e hora_fim, subtrai
#' @param num_ext [escalar inteiro] similar a num_dias, este parametro aumenta num_ext OBSERVACOES 
#'     para  alem do final da janela. Util para reservar partes out of sample em referencia a uma 
#'     janela
#' 
#' @details Existem duas formas de especificar a janela. A primeira exige ambas as datas e horas
#'     limites da janela. A segunda so precisa de uma borda e o argumento \code{num_dias} para 
#'     calcular a outra ponta. 
#'     Se todos forem fornecidos, i.e duas datas, duas horas e num_dias, num_dias e ignorado
#' 
#'     Quanto a \code{num_ext}, como se refere ao numero de OBSERVACOES e nao de dias, para reservar 
#'     10 horas como out-of-sample o valor deste parametro deve ser
#' 
#'     num_ext = 20 (10 x 2 meia horas a cada hora)
#' 
#' @value [objeto zoo] serie temporal indexada pelo dia-semihora

extraiserie <- function(dat, data_ini, hora_ini = "00:00", data_fim, hora_fim = "23:30", num_dias, num_ext) {

    # Identifica qual tipo de parametro foi fornecido
    tem_di <- !missing(data_ini)
    tem_df <- !missing(data_fim)
    tem_nd <- !missing(num_dias)
    tem_ne <- !missing(num_ext)

    # Checa se da pra fazer alguma coisa
    if(!tem_di & tem_df & tem_nd) {
        fim <- as.POSIXct(paste0(data_fim, hora_fim), format = "%Y%m%d %H:%M", tz = "GMT")
        ini <- fim - (60 * 60 * 24 * num_dias) + (60 * 30)
        hora_ini <- format(ini, format = "%H:%M")
        data_ini <- as.numeric(format(ini, format = "%Y%m%d"))
    } else if(tem_di & !tem_df & tem_nd) {
        ini <- as.POSIXct(paste0(data_ini, hora_ini), format = "%Y%m%d %H:%M", tz = "GMT")
        fim <- ini + (60 * 60 * 24 * num_dias) - (60 * 30)
        hora_fim <- format(fim, format = "%H:%M")
        data_fim <- as.numeric(format(fim, format = "%Y%m%d"))
    } else if(!tem_di & !tem_df) {
        stop("Nao foram fornecidos argumentos suficientes para definicao da janela")
    }

    lin_i <- dat[, which((data == data_ini) & (hora == hora_ini))]
    lin_f <- dat[, which((data == data_fim) & (hora == hora_fim))]

    if(tem_ne) lin_f <- lin_f + num_ext

    out <- dat[lin_i: lin_f, verif]
    start <- as.Date(as.character(data_ini), format = "%Y%m%d")
    out   <- ts(out, start = start, frequency = 48)

    class(out) <- c("ts_TR", class(out))
    attr(out, "out_sample") <- ifelse(tem_ne, num_ext, 0)

    return(out)
}

print.ts_TR <- function(x) {attr(x, "out_sample") <- NULL; class(x) <- "ts"; print(x)}

#' Separador de in e out sample
#' 
#' Funcao que recebe um inteiro e uma serie temporal e separa em dois, mantendo continuidade
#' 
#' @param serie [serie temporal] serie a ser quebrada. Ver detalhes
#' @param int [escalar inteiro] numero de observacoes ao final para separar
#' 
#' @details \code{quebrats} e uma funcao generica com um metodo padrao para objetos ts e outro para 
#'     ts_TR. A unica diferenca entre estes e que o metodo padrao necessita que \code{int} seja 
#'     fornecido (padrao = 0L). Objetos ts_TR ja carregam um indicador de out of sample como 
#'     atributo, que sera usado por padrao, embora possa ser fornecido um novo valor.
#' 
#' @value [lista de ts] lista de dois elementos contendo a serie antes e depois do ponto de quebra

quebrats <- function(serie, int) UseMethod("quebrats")

quebrats.ts <- function(serie, int = 0L) {
    S   <- frequency(serie)
    INI <- start(serie)
    DLT <- length(serie) - int - 1
    FIM <- deltats(INI, delta = DLT, freq = S)
    serie_in <- window(serie, end = FIM)
    if(int != 0) {
        INI <- deltats(INI, delta = DLT+1, freq = S)
        serie_out <- window(serie, start = INI)
    } else {
        serie_out <- NULL
    }

    return(list("in" = serie_in, "out" = serie_out))
}

quebrats.ts_TR <- function(serie, int) {
    if(missing(int)) int <- attr(serie, "out_sample")

    class(serie) <- "ts"
    out <- quebrats(serie, int)
    for(i in out) if(!is.null(i)) class(i) <- c("ts_TR", "ts")

    return(out)
}

#' Indice de tempo por delta em ts
#' 
#' Calcula indice no sistema de tempo de um ts especificada

deltats <- function(ini, fim, delta, freq) {
    tem_ini <- !missing(ini)
    tem_fim <- !missing(fim)
    tem_dlt <- !missing(delta)

    if(tem_ini & !tem_fim & tem_dlt) {
        fim  <- c(NA, NA)
        aux <- ini[2] + delta
        fim[2] <- aux %% freq
        fim[1] <- ini[1] + aux %/% freq
        return(fim)
    }
}
