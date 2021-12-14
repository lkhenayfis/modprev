####################################################################################################
# FUNCOES DIVERSAS E AUXILIARES
####################################################################################################

#' Extrator de Janela do Verificado
#' 
#' Função para facilitar isolamento de uma parte da série de históricos no formato padrão
#' 
#' O formato padrao ao qual se refere é aquele mais tipicamente encontrado nos históricos dos 
#' modelos de eólica e solar, isto é, data.frames de 49 colunas onde a primeira corresponde a data
#' e as seguintes valores por meia hora do dia
#' 
#' |    V1    | V2 | V3 | V4 | ... | V49 |
#' | -------- | -- | -- | -- | --- | --- |
#' | 20170101 | XX | XX | XX | ... |  XX |
#' | 20170102 | XX | XX | XX | ... |  XX |
#' | 20170103 | XX | XX | XX | ... |  XX |
#' 
#' Existem duas formas de especificar a janela. A primeira e mais simples consiste em passar ambas 
#' as datas e horas limite \code{data_ini}, \code{hora_ini}, \code{data_fim}  \code{hora_fim}. Uma
#' segunda forma alternativa permite a especificação de apenas uma ponta da janela em combinação com
#' o argumento opcional \code{num_dias}, de modo que o outro limite da janela será calculado de 
#' acordo. Caso todos os parâmetros sejam fornecidos, \code{num_dias} será ignorado
#' 
#' @param dat data.frame padrao contendo coluna de data e 48 colunas de valores (uma por meia hora)
#' @param data_ini inteiro (formato numerico aaaammdd) indicando data inicial da janela
#' @param hora_ini string (formato "HH:MM") indicando hora inicial da janela. Padrao "00:00"
#' @param data_fim inteiro (formato numerico aaaammdd) indicando data final da janela
#' @param hora_fim string (formato "HH:MM") indicando hora final da janela. Padrao "23:00"
#' @param num_dias parametro opcional, se fornecido deve ser um inteiro indicando numero total de 
#'     dias para extracao
#' 
#' @return serie temporal na janela especificada
#' 
#' @export

extraiserie <- function(dat, data_ini, hora_ini = "00:00", data_fim, hora_fim = "23:30", num_dias) {

    dat <- as.data.table(dat)

    # Identifica qual tipo de parametro foi fornecido
    tem_di <- !missing(data_ini)
    tem_df <- !missing(data_fim)
    tem_nd <- !missing(num_dias)

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

    out <- dat[lin_i: lin_f, verif]
    start <- as.Date(as.character(data_ini), format = "%Y%m%d")
    out   <- ts(out, start = start, frequency = 48)

    return(out)
}

#' Índice De Tempo Por Delta Em Série Temporal
#' 
#' Calcula índice no sistema de tempo de um ts especificada para um dado delta

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
