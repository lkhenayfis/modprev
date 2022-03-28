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

    data <- hora <- verif <- NULL

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
#' Calcula índice no sistema de tempo de um \code{ts} especificada para um dado delta
#' 
#' Função interna utilizada no escopo da janela rolante. Calcula o índice temporal do final de uma
#' janela iniciada em \code{ini} com largura \code{delta}.
#' 
#' @param ini indice temporal no formato (x, y) de serie temporal indicando inicio da janela 
#' @param delta numero de observacoes que a janela deve conter 
#' @param freq frequencia/sazonalidade da serie
#' 
#' @return Se forncecidos \code{ini} e \code{delta}, o índice no sistema de tempo \code{ts}
#'     indicando o final da janela

deltats <- function(ini, delta, freq) {
    fim  <- c(NA, NA)
    aux <- ini[2] + delta
    fim[2] <- aux %% freq
    fim[1] <- ini[1] + aux %/% freq
    return(fim)
}

#' Detecção De Arquivo De Configuração
#' 
#' Busca arquivo de configuração segundo regras que contemplam a execução operacional
#' 
#' Esta função não é necessária para uso do pacote. Ela existe e é fornecida apenas para facilitar
#' as distintas aplicações, removendo a necessidade de repetir esse código para achar a configuração
#' em todos os arquivos. Cada aplicação terá seu conjunto de configurações necessárias, de modo que
#' isto deve ser especificado claramente nos projetos em si, não aqui.
#' 
#' A função executa uma busca simples por um arquivo de configuração chamado \code{arq} em um 
#' search path de dois níveis principais:
#' 
#' 1. diretório de trabalho atual
#' 2. diretório um nível acima do de trabalho
#' 
#' A cada nível será checado se existe o aquivo \code{arq} e, alternativamente, se existem os
#' diretórios 'conf', 'config' ou 'confs' no nível que possam conter \code{arq}.
#' 
#' O argumento \code{path} permite informar diretamente o caminho até o \bold{diretório} que contém 
#' \code{arq}. Caso seja fornecido, um novo nível será adicionado no topo do search path 
#' correspondendo ao caminho informado no argumento \code{path}.
#' 
#' Normalmente \code{path} não tem muita utilidade. Em rodadas interativas ou por cmd apenas os dois 
#' níveis padrão são necessários. No código principal o caminho do script já deve ser conhecido para
#' execução, de modo que a busca por configuração em referência a ele é simples
#' 
#' @param path caminho completo até o diretório onde o arquivo de configuração se encontra.
#' @param arq o nome do arquivo, com extensão. O nome pode variar, mas deve necessariamente ser um 
#'     "json" ou "jsonc"
#' 
#' @return lista contendo o caminho ate arquivo localizado no primeiro elemento e lista de
#'     configuracoes no segundo
#' 
#' @importFrom jsonlite read_json
#' 
#' @export

localizaconf <- function(path, arq = "conf.jsonc") {

    if(!grepl(".jsonc?", arq)) stop("Arquivo de configuracao invalido -- deve ser .json ou .jsonc")

    # Numa rodada por cmd, o diretorio do arquivo pode ser localizado facilmente
    args <- commandArgs()
    dir  <- sapply(args, function(arg) grepl("\\-\\-file", arg))
    if(any(dir)) {
        dir <- args[[which(dir)]]
        dir <- sub("\\-\\-file\\=", "", dir)
        dir <- gsub("\\\\", "/", dir)
        dir <- strsplit(dir, "/")[[1]]
        dir <- do.call(file.path, as.list(dir[-length(dir)]))
    } else {
        # Em rodadas interativas, dir e o diretorio de trabalho sao o mesmo
        dir <- getwd()
    }

    searchpath <- c(
        file.path(".", arq), file.path(".", c("conf", "confs", "config"), arq),
        file.path("..", arq), file.path("..", c("conf", "confs", "config"), arq)
    )

    if(!missing("path")) searchpath <- c(searchpath,
        file.path(path, arq), file.path(path, c("conf", "confs", "config"), arq))

    CONFIG <- NULL
    for(sp in searchpath) {
        if(file.exists(sp)) {
            root <- sub(paste0("(/conf(s|ig)?)?/", arq), "", sp)
            CONFIG <- read_json(sp, simplifyVector = TRUE)
            break
        }
    }
    if(is.null(CONFIG)) stop("Nao foi possivel localizar um arquivo de configuracoes")

    return(list(root, CONFIG))
}