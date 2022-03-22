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
#' isto deve ser especificado claramente em cada projeto, não aqui.
#' 
#' O caminho de busca e composto por tres diretorios:
#' 
#' 1. diretorio de trabalho atual
#' 2. diretorio um nivel acima do de trabalho
#' 3. caminho composto a partir de \code{file_p} e argumento \code{path_principal}
#' 
#' As opcoes 1 e 2 sao dedicadas às rodadas via linha de comando e sessões interativas,
#' respectivamente. A terceira opcao é necessária quando o script for rodado de dentro do codigo
#' principal da eólica. Nesse caso, \code{file_p} corresponderá normalmente ao caminho do D: de uma
#' das máquinas virtuais, de modo que \code{path_principal} precisa conter o restante do trajeto até
#' o diretório onde o arquivo de configuração se encontra. Ver Exemplos
#' 
#' @param path caminho completo até o diretório onde o arquivo de configuração se  encontra. 
#'     Ver Exemplo
#' @param nome o nome do arquivo, com extensão. O nome pode variar, mas deve necessariamente ser um 
#'     "json" ou "jsonc"
#' 
#' @examples
#' 
#' \dontrun{
#' # numa rodada pelo codigo principal, file_p sera criado como, por exemplo
#' file_p <- "D:/ModeloPrevEolico"
#' 
#' # a funcao deve entao ser chamada como
#' path <- file.path(file_p, "ModEolPtoConex/VersaoAutomatica/pasta/contendo/conf")
#' localizaconf(path)
#' }
#' 
#' @return lista contendo o caminho ate arquivo localizado no primeiro elemento e lista de
#'     configuracoes no segundo
#' 
#' @importFrom jsonlite read_json
#' 
#' @export

localizaconf <- function(path, nome = "conf.jsonc") {

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

    # Procura root ou no wd atual ou um nivel acima (vai acontecer em rodada agendada)
    # Em ultimo caso, se estiver sendo rodado pelo codigo principal, acha o root pelo nome completo
    wd0 <- getwd()
    setwd(dir)
    if(file.exists(nome)) {
        root <- getwd()
        CONFIG <- read_json(nome, simplifyVector = TRUE)
    } else if(file.exists("../conf.jsonc")) {
        root <- file.path("..")
        CONFIG <- read_json("../conf.jsonc", simplifyVector = TRUE)
    } else if(!missing(path)) {
        root   <- path
        CONFIG <- read_json(file.path(root, nome), simplifyVector = TRUE)
    } else {
        # Se nada funcionar, emite erro
        stop("Nao foi possivel localizar um arquivo de configuracoes")
    }

    return(list(root, CONFIG))
}