####################################################################################################
#                          Script para estimacao de modelos do Tempo Real
#
# Este script e rodado de meia em meia hora para (possivel) estimacao de modelos de tempo real. A
# cada rodada sera checado se bate com a hora para estimacao dos modelos e, caso positivo, ajusta e
# salva modelos especificados no arquivo de conf. Do contrario simplesmente encerra sem fazer nada
#
# Embora so se realize a estimacao em uma determinada hora do dia, o script e rodado regularmente de
# meia em meia hora. Assim so e necessario mudar a hora de estimacao no arquivo de conf e nao no
# agendamento de execucao do script
####################################################################################################

# CARREGA CONFIGURACAO E FUNCOES -------------------------------------------------------------------

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

    # Em rodadas normais, dir e o diretorio de trabalho mesmo
    dir <- getwd()
}

# Procura root ou no wd atual ou um nivel acima (vai acontecer em rodada agendada)
# Em ultimo caso, se estiver sendo rodado pelo codigo principal, acha o root pelo nome completo
wd0 <- getwd()
setwd(dir)
if(file.exists("config.yml")) {
    root <- getwd()
    CONFIG <- configr::read.config("config.yml")
} else if(file.exists("../config.yml")) {
    root <- file.path("..")
    CONFIG <- configr::read.config("../config.yml")
} else if(exists("file_p")) {
    root   <- file.path(file_p, "ModTemReal/VersaoAutomatica/Codigos/prevtemporeal")
    CONFIG <- configr::read.config(file.path(root, "config.yml"))
} else {
    # Se nada funcionar, emite erro
    stop("Nao foi possivel localizar um arquivo de configuracoes")
}

# Carrega funcoes necessarias
for(arq in list.files(file.path(root, "R"), full.names = TRUE)) source(arq)
setwd(wd0)

# INICIALIZACAO ------------------------------------------------------------------------------------

# Identifica data-hora atual e arredonda para meia hora inferior
tempoatual <- as.POSIXlt(Sys.time())
tempoatual$sec <- 0
tempoatual$min <- (tempoatual$min > 30) * 30

# Checa horario
if(format(tempoatual, format = "%H:%M") != CONFIG$PARAMS$horafit) stop("")

# Le lista de pontos de conexao
arq <- file.path(CONFIG$CAMINHOS$raiz, CONFIG$CAMINHOS$dadpontos)
v_ptoconex <- read.table(arq, sep = ";", header = FALSE, stringsAsFactors = FALSE)
v_ptoconex <- v_ptoconex[, 1]

# Identifica inicio do historico para ajuste
inihist <- as.POSIXlt(tempoatual - (CONFIG$PARAMS$janela - 1) * 30 * 60)
inidata <- as.numeric(format(inihist, format = "%Y%m%d"))
inihora <- (inihist$hour * 2 + 1) + (inihist$min == 30)

# Le o primeiro historico completo para calcular quantas linhas pular
arq <- file.path(CONFIG$CAMINHOS$raiz, CONFIG$CAMINHOS$gerhist)
arq <- list.files(arq, full.names = TRUE)[1]
hgedummy <- read.table(arq, sep = ";", header = FALSE)
k_pular  <- which(hgedummy[, 1] == inidata) - 1

# LEITURA DE DADOS E ESTIMACAO ---------------------------------------------------------------------

# Vetor de horas para nomear colunas
v_horas <- seq(as.POSIXct("2020-01-01 00:00:00"), as.POSIXct("2020-01-01 23:30:00"), by = "30 min")
v_horas <- c("data", format(v_horas, format = "%H%M"))

for(pto in v_ptoconex) {

    # Le hge e pi do dia anterior
    arq <- file.path(CONFIG$CAMINHOS$raiz, CONFIG$CAMINHOS$gerhist, paste0("HGE_", pto, ".txt"))
    d_hge <- read.table(arq, sep = ";", header = FALSE, skip = k_pular, col.names = v_horas)

    arq <- file.path(CONFIG$CAMINHOS$raiz, CONFIG$CAMINHOS$gerD_1, paste0(pto, ".txt"))
    d_ontem <- read.table(arq, sep = ";", header = FALSE, col.names = v_horas)
    d_ontem$data <- as.numeric(gsub("[^[:digit:]]", "", d_ontem$data)) # corrige um carctere maluco que vem na leitura

    # Le geracao online
    arq <- file.path(CONFIG$CAMINHOS$raiz, CONFIG$CAMINHOS$gerOnline, paste0(pto, ".txt"))
    d_hoje <- read.table(arq, sep = ";", header = FALSE, col.names = v_horas)
    d_hoje$data <- as.numeric(gsub("[^[:digit:]]", "", d_hoje$data)) # corrige um carctere maluco que vem na leitura
    d_hoje[] <- lapply(d_hoje, function(x) if(x < -1e3) NA else x)

    # Junta tudo
    d_hist <- data.matrix(rbind(d_hge, d_ontem, d_hoje))
    d_hist <- d_hist[!duplicated(d_hist[, 1]), -1]

    # Monta serie para estimacao
    serie <- c(t(d_hist))
    serie <- serie[-c(1:(inihora - 1))]
    serie <- serie[!is.na(serie)]
    serie <- ts(serie, start = c(as.numeric(as.Date(inihist)), inihora), freq = 48)

    # Ajusta modelos, preve e salva
    lapply(CONFIG$PARAMS$modelos, function(spec) {
        fit <- estimamodelo(serie, tipo = spec)
        arq <- file.path(CONFIG$CAMINHOS$raiz, CONFIG$CAMINHOS$outmods, paste0(pto, "_", spec, ".RData"))
        save(list = "fit", file = arq)

        arq <- file.path(CONFIG$CAMINHOS$raiz, CONFIG$CAMINHOS$outmods, paste0(pto, "_", spec, ".jpeg"))
        jpeg(arq, width = 3600, height = 2700, res = 350)
        plot(fit, main = paste0(pto, " [", spec, "]"))
        dev.off()
    })
}
