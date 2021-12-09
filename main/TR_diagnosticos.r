####################################################################################################
#                         Script para diagnosticos de modelos do Tempo Real
#
# Este script e rodado uma vez por dia para calcular os erros de previsao de cada modelo rodando no
# tempo real. A cada rodada sao gerados graficos comparando o desempenho de cada um num horizonte
# de ate seis horas a frente
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

# Le lista de pontos de conexao
arq <- file.path(CONFIG$CAMINHOS$raiz, CONFIG$CAMINHOS$dadpontos)
v_ptoconex <- read.table(arq, sep = ";", header = FALSE, stringsAsFactors = FALSE)
v_ptoconex <- v_ptoconex[, 1]

outplots <- sub("previsoes", "erros", CONFIG$CAMINHOS$outprevs)
outplots <- file.path(CONFIG$CAMINHOS$raiz, outplots)

# CALCULA ERROS E DIAGNOSTICOS ---------------------------------------------------------------------

for(pto in v_ptoconex) {

    # Le hge e lista de previsoes do TR
    attach(CONFIG$CAMINHOS)
    hge <- data.matrix(fread(file.path(raiz, gerhist, paste0("HGE_", pto, ".txt")), na.strings = "999"))
    load(file.path(raiz, outprevs, paste0(pto, ".RData")))
    detach(CONFIG$CAMINHOS)

    ini <- as.Date(as.character(hge[1, 1]), format = "%Y%m%d")
    ini <- c(as.numeric(ini), 1)
    obs <- ts(c(t(hge[, -1])), start = ini, freq = 48)

    fim <- end(obs)[1]

    l_prev <- lapply(l_prev, function(l) {
        v <- sapply(l, function(x) start(x)[1] < fim)
        l[v]
    })

    erros <- lapply(l_prev, function(l) {
        l <- lapply(l, function(v) unname(as.vector(obs - v[, 1] * (v[, 1] > 0))))
        tam <- sapply(l, length)
        if(all(tam == min(tam))) {
            return(do.call(cbind, l))
        } else {
            l <- lapply(l, function(v) c(v, rep(NA, max(tam) - length(v))))
            return(do.call(cbind, l))
        }
    })

    rmse <- sapply(erros, function(m) apply(m, 1, function(v) sqrt(mean(v^2, na.rm = TRUE))))
    maxt <- max(sapply(rmse, length))
    rmse <- sapply(rmse, function(v) c(v, rep(NA, maxt - length(v))))
    rmse <- rmse[complete.cases(rmse), ]
    colregular <- which(colnames(rmse) == "regular")
    rmse <- rmse[, c(colregular, seq(ncol(rmse))[-colregular])]

    png(file.path(outplots, paste0(pto, ".png")), width = 2700, height = 1800, res = 300)
    plot(NA, panel.first = grid(col = "grey85"),
        xlim = c(1, nrow(rmse)), ylim = range(rmse),
        xlab = "Horizonte de previs�o [n x 30 min]", ylab = "Valor", main = paste0("RMSE - ", pto))
    for(i in seq(ncol(rmse))) {
        points(rmse[, i], col = i, type = "o", lwd = 2)
    }
    legend("bottomright", inset = 0.02,
        pch = 1, lty = 1, lwd = 2,
        legend = colnames(rmse), col = seq(ncol(rmse)))
    dev.off()
}
