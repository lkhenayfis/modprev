# Dado dummy para testar funcionalidade da regressao dinamica

dado      <- read.table("data-raw/VenPrev.txt", header = TRUE, sep = ";")
datregdin <- dado[dado$Ano == 2020, c("VenVerif", "VenPrev")]
datregdin[] <- lapply(datregdin, ts)
colnames(datregdin) <- c("obs", "prev")

usethis::use_data(datregdin, overwrite = TRUE)
