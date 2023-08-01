library(aws.s3)
library(arrow)

cods    <- c("1_1", "6_6", "8_8")
objects <- paste0("vazoes_", cods, ".parquet.gzip")
objects <- paste0("modelos/cenarios/modelos-comuns/vazoes/", objects)

BUCKET <- "s3://ons-pem-historico"

seriepar <- lapply(objects, function(obj) s3read_using(read_parquet, object = obj, bucket = BUCKET))
seriepar <- lapply(seriepar, function(d) ts(d$incremental, start = c(1931, 1), freq = 12))
seriepar <- do.call(cbind, seriepar)
colnames(seriepar) <- c("serie1", "serie2", "serie3")

usethis::use_data(seriepar, overwrite = TRUE)
