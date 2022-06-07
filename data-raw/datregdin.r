# Dado dummy para testar funcionalidade da regressao dinamica
devtools::load_all()

geradado <- function(n = 200, seed = 1234) {
    set.seed(seed)
    X <- data.frame(V1 = rnorm(n), V2 = rnorm(n, sd = .5), V3 = rnorm(n, sd = 2))
    y <- ts(2 * X$V1 + .7 * X$V2 - 1.1 * X$V3 + .8 * X$V2 * X$V3 + rnorm(n, sd = .25))
    return(list(y, X))
}
datregdin <- geradado()
names(datregdin) <- c("obs", "varex")

usethis::use_data(datregdin, overwrite = TRUE)
