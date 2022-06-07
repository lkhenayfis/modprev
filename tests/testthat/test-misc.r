test_that("Localizacao de arquivo de conf", {
    wd0 <- getwd()
    wd <- system.file("inst/conf", package = "modprev")

    # quando rodando pelo check o pacote e instalado, ai conf vai pro root do pacote, de modo que
    # inst/conf nao existe mais
    if(wd == "") wd <- system.file("conf", package = "modprev")

    # CONF NO MESMO NIVEL DO DIRETORIO LOCAL

    setwd(wd)
    expect_warning(ll <- localizaconf())

    local <- ll[[1]]
    conf  <- ll[[2]]

    expect_equal(local, ".")

    expect_equal(names(conf), c("NIVEL_1", "NIVEL_2"))

    expect_equal(conf$NIVEL_1$indice1, 1)
    expect_equal(conf$NIVEL_1$indice2, "teste")
    expect_equal(conf$NIVEL_1$indice3, 1:4)

    expect_equal(conf$NIVEL_2$indice1, 2)
    expect_equal(conf$NIVEL_2$indice2, "aabb")
    expect_equal(conf$NIVEL_2$indice3, list(a = 1, b = 2))

    # CONF NUMA PASTA ESPECIFICA DO DIRETORIO LOCAL (/conf)

    setwd("..")

    expect_warning(ll <- localizaconf())

    local <- ll[[1]]
    conf  <- ll[[2]]

    expect_equal(local, ".")

    expect_equal(names(conf), c("NIVEL_1", "NIVEL_2"))

    expect_equal(conf$NIVEL_1$indice1, 1)
    expect_equal(conf$NIVEL_1$indice2, "teste")
    expect_equal(conf$NIVEL_1$indice3, 1:4)

    expect_equal(conf$NIVEL_2$indice1, 2)
    expect_equal(conf$NIVEL_2$indice2, "aabb")
    expect_equal(conf$NIVEL_2$indice3, list(a = 1, b = 2))

    setwd(wd0)
})