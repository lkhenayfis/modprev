
test_that("Estimacao de modelo S.S. AR1+Saz", {
    mod <- estimamodelo(AirPassengers, tipo = "ss_ar1_saz")

    expect_equal("ss_ar1_saz", attr(mod, "tipo"))
    expect_equal(mod$modelo["Q"][1, 1, 1], 17.183724) # precalculados e testados para garantir
    expect_equal(mod$modelo["Q"][2, 2, 1], 172.71706)
    expect_equal(mod$modelo["T"]["custom2", "custom2", 1], 0.9957003)
})
