test_that("Shift de vetores", {
    vv <- seq(10)
    expect_equal(shift(vv, 0), vv)
    expect_equal(shift(vv, 1), c(10, 1:9))
    expect_equal(shift(vv, 2), c(9:10, 1:8))
    expect_equal(shift(vv, -1), c(2, 3:10, 1))
    expect_equal(shift(vv, -2), c(3, 4:10, 1:2))
})

test_that("Intervalos de tempo em sistema ts", {

    tt <- c(3, 2)

    expect_equal(deltats(tt, 0, 12), tt)

    expect_equal(deltats(tt, 1, 12), c(3, 3))
    expect_equal(deltats(tt, 2, 12), c(3, 4))
    expect_equal(deltats(tt, 10, 12), c(3, 12))

    expect_equal(deltats(tt, 11, 12), c(4, 1))
    expect_equal(deltats(tt, 12, 12), c(4, 2))
    expect_equal(deltats(tt, 20, 12), c(4, 10))

    expect_equal(deltats(tt, 30, 12), c(5, 8))

    expect_equal(deltats(tt, -1, 12), c(3, 1))
    expect_equal(deltats(tt, -2, 12), c(2, 12))
    expect_equal(deltats(tt, -10, 12), c(2, 4))

    expect_equal(deltats(tt, -20, 12), c(1, 6))
})

test_that("Expansao de formula", {
    dd1 <- data.frame(a = 1, b = 2, c = 3)
    expect_warning(ff1 <- expandeformula(dd1))
    expect_equal(as.character(ff1), c("~", "a + b + c"))

    dd2 <- data.frame(V = 1, V4 = 2, BB = 3, CAD = 0, f1j = 10)
    expect_warning(ff2 <- expandeformula(dd2))
    expect_equal(as.character(ff2), c("~", "V + V4 + BB + CAD + f1j"))
})