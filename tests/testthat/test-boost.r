
ss <- datregdin[[1]]
dd <- datregdin[[2]]

aa <- function(...) bbs(as.data.frame(list(...)))

jj <- janelamovel(ss, regdata = dd, "BOOST", c(1, 150), 10, 10, 3, full.output = TRUE, baselearner = aa)

pp <- lapply(jj, function(j) j[[1]][, 1])
pp <- ts(do.call(c, pp), start(pp[[1]]))

plot(datregdin[[1]])
lines(pp, col = 2)

jj2 <- janelamovel(ss, "ss_reg_din", regdata = dd, formula = ~ V1 + V2 * V3, c(1, 150), 10, 10, 3)
pp2 <- lapply(jj, function(j) j[[1]][, 1])
pp2 <- ts(do.call(c, pp2), start(pp2[[1]]))


jpeg("compara_boost.jpeg", width = 1600, height = 900, res = 150)
plot(window(datregdin[[1]], start = 130))
lines(pp, col = 2)
lines(pp2, col = 4, lty = 2)
legend("bottomleft", inset = .02, legend = c("reg_boost", "reg_canonica"), lty = c(1, 2),
    col = c(2, 4))
dev.off()
