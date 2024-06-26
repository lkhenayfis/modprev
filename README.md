
<!-- README.md is generated from README.Rmd. Please edit that file -->

# modprev

<!-- badges: start -->

[![R-CMD-check](https://github.com/lkhenayfis/modprev/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/lkhenayfis/modprev/actions/workflows/check-standard.yaml)
[![test-coverage](https://github.com/lkhenayfis/modprev/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/lkhenayfis/modprev/actions/workflows/test-coverage.yaml)
[![codecov](https://codecov.io/gh/lkhenayfis/modprev/branch/master/graph/badge.svg?token=LKMK4R8W9N)](https://codecov.io/gh/lkhenayfis/modprev)
<!-- badges: end -->

Arcabouco unificado para estimacao, previsao, simulacao e etc de
diversos tipos de modelos, incluindo aqueles com variaveis explicativas.
Alem de wrappers para metodos tradicionais de modelos estatisticos, e
implementada uma funcao para modelagem e previsao em horizonte rolante,
facilitando testes com novos modelos. As funcoes do pacote operam em
grande parte como wrappers em torno dos metodos especificos de cada tipo
de modelo, visando apresentar uma interface comum para uso de todos
eles, tal que a adicao de novos modelos seja simples.

## Instalacao

Este pacote ainda nao se encontra disponibilizado no CRAN, de modo que
deve ser instalado diretamente a partir do repositorio utilizando:

``` r
# Caso a biblioteca remotes nao esteja instalada, execute install.packages("remotes") primeiro
remotes::install_github("lkhenayfis/modprev", build_vignettes = TRUE) # instalacao da versao de desenvolvimento
remotes::install_github("lkhenayfis/modprev@*release", build_vignettes = TRUE) # instalacao da ultima versao fechada
```

## Exemplo de uso

A principal funcionalidade de `modprev` gira em torno da estimacao e
entao previsao com diferentes tipos de modelos atraves de uma mesma
interface, retornando saidas com mesmo formato. A estimacao dos modelos
e realizada atraves da funcao `estimamodelo`:

``` r
# usando serie gerada aleatoriamente com sazonalidade periodo 12
set.seed(1234)
serie <- ts(arima.sim(list(ar = .7), 240) + 3 * sin(0:11 * 2 * pi / 12), frequency = 12)

m_sarima <- estimamodelo(window(serie, c(1, 1), c(19, 12)), "sarima")
m_ar1saz <- estimamodelo(window(serie, c(1, 1), c(19, 12)), "ss_ar1_saz")
```

A previsao pode ser feita pela generica `predict`:

``` r
pred_sarima <- predict(m_sarima, n.ahead = 12)
pred_ar1saz <- predict(m_ar1saz, n.ahead = 12)
```

### Modelos com variaveis explicativas

Quando `tipo` corresponde a um modelo que use variaveis explicativas,
estas devem ser passadas tanto para a estimacao

``` r
# usando dado interno do pacote
serie <- window(datregdin$obs, 1, 100)
varex <- datregdin$varex[1:100, , drop = FALSE]
m_regdin <- estimamodelo(serie, "ss_reg_din", formula = ~ V1 + V2 * V3, regdata = varex)
```

… quanto para previsao

``` r
newvarex <- datregdin$varex[101:110, , drop = FALSE]
pred_regdin <- predict(m_regdin, newdata = newvarex)
```
