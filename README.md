# prevtemporeal

Conjunto de funcoes para estimacao de modelos e subsequente previsao da geracao eolica em
curtissimoprazo. Estas funcoes sao um wrapper de diversas formas de modelagem, com o objetivo de
apresentar uma interface comum independente do tipo de modelo ajustado

## Modelos implementados

Ate o momento apenas dois tipos de modelo foram implementados

* SARIMA(p,d,q)(P,D,Q)(48)
* Espaco de Estados AR(1) + Sazonalidade(48)

Ainda ha uma serie de possibilidades para extensao do codigo. A unica restricao imposta e que nao se
utilizem variaveis explicativas, ainda que seja possivel incorporar facilmente esta funcionalidade
no codigo

## Guia rapido

### Estimacao

Ajustar um modelo de determinado tipo e simples

```r
# Ajuste de um SARIMA
fit <- estimamodelo(serie, tipo = "sarima")

# Ajuste do espaco de estados
fit <- estimamodelo(serie, tipo = "ss_ar1_saz")
fit <- estimamodelo(serie, tipo = "ss") # abreviacoes sao permitidas, contanto que haja match unico
```

Onde `serie` representa um objeto serie temporal. Caso seja um vetor, internamente ocorrera a
conversao para tipo `ts`.

**IMPORTANTE**: caso `serie` seja um vetor, a conversao para `ts` **NAO** identifica sazonalidade, de
modo que o modelo SARIMA sera estimado como um ARIMA simples

### Previsao

Uma vez ajustado o modelo, a previsao prossegue de forma tradicional em R

```r
# Previsao de qualquer um dos modelos 10 passos a frente, por exemplo
previsao <- predict(fit, n.ahead = 10L)
```

Sera retornada uma matriz contendo, na primeira coluna, a media prevista e na segunda o desvio
padrao, para cada passo a frente

### Atualizacao

Em muitos casos se deseja aplicar um modelo ja estimado a um novo conjunto de dados (previsao
online) sem a necessidade de reestimar parametros, o que pode ser demorado.

Para atualizar um modelo ja estimado com uma nova serie, basta

```r
# Atualizacao de qualquer tipo de modelo para considerar uma nova serie chamada serie2
fit_atualizado <- update(fit, newdata = serie2)

# Caso seja desejavel reestimar o modelo
fit_atualizado <- update(fit, newdata = serie2, refit = TRUE) # refit e, por padrao, = FALSE
```

### Janela movel

A ultima funcionalidade e a previsao em janela movel, util para teste e levantamento de performance
de modelos (existentes e novas implementacoes)

```r
# Previsao ao longo de serie_longa
janela <- janelamovel(serie_longa, tipo = "ss", 
    largura = 480L, # numero de observacoes dentro da janela -- 480 pontos = 10 dias
    n.ahead = 10L, # horizonte de previsao a cada passo da janela
    refit.cada = 48L, # reajusta o modelo a cada refit.cada observacoes -- 48 = a cada dia
    verbose = 0L) # detalhe da execucao -- 0 = nada, 1 = aviso a cada refit, 2 = aviso a cada passo

# Ainda e possivel definir refit.cada como um vetor de posicoes em serie_longa para reajustar
janela <- janelamovel(serie_longa, tipo = "ss", largura = 480L, n.ahead = 10L,
    refit.cada = seq(490, 1000, by = 30))
```

