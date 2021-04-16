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

## Adicionando novos modelos

A incorporacao de novos modelos e relativamente simples. Como mencionado anteriormente, as funcoes
deste pacote sao wrappers de outras, se apresentando como uma interface unificada. Cada uma das
funcoes `estimamodelo`, `predict` e `update` chama uma helper especifica, dependendo do `tipo` de
modelo especificado no ajuste.

Tomando como exemplo `ss_ar1_saz`, ao executar

```r
fit <- estimamodelo(serie, tipo = "ss_ar1_saz")
```

Por baixo dos panos o codigo executa uma funcao chamada `fit_ss_ar1_saz`, que recebe o parametro
`serie` e retorna um modelo ajustado. Este modelo e um objeto de qualquer tipo, apenas sendo
necessario que possua um metodo `predict` definido.

Ainda no caso do tipo `ss_ar1_saz`, a funcao helper e defifida

```r
fit_ss_ar1_saz <- function(serie, ...) {

    # Matrizes de sistema
    Z <- matrix(c(1, 1), 1)
    T <- matrix(c(1, 0, 0, 0), 2)
    R <- matrix(c(0, 1), 2)

    # Especifica modelo, funcao de atualizacao e estima
    mod <- SSModel(serie ~ -1 +
        SSMcustom(Z = Z, T = T, R = R, a1 = c(1, 0), Q = NA) + 
        SSMseasonal(period = 48, sea.type = "dummy", Q = NA), 
        H = 0)
    upfunc <- function(par, model) {
        model["Z", "custom"][1] <- par[1]
        model["T", "custom"][2, 2] <- par[2] / sqrt(1 + par[2]^2)
        model["Q", etas = "custom"] <- exp(par[3])
        model["Q", etas = "seasonal"] <- exp(par[4])
        model["P1", "custom"][2, 2] <- exp(par[3]) / (1 - (par[2] / sqrt(1 + par[2]^2))^2)
        model
    }
    fit <- fitSSM(mod, inits = c(mean(serie),0,0,0), updatefn = upfunc, method = "BFGS")

    # Retorna apenas modelo ajustado
    return(fit$model)
}
```

O obeto `fit$model` retornado e um objeto da classe `SSModel` do pacote `KFAS`, de modo que o
arcabouco padrao de funcoes do R se aplica naturalmente. Outros parametros podem ser passados
atraves de `...`.

O mesmo fluxo se aplica a `predict` e `update`, sendo agora os prefixos para definicao das helpers
`pred_*` e `upd_*`. 

No caso da primeira, a funcao deve receber um modelo ajustado e o parametro `n.ahead` e devolver uma
matriz contendo a media prevista na primeira coluna e desvio padrao na segunda. A seguir esta
`pred_sarima` como exemplo

```r
# model e um objeto da classe Arima, como retornado pelo ajuste com arima() ou forecast::Arima()
pred_sarima <- function(model, ...) {
    prev <- predict(model, ...) # a funcao tira proveito da infraestrutura de previsao ja existente
    prev <- do.call(cbind, prev)
    colnames(prev) <- c("prev", "sd")
    prev
}
```

Por fim, para a helper de update, deve ser informado o modelo e o argumento `newdata` para
atualizacao, e retornar o modelo com os novos dados incorporados.


```r
upd_sarima <- function(model, newdata) Arima(newdata, model = model)
```

**IMPORTANTE**: tanto em `pred_*` quanto `upd_*` o argumento `model` **NAO** e a saida de
`estimamodelo`, mas sim um objeto da classe correspondento ao tipo de modelo. O que e retornado da
`estimamodelo` possui classe e informacoes diferentes, utilizadas em outras situacoes.