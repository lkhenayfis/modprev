# gam

## New features

* Adiciona Modelos Aditivos Generalizados através do tipo de modelo `GAM` 

# master

## Bug fixes

* Corrige implementacao de heterocedasticidade em modelos de regressao dinamica. Faltava um termo
  constante nos harmonicos estimados
* `janelamovel` tinha um erro quando `refit.cada > 1`. Como rodava o loop em `lapply`, `update`s sem
  reajuste estavam sempre usando o modelo inicial

## Misc

* Diversas modificacoes na interface de `ss_reg_din`
  * `vardin` agora so pode ser um booleano. Para que a serie seja estimada com heterocedasticidade
    ela deve ter sazonalidade
  * opcao `estatica` foi removida, pois ja existe o modelo de regressao linear simples 
    implementado no pacote
  * encolhimento de variancias na regressao dinamica foi removido, pois os resultados obtidos nao
    foram satisfatorios.

# modprev 1.8

## New features

* `ss_reg_din` agora recebe argumento `lambda`, uma penalidade aplicada ao traco da matriz Q nos
  modelos para controle da variabilidade das regressoes
* Adiciona funcao `CV_regdin` para validacao cruzada da penalidade `lambda` 

# modprev 1.7

## New features

* `reg_lin` agora pode receber pesos para a estimacao
* Adiciona modelagem por regressao quantilica atraves da especificacao de tipo `reg_quant`
* Melhorias nos modelos `sarima(x)`:
  * o argumento `...` agora permite que os demais argumentos de `auto.arima` sejam passados para a
    estimacao destes tipos de modelo. Isto permite maior controle sobre a pesquisa de especificacao
  * caso `order` e/ou `seasonal` sejam passados para a estimacao, sera retornado o modelo com esta
    especificacao exata, pulando a parte de pesquisa do `auto.arima`

# modprev 1.6.1

## Bug fixes

* `update.sarimax` sem `refit` estava devolvendo objetos com classe `sarima`, de modo que a previsao
  seguinte quebra. Isto afetava a execucao de janelas moveis com este modelo

# modprev 1.6

## New features

* Introduz modelos `SARIMAX` por meio de regressões lineares com erros SARIMA.
* Itroduz modelos periodicos. Esta classe e uma generalizacao dos modelos unicos em que cada estacao
  de uma serie sazonal tem seu proprio modelo, mantendo a mesma interface de estimacao, atualizacao
  e previsao. Ainda, qualquer modelagem unica introduzida no pacote sera automaticamente compativel
  com a representacao periodica por contrucao.
* Adiciona modelo de regressao linear. A modelagem estatica de `ss_reg_din` nao funciona num 
  ambiente de janela rolante pois a filtragem inicia em tempos diferentes, o que leva a coeficientes
  diferentes mesmo sem reestimacao

# modprev 1.5

## New features

* `ss_reg_din` agora recebe argumento `estatica`, o que permite a estimacao de regressoes com 
  coeficientes fixos (nao variando no tempo)
* `janelamovel` agora tem argumento `full.output`, um booleano indicando o nivel de complexidade da
  saida. Se true, retorna alem da previsao o modelo e variaveis explicativas utilizados

### Minor

* `update`s agora retornam pelo construtor interno, conferindo mais robustez (#10)
* Objetos `modprev` agora podem carregar atributos genericos pertinentes ao modelo estimado (#11)

# modprev 1.4.5

## New features

### Minor

* `estimamodelo` agora faz uma critica acerca dos argumentos extras passados via `...`. Estes sao
  comparados com os argumentos do `tipo` especificado e apenas aqueles com match positivo permanecem
* unifica `janelamovel` numa unica funcao

## Misc

* Remove `extraiserie` e `localizaconf` do pacote e dependencias `data.table` e `jsonlite` que eram
  associadas a estas funcoes

# modprev 1.4.4

## New features

### Minor

* `estimamodelo` agora recebe em `tipo` nome completo do modelo a ser estimado, como simbolo ou
  string. A chamada interna foi reformulada para simplificar a incorporacao de novos modelos

## Bug fixes

* `JANELAMOVEL.ss_reg_din` nao estava usando a `formula` passada. Adicionalmente `ss_reg_din` agora 
  emite um aviso quando `formula` e omitido
* `parsedesloc` nao estava correta na ausencia de heterocedasticidade com series sazonais, e emitia
  um aviso incorretamente
* `JANELAMOVEL.ss_reg_din` nao possuia argumento `vardin`

# modprev 1.4.1

## New features

* Reformulacao extensiva da documentacao do pacote, com inclusao de novos exemplos
* Melhorias em `ss_reg_din` 
  * agora permite a estimacao de regressoes dinamicas com qualquer formula, informadas via argumento
    `formula` como formulas padrao do R
  * agora permite a estimacao de modelos com heterocedasticidade, na forma de variancias sazonais. 
    Isto e feito atraves de variaveis circulares, nao dummies, de modo que a carga extra para 
    estimacao e de apenas um novo hiperparametero
* `janelamovel` tem dois novos argumentos: `passo` e `largura`:
  * `passo` permite informar saltos de tempo entre janelas, possibilitando a rodada de menos janelas
    sobre uma mesma serie
  * `janela` substitui o antigo argumento `largura`, permitindo mais flexibilidade. Quando e um 
    escalar, funciona exatamente como `largura`, porem pode tambem ser um vetor indicando na 
    primeira posicao uma observacao fixa para o inicio da janela e na segunda a largura inicial. 
    Estas duas formas configuram uma janela rolante ou expansivel, respectivamente.

### Minor

* `localizaconf` e `extraidados` foram deprecadas. Ambas estao marcadas para serem removidas do 
  pacote na proxima versao
* Muda o dado interno de exemplos do pacote para algo que permite exemplificar regressoes dinamicas
  de forma mais completa
* Atualiza o README para rmarkdown

## Bug fixes

* Corrige `update` de `sarima` e `ss_ar1_saz`. Estava retornando o objeto com apenas o modelo 
  atualizado, mantendo a serie antiga. Agora atualiza a serie para `newseries`

# modprev 1.0

Arcabouco unificado para estimacao, previsao, simulacao e etc de diversos tipos de modelos,
incluindo aqueles com variaveis explicativas. Alem de wrappers para metodos tradicionais de modelos
estatisticos, e implementada uma funcao para modelagem e previsao em horizonte rolante, facilitando
testes com novos modelos. As funcoes do pacote operam em grande parte como wrappers em torno dos
metodos especificos de cada tipo de modelo, visando apresentar uma interface comum para uso de todos
eles, tal que a adicao de novos modelos seja simples. 