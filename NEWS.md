# janelamovel

## New features

### Minor

* `estimamodelo` agora faz uma critica acerca dos argumentos extras passados via `...`. Estes sao
  comparados com os argumentos do `tipo` especificado e apenas aqueles com match positivo permanecem
* unifica `janelamovel` numa unica funcao

# master

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