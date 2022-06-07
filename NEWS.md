# redoc

## New features

* Reformulacao extensiva da documentacao do pacote, com inclusao de novos exemplos
* Muda o dado interno de exemplos do pacote para algo que permite exemplificar regressoes dinamicas
  de forma mais completa
* Atualiza o README para rmarkdown

# regdin

## New features

* `ss_reg_din` agora permite a estimacao de regressoes dinamicas com qualquer formula, informadas 
  pelo argumento `formula` como formulas padrao do R
* `ss_reg_din` agora permite a estimacao de modelos com heterocedasticidade, na forma de variancias
  sazonais. Isto e feito atraves de variaveis circulares, nao dummies, de modo que a carga extra
  para estimacao e de apenas um novo hiperparametero

# janelamovel

## New features

* `janelamovel` tem dois novos argumentos: `passo` e `largura`:
  * `passo` permite informar saltos de tempo entre janelas, possibilitando a rodada de menos janelas
    sobre uma mesma serie
  * `janela` substitui o antigo argumento `largura`, permitindo mais flexibilidade. Quando e um 
    escalar, funciona exatamente como `largura`, porem pode tambem ser um vetor indicando na 
    primeira posicao uma observacao fixa para o inicio da janela e na segunda a largura inicial. 
    Estas duas formas configuram uma janela rolante ou expansivel, respectivamente.

# master

## New features

### Minor

* `localizaconf()` agora procura por diretorios `conf`, `confs` ou `config` em cada nivel do
  searchpath. Tambem passa a retornar em `raiz` o diretorio que contem o arquivo ou o pai do
  diretorio `conf`, `confs` ou `config` caso exista. O objetivo disso e que `raiz` corresponda a 
  raiz do projeto contendo os programas, nao o caminho ate o arquivo de confifuracao

# modprev 1.0

Arcabouco unificado para estimacao, previsao, simulacao e etc de diversos tipos de 
modelos, incluindo aqueles com variaveis explicativas. Alem de wrappers para metodos 
tradicionais de modelos estatisticos, e implementada uma funcao para modelagem e previsao em
horizonte rolante, facilitando testes com novos modelos. As funcoes do pacote operam em grande
parte como wrappers em torno dos metodos especificos de cada tipo de modelo, visando apresentar
uma interface comum para uso de todos eles, tal que a adicao de novos modelos seja simples. 