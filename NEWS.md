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