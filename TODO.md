### IMPLEMENTACAO
- Adicionar a fit_ss os coeficientes ajustados na saida de modelo
- Ferramentas de diagnostico
- Implementar selecao de tipo de modelo por ponto no arquivo de conf
- Controlar negativos e numeros maiores que a capaciadade da usina
- Leitura das previsoes de tempo real oficiais para adicionar a lista

- Possivelmente reformular a parte de ajuste, print, predict e save dos modelos, feitas de uma vez

### Testes
- Checar datas da extracao de dado historico
- Checar continuidade das previsoes feitas apos update de modelo

### MODELAGEM
- Estudar possiveis diferencas na inicializacao da sazonalidade no modelo em espaco de estados
- Auto regressoes de horizontes sucessivos (possivelmente dinamica)
- Modelos ETS do Hyndman
