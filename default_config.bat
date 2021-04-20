@ECHO off
ECHO # Local dos dados necessarios > config.yml
ECHO CAMINHOS: >> config.yml
ECHO   # Caminho de rede do drive D: da maquina alvo >> config.yml
ECHO   raiz: "//rj-vd-weol-01/d$" >> config.yml
ECHO   # Caminho do arquivo de dados de pontos de conexao >> config.yml
ECHO   dadpontos: "ModeloPrevEolico/ModEolPtoConex/VersaoAutomatica/Arquivos de Entrada/Dados Entrada/Dados_Pontos_Pto_Conex.txt" >> config.yml
ECHO   # Caminho da geracao historica por ponto de conexao >> config.yml
ECHO   gerhist: "ModeloPrevEolico/ModEolPtoConex/VersaoAutomatica/Arquivos de Saída/HGE/Conexao" >> config.yml
ECHO   # Caminho da geracao do dia anterior por ponto de conexao >> config.yml
ECHO   gerD_1: "ModeloPrevEolico/ModTemReal/DadosVerificadosOnline/GerPtoConex_D_1" >> config.yml
ECHO   # Caminho da geracao online por ponto de conexao >> config.yml
ECHO   gerOnline: "ModeloPrevEolico/ModTemReal/DadosVerificadosOnline/GerPtoConex_Online" >> config.yml
ECHO   # Caminho das previsoes regulares por ponto de conexao >> config.yml
ECHO   prevOnline: "ModeloPrevEolico/ModTemReal/VersaoAutomatica/Arquivos de Saída/Acompanhamento Online Previsão/Ponto Conexão/Arquivos" >> config.yml
ECHO   # Caminho dos modelos estimados em estimacao_diaria >> config.yml
ECHO   outmods: "ModeloPrevEolico/ModTemReal/VersaoAutomatica/Arquivos de Saída/Previsao Curto Prazo/modelos" >> config.yml
ECHO   # Caminho das previsoes realizadas estimacao_diaria >> config.yml
ECHO   outprevs: "ModeloPrevEolico/ModTemReal/VersaoAutomatica/Arquivos de Saída/Previsao Curto Prazo/previsoes" >> config.yml
ECHO. >> config.yml
ECHO # Parametros de execucao da previsao >> config.yml
ECHO PARAMS: >> config.yml
ECHO   # Quantidade de pontos para ajuste de modelos >> config.yml
ECHO   janela: 480 >> config.yml
ECHO   # Horizonte de previsao >> config.yml
ECHO   nahead: 12 >> config.yml
ECHO   # Modelos considerados >> config.yml
ECHO   modelos: ["ss_ar1_saz", "sarima"] >> config.yml
ECHO   # Hora do dia para reestimacao dos modelos (precisa ser ou hora cheia ou meia hora) >> config.yml
ECHO   horafit: "03:00" >> config.yml

ECHO.
ECHO Gerado arquivo config.yml em %~dp0
ECHO.

ECHO ################################ AVISO ##############################################
ECHO.
ECHO # O CAMPO _RAIZ_ NA CHAVE _CAMINHOS_ FOI DEFINIDO APONTANDO PARA O D: DA MAQUINA 01 #
ECHO # POR FAVOR, CHEQUE SE ESTE VALOR ESTA ADEQUADO OU NAO E MUDE CONFORME NECESSARIO   #
ECHO.
ECHO #####################################################################################
ECHO.
@pause
