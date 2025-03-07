% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod-lgbm.r
\name{modelos_lightgbm}
\alias{modelos_lightgbm}
\alias{LGBM}
\title{Modelos \code{LGBM}}
\usage{
LGBM(
  serie,
  regdata,
  test_data = list(c(), data.frame()),
  dataset_params = list(),
  train_params = list(),
  ...
)
}
\arguments{
\item{serie}{série para ajustar}

\item{regdata}{\code{data.frame}-like contendo variáveis explicativas}

\item{test_data}{uma lista contento valores out-of-sample da variavel dependente e regressores
que serao utilizados para selecao do criterio de parada}

\item{dataset_params}{lista de parâmetros opcionais para construção do dataset. Veja
\code{\link[lightgbm]{lgb.Dataset}} para mais detalhes}

\item{train_params}{lista de parâmetros opcionais para treinamento do modelo. Veja
\code{\link[lightgbm]{lightgbm}} para mais detalhes}

\item{...}{para estimacao, demais argumentos passados a funcao \code{\link[lightgbm]{lightgbm}};
nas restantes nao possui uso}
}
\value{
Objeto da classe \code{modprev} e subclasse \code{LGBM}, uma lista de dois
elementos: \code{modelo} e \code{serie} contendo o modelo estimado e a série passada
}
\description{
Estimação e métodos de modelos da classe \code{LGBM}

Para estimação deste tipo de modelo, além de ser fornecido o arumento \code{serie} também deve
ser necessariamente informado \code{regdata}, um \code{data.frame}-like contendo as variáveis
explicativas necessárias.
}
\details{
Boosting de árvores pelo algoritmo lgbm estimado atraves de \code{\link[lightgbm]{lightgbm}}.
Para mais detalhes a respeito desta modelagem e seu uso, veja a documentacao oficial do pacote.

Ao contrario da maioria dos modelos com variáveis explicativas neste pacote, não há possibilidade
de passar argumentos \code{formula} para estimação de lightGBMs. Isto se deve ao fato de que o
modelo cresce árvores em todas as variáveis que lhe são informadas. Caso o usuário deseje reduzir
regressores, deve fazê-lo de antemão.

Na execucao em janela movel, o vetor de pesos deve ser passado com o tamanho igual ao da janela.
O mesmo vetor sera aplicado em todas as janelas avaliadas.
}
