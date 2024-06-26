% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classe-modprevP.r
\name{new_modprevP}
\alias{new_modprevP}
\title{Construtor Interno De \code{modprevP}}
\usage{
new_modprevP(fits, serie, atrs)
}
\arguments{
\item{fits}{lista de modelos parciais estimados, cada um um objeto \code{modprevU}}

\item{serie}{serie para qual o modelo periodico foi estimado}

\item{atrs}{lista nomeada contendo atributos extras pertinentes ao modelo. Ver Detalhes}
}
\value{
Objeto da classe \code{modprev} e subclasse igual a \code{modprevP}, uma lista de dois
elementos: \code{modelos} e \code{serie} contendo os modelos parciais estimados e a série
passada. Adicionalmente, se \code{atrs} for passada, um atributo "mod_atrs" contendo o
argumento homônimo.
}
\description{
Função interna, não deve ser chamada diretamente pelo usuário
}
\details{
\code{atrs} Existe para permitir que outras informacoes, nao necessariamente contidas no objeto
do modelo (como por exemplo a formula de regressao nos modelos de regressao dinamica), sejam
passadas adiante para os metodos de cada modelagem. A lista aqui passada sera adicionada ao
objeto \code{modprev} de saida como um atributo chamado "mod_atrs".
}
