% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/janelamovel.r
\name{expandejanelas}
\alias{expandejanelas}
\title{Janelas Para Execucao Em Janela Movel}
\usage{
expandejanelas(serie, janela, passo)
}
\arguments{
\item{serie}{a serie sobre a qual sera passada a janela movel}

\item{janela}{escalar ou vetor especificando a janela. Ver Detalhes}

\item{passo}{saltos entre cada janela. Ver Detalhes}
}
\value{
lista na qual cada elemento e uma lista de dois elementos, contendo o instante inicial e
final de \code{serie} a considerar em cada janela, no sistema de tempo \code{ts}. Deve ser
notado que os instantes definem uma janela fechada no inicio e aberta no final
}
\description{
Interpreta os argumentos passados a \code{\link{janelamovel}} a respeito da largura da janela
}
\details{
\code{janela} pode ser ou um escalar ou um vetor de dois elementos inteiros. No primeiro caso,
entende-se que deverao ser rodadas janelas de tamanho \code{janela}. Quando este argumento e um
vetor entende-se que todas as janelas devem iniciar no instante de tempo \code{janela[1]} com
largura inicial \code{janela[2]}. Essencialmente estas duas formas permitem a execucao em janela
rolante ou expansivel, respectivamente.

O argumento \code{passo} permite especificar quantas novas observacoes sao incorporadas entre
janelas adjacentes. No caso de janela rolante, cada janela incorpora \code{passos} novos pontos e
abandona os \code{passos} valores mais antigos, de modo que todas tem o mesmo tamanho; janelas
expansiveis vao simplesmente agregando novas observacoes. Deve ser observado que, no caso de
janelas rolantes, sempre havera uma janela contendo o final da serie, mesmo que passos seja um
número tal que da penultima para a ultima janela ocorra um intervalo menor que \code{passos}.
}
