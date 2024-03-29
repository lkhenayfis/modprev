% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.r
\name{shift}
\alias{shift}
\title{Desloca Vetor De \code{i} Posições}
\usage{
shift(v, i)
}
\arguments{
\item{v}{vetor a ser manipulado}

\item{i}{posições para deslocar -- se negativo, para a esquerda, positivo para a direita}
}
\value{
Vetor \code{v} com elementos deslocados \code{i} posições
}
\description{
Desloca os valores de um vetor mantendo o mesmo comprimento original
}
\examples{

vec <- seq(10)
vec_1 <- modprev:::shift(vec, -2) # c(3, 4, 5, 6, 7, 8, 9, 10, 1, 2)
vec_1 <- modprev:::shift(vec, 2)  # c(9, 10, 1, 2, 3, 4, 5, 6, 7, 8)

}
