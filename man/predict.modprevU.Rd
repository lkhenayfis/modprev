% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classe-modprevU.r
\name{predict.modprevU}
\alias{predict.modprevU}
\title{Previsão De Modelos \code{modprevU}}
\usage{
\method{predict}{modprevU}(object, n.ahead, ...)
}
\arguments{
\item{object}{modelo ajustado através de \code{\link{estimamodelo_U}}}

\item{n.ahead}{número de passos à frente para prever}

\item{...}{Opcionalmente, pode ser passado o argumento \code{newdata} \code{data.frame}-like
contendo variaveis explicativas fora da amostra para modelos que necessitem}
}
\value{
série temporal multivariada contendo a previsão e o desvio padrão associado para os
passos de tempo \code{1:n.ahead}
}
\description{
Wrapper para previsão de modelos unicos
}
\seealso{
Other Metodos modprevU: 
\code{\link{estimamodelo_U}()},
\code{\link{update.modprevU}()}
}
\concept{Metodos modprevU}
