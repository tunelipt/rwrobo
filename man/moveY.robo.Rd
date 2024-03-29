% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wroboclient.R
\name{moveY.robo}
\alias{moveY.robo}
\title{Movimenta robô cartesiano no eixo y}
\usage{
\method{moveY}{robo}(dev, x, a = FALSE, r = FALSE, sync = FALSE)
}
\arguments{
\item{dev}{Um objeto de classe \code{robo} contendo informações sobre servidor XML-RPC}

\item{x}{Número informando o quanto se deve movimentar no eixo y}

\item{a}{TRUE/FALSE, informa se o movimento se dá na referência absoluta ou relativa}

\item{r}{TRUE/FALSE, movimento relativo em relação ao ponto atual?}

\item{@sync}{TRUE/FALSE Se sincronizado, a função só retorna depois de executado o movimento}
}
\description{
Executa os movimentos do robô cartesiano ao longo do eixo y apenas
}
\examples{
moveY(dev, 5, r=TRUE)

}
