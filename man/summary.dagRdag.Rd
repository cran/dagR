\name{summary.dagRdag}
\alias{summary.dagRdag}
\title{
Summarize a DAG.
}
\description{
Generic function \code{summary()} for class \code{dagRdag}.
}
\usage{
\method{summary}{dagRdag}(object, ...)
}
\arguments{
  \item{object}{
An object of class \code{dagRdag}.}
  \item{...}{
Currently not used.
}
}
\details{
Summarizes according to what functions have been applied to the DAG. It does not itself call \code{dag.search} and the like. Exception: is calls \code{is.acyclic} (with \code{maxSecs=5}).\cr
This function passes the \code{object} to \code{summary_dagRdag}, which is preserved for backwards compatibility.
}
\references{
Breitling LP (2010). dagR: a suite of R functions for directed acyclic graphs. Epidemiology 21(4):586-587.
 \cr Greenland S, Pearl J, Robins JM (1999). Causal diagrams for epidemiologic research. Epidemiology 10(1):37-48.
 \cr Kn\"uppel S, Stang A (2010). DAG Program: identifying minimal sufficient adjustment sets. Epidemiology 21(1):159.
}
\author{
Lutz P Breitling <l.breitling@posteo.de>
}
