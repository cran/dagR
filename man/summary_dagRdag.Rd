\name{summary_dagRdag}
\alias{summary_dagRdag}
\title{
Summarize a DAG.
}
\description{
Generic function \code{summary()} working code for class \code{dagRdag}, which is used by package \code{dagR} from version 1.1.1 on. From version 1.2.0, \code{summary.dagRdag()} is available as a generic function, but summary_dagRdag is preserved for backwards compatibility.
}
\usage{
summary_dagRdag(dag)
}
\arguments{
  \item{dag}{
An object of class \code{dagRdag}.
}
}
\details{
Summarizes according to what functions have been applied to the DAG. It does not itself call \code{dag.search} and the like. Exception: is calls \code{is.acyclic} (with \code{maxSecs=5}).
}
\references{
Breitling LP (2010). dagR: a suite of R functions for directed acyclic graphs. Epidemiology 21(4):586-587.
 \cr Greenland S, Pearl J, Robins JM (1999). Causal diagrams for epidemiologic reserach. Epidemiology 10(1):37-48.
 \cr Kn\"uppel S, Stang A (2010). DAG Program: identifying minimal sufficient adjustment sets. Epidemiology 21(1):159.
}
\author{
Lutz P Breitling <l.breitling@posteo.de>
}


