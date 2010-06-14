\name{summary_dagRdag}
\alias{summary_dagRdag}
\title{
Summarize a DAG.
}
\description{
Generic function \code{summary} code for class \code{dagRdag}, which is used by package \code{dagR} from version 1.1.1 on. \emph{At present, it actually is NOT coded as a generic function!}
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
Lutz P Breitling <lutz.breitling@gmail.com>
}

\examples{
d3<-demo.dag3();
d3s<-dag.search(d3);
d3a<-dag.adjust(d3, 2);
d3as<-dag.search(d3a);
summary_dagRdag(d3);
summary_dagRdag(d3s);
summary_dagRdag(d3a);
summary_dagRdag(d3as);
}
