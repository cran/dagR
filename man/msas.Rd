\name{msas}
\alias{msas}
\title{
Identify minimal sufficient adjustment sets.
}
\description{
Evaluates DAG adjustment sets identified by a \code{dag.search} (or \code{brute.search}) for minimal sufficiency by counting for each sufficient adjustment set A how many smaller sufficient ones that are contained in A exist.
}
\usage{
msas(adjSets)
}
\arguments{
  \item{adjSets}{
The \code{searchRes} component of a DAG (or the output of \code{brute.search}, which is used by \code{dag.adjust} to produce \code{searchRes}).
}
}
\value{
A vector containing a \code{-1} for each insufficient adjustment set, and for sufficient ones the number of smaller sufficient ones contained in it. 
}
\references{
Breitling LP (2010). dagR: a suite of R functions for directed acyclic graphs. Epidemiology 21(4):586-587.
 \cr Greenland S, Pearl J, Robins JM (1999). Causal diagrams for epidemiologic reserach. Epidemiology 10(1):37-48.
 \cr Kn\"uppel S, Stang A (2010). DAG Program: identifying minimal sufficient adjustment sets. Epidemiology 21(1):159.
}
\author{
Lutz P Breitling <l.breitling@posteo.de>
}

\seealso{
\code{\link{viv}}, \code{\link{summary_dagRdag}}
}
\examples{
d3<-demo.dag3();
d3s<-dag.search(d3);
msas(d3s$searchRes);
bs<-brute.search(d3);
msas(bs);
}
