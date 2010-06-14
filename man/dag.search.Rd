\name{dag.search}
\alias{dag.search}
\title{
Evaluate possible adjustment sets of a DAG.
}
\description{
Currently, this simply is a wrapper for \code{brute.search}, which returns the input DAG with the results of \code{brute.search} and a string describing the search setup.
}
\usage{
dag.search(dag, type = "brute", allow.unknown = FALSE, trace = FALSE, stop = 0)
}
\arguments{
  \item{dag}{
DAG to be evaluated.
}
  \item{type}{
Type of search to be performed. Currently, only =brute is possible.
}
  \item{allow.unknown}{
See \code{\link{brute.search}}.
}
  \item{trace}{
See \code{\link{brute.search}}.
}
  \item{stop}{
See \code{\link{brute.search}}.
}
}
\value{
The DAG with components \code{searchType} and \code{searchRes} added.
}
\author{
Lutz P Breitling <lutz.breitling@gmail.com>
}

\seealso{
\code{\link{brute.search}}
}
\examples{
d3<-demo.dag3();
d3s<-dag.search(d3);
d3s;
}
