\name{plot.dagRdag}
\alias{plot.dagRdag}
\title{
Function to draw a DAG
}
\description{
Generic function to draw a directed acyclic graph in an object of class dagRdag.
This essentially passes the DAG object to the function \code{dag.draw}, which is
maintained for backwards compatibility.
}
\usage{
\method{plot}{dagRdag}(x, y, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class dagRdag to be passed to \code{dag.draw}.
}
  \item{y}{
Currently not used.
}
  \item{\dots}{
Other arguments to be passed to \code{dag.draw}.
}
}
\details{
For all available arguments, see documentation of \code{dag.draw}.
}
\value{
The DAG object is returned.
}
\references{
Breitling LP (2010). dagR: a suite of R functions for directed acyclic graphs. Epidemiology 21(4):586-587.\cr
Breitling LP, Duan C, Dragomir AD, Luta G (2022). Using dagR to identify minimal sufficient adjustment sets and
to simulate data based on directed acyclic graphs. Int J Epidemiol 50(6):1772-1777.\cr
}
\author{
Lutz P Breitling <l.breitling@posteo.de>
}

\seealso{
\code{\link{dag.draw}}
}

