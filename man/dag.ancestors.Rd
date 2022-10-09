\name{dag.ancestors}
\Rdversion{1.1}
\alias{dag.ancestors}
\title{
Identify ancestors of DAG nodes.
}
\description{
This identifies those nodes in a DAG that are ancestors of the nodes specified, i.e. acc. to the model depicted by the DAG they causally precede those nodes.
 Internally called by \code{dag.adjustment} in the context of finding associations introduced by adjustment.
}
\usage{
dag.ancestors(dag, A)
}
\arguments{
  \item{dag}{
The DAG to be evaluated.
}
  \item{A}{
A vector of nodes for which ancestors are to be identified.
}
}
\value{
A vector indicating which nodes are ancestors of those in A.
 Note that A actually is included at the beginning of the vector.
}
\author{
Lutz P Breitling <l.breitling@posteo.de>
}


\seealso{
\code{\link{dag.adjust}}, \code{\link{dag.adjustment}}
}

