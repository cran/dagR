\name{add.arc}
\Rdversion{1.1}
\alias{add.arc}
\title{
Add an arc to a DAG.
}
\description{
Conveniently add an arc to an existing DAG.
}
\usage{
add.arc(dag, arc, type = 0)
}
\arguments{
  \item{dag}{
The DAG to which an arc should be added.
}
  \item{arc}{
A vector of length 2, indicating from which node (first element) to
 which node (second element) the arc is to go.
 Note: the node numbering follows the numbering of the existing
 DAG (as shown in \code{dag.draw} with option \code{numbering=T}), not the numbering of \code{dag.init}.
}
  \item{type}{
0 (=default) for a directed arc, 1 for an undirected association.
}
}
\value{
A DAG with the arc (and corresponding \code{arc.type}) added, and with the
 path-related variables (\code{paths}, \code{pathsN}, \code{path.status}, \code{searchType}, \code{searchRes}) removed.
}
\author{
Lutz P Breitling <l.breitling@posteo.de>
}


\seealso{
\code{\link{rm.arc}}, \code{\link{add.node}}, \code{\link{rm.node}}
}


