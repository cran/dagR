\name{rm.node}
\Rdversion{1.1}
\alias{rm.node}
\title{
Remove a node from a DAG.
}
\description{
Function to conveniently remove a node from an existing DAG.
}
\usage{
rm.node(dag, node)
}
\arguments{
  \item{dag}{
The DAG from which to remove the node.
}
  \item{node}{
A single integer, indicating which node is to be removed.
}
}
\details{
}
\value{
A DAG with the node specified removed, along with the corresponding attributes and dependent variables, i.e. arcs involving this node are also removed, and the numbering of the nodes (and their occurrence in arcs) is corrected accordingly.
}
\references{
}
\author{
Lutz P Breitling <lutz.breitling@gmail.com>
}
\note{
}


\seealso{
\code{\link{add.node}}, \code{\link{rm.arc}}, \code{\link{add.arc}}
}
\examples{
dag1a<-dag.adjust(demo.dag1());
dag1armn<-rm.node(dag1a, 3);
}
