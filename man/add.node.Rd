\name{add.node}
\Rdversion{1.1}
\alias{add.node}
\title{
Add a node to an existing DAG.
}
\description{
Conveniently adds a node to an existing DAG, inserting its coordinates and
 label before the outcome node. Also updates the arcs correspondingly.
}
\usage{
add.node(dag, name = "unknown", type = 1, x = NA, y = NA)
}
\arguments{
  \item{dag}{
The DAG to which the node is to be added.
}
  \item{name}{
Label for the node (defaults to "unknown").
}
  \item{type}{
Type of node (1=covariable, 2=unknown); defaults to 1.
}
  \item{x}{
X coordinate for the node position.
}
  \item{y}{
Y coordinate for the node position.
}
}
\details{
If no x and y coordinates are provided, the function places the node
 in an arbitrary position, slightly different with each additional
 node, so that one can more easily replace the nodes afterwards
 using \code{dag.move}.
}
\value{
A DAG with the new node added.
}
\references{
}
\author{
Lutz P Breitling <lutz.breitling@gmail.com>
}
\note{
}


\seealso{
\code{\link{rm.node}}, \code{\link{add.arc}}, \code{\link{rm.arc}}
}
\examples{
dag1<-demo.dag1();
dag1a<-add.node(dag1);
}
