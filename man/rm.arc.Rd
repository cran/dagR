\name{rm.arc}
\Rdversion{1.1}
\alias{rm.arc}
\title{
Remove an arc from a DAG.
}
\description{
Function to conveniently remove an arc from an existing DAG.
}
\usage{
rm.arc(dag, arc)
}
\arguments{
  \item{dag}{
The DAG from which to remove the arc.
}
  \item{arc}{
A single integer, indicating which arc is to be removed (refering to the respective row of the \code{dag$arc} matrix).
}
}
\value{
A DAG with the arc specified removed along with the corresponding attributes like arc types, curves, and path evaluation variables.
}
\author{
Lutz P Breitling <l.breitling@posteo.de>
}
\note{
The numbering of the arcs can be visualized by applying \code{dag.draw} with the option "numbering=TRUE".
}


\seealso{
\code{\link{add.arc}}, \code{\link{add.node}}, \code{\link{rm.node}}
}
\examples{
dag1<-demo.dag1();
dag1$arc;
dag1rma<-rm.arc(dag1, 2);
}
