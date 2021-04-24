\name{is.acyclic}
\Rdversion{1.1}
\alias{is.acyclic}
\title{
Check if a DAG actually is acyclic.
}
\description{
This function checks for each node in a DAG whether backtracing arcs leading to it results in an "infinite recursion" error indicating that there actually is a cyclic part in the DAG (which then obviously seems not to be a DAG).
}
\usage{
is.acyclic(dag, maxSecs=NA)
}
\arguments{
  \item{dag}{
The DAG to be check.
}
  \item{maxSecs}{
maximum time before function aborts;
}
}
\value{
A list with two elements.
 \code{acyclic} is a boolean indicating whether the DAG is acyclic (=TRUE) or contains a cyclic component (=FALSE).
 \code{nodewise} is a vector containing 1 boolean per node in the DAG, TRUE indicating that backtracing from this node does not lead to a cyclic component, FALSE indicating that backtracing from this node leads to a cyclic component.
}
\author{
Lutz P Breitling <l.breitling@posteo.de>
}

\examples{
dag6<-demo.dag6();
is.acyclic(dag6);
dag6c<-add.arc(dag6, c(3,5));
is.acyclic(dag6c);
}
