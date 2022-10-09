\name{assoc.exists}
\Rdversion{1.1}
\alias{assoc.exists}
\title{
Check if association between two DAG nodes exists.
}
\description{
Checks if an association between two DAG nodes already exists, i.e.
 does not need to be introduced when adjusting for a shared child etc.
 Internally called by \code{\link{dag.adjustment}}.
}
\usage{
assoc.exists(dag, a, b)
}
\arguments{
  \item{dag}{
The DAG to be dealt with.
}
  \item{a}{
First node.
}
  \item{b}{
Second node.
}
}
\value{
A boolean indicating whether or not an association between first node 
 and second node already exists.
}
\author{
Lutz P Breitling <l.breitling@posteo.de>
}



