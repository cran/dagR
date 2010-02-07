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
\details{
}
\value{
A boolean indicating whether or not an association between first node 
 and second node already exists.
}
\references{
}
\author{
Lutz P Breitling <lutz.breitling@gmail.com>
}
\note{
}


\seealso{
}
\examples{
dag1a<-dag.adjust(demo.dag1(), A=3);
assoc.exists(dag1a, 2, 4);
assoc.exists(dag1a, 2, 5);
}
