\name{is.unknown}
\Rdversion{1.1}
\alias{is.unknown}
\title{
Check if a DAG node presents an unknown variable.
}
\description{
Another helper function, internally used by \code{brute.search}. It checks whether the node specified is of type=2 or is named 'unknown'.
}
\usage{
is.unknown(x, dag)
}
\arguments{
  \item{x}{
The node of interest.
}
  \item{dag}{
The DAG to be evaluated.
}
}
\value{
TRUE if unknown (acc. to type or name), FALSE otherwise.
}
\author{
Lutz P Breitling <l.breitling@posteo.de>
}


\seealso{
\code{\link{brute.search}}
}
\examples{
dag2<-demo.dag2();
is.unknown(2, dag2);
is.unknown(3, dag2);
dag2u<-dag2; dag2u$names[2]<-"unknown";
is.unknown(2, dag2u);
is.unknown(3, dag2u);
}
