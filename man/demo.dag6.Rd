\name{demo.dag6}
\Rdversion{1.1}
\alias{demo.dag6}
\title{
Set up demo DAG #6.
}
\description{
Initializes a miscellaneous DAG.
 What happens if you adjust for the collider?
}
\usage{
demo.dag6()
}
\value{
Returns a DAG.
}
\author{
Lutz P Breitling <lutz.breitling@gmail.com>
}


\seealso{
\code{demo.dag0}, \code{demo.dag1}, \code{demo.dag2},
 \code{demo.dag3}, \code{demo.dag4}, \code{demo.dag5}
}
\examples{
demo.dag6();
x6<-dag.adjust(demo.dag6(), A=3);
}
