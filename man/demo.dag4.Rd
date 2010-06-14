\name{demo.dag4}
\Rdversion{1.1}
\alias{demo.dag4}
\title{
Set up demo DAG #4.
}
\description{
Initializes a miscellaneous DAG.
 What happens if you adjust for the exposure's child?
}
\usage{
demo.dag4()
}
\value{
Returns a DAG.
}
\author{
Lutz P Breitling <lutz.breitling@gmail.com>
}


\seealso{
\code{demo.dag0}, \code{demo.dag1}, \code{demo.dag2},
 \code{demo.dag3}, \code{demo.dag5}, \code{demo.dag6}
}
\examples{
demo.dag4();
x4<-dag.adjust(demo.dag4(), A=3);
}
