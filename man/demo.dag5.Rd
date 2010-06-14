\name{demo.dag5}
\Rdversion{1.1}
\alias{demo.dag5}
\title{
Set up demo DAG #5.
}
\description{
Initializes a miscellaneous DAG.
 What happens if you adjust for the outcome's child?
}
\usage{
demo.dag5()
}
\value{
Returns a DAG.
}
\author{
Lutz P Breitling <lutz.breitling@gmail.com>
}


\seealso{
\code{demo.dag0}, \code{demo.dag1}, \code{demo.dag2},
 \code{demo.dag3}, \code{demo.dag4}, \code{demo.dag6}
}
\examples{
demo.dag5();
x5<-dag.adjust(demo.dag5(), A=3);
}
