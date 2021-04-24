\name{dag.legend}
\Rdversion{1.1}
\alias{dag.legend}
\title{
Write the legend in a DAG drawing.
}
\description{
Lists the DAG symbols along with their names/labels below a DAG drawn.
}
\usage{
dag.legend(dag, lx = -0.15, ly = -0.075, alt.symb = TRUE)
}
\arguments{
  \item{dag}{
The DAG for which the legend is needed.
}
  \item{lx}{
X coordinate for repositioning legend.
}
  \item{ly}{
Y coordinate for repositioning legend.
}
  \item{alt.symb}{
Boolean indicating if the alternative node symbols (dag$symbols) should be used. Note that the formatting is not changed, i.e. longer symbols will not be formatted nicely.
}
}
\author{
Lutz P Breitling <l.breitling@posteo.de>
}


\seealso{
\code{\link{dag.draw}}, \code{\link{write.paths}}
}
\examples{
dag.draw(demo.dag1(), legend=FALSE);
dag.legend(demo.dag1(), lx=0.2, ly=0.01);
}
