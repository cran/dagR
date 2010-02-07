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
dag.legend(dag, lx = -0.15, ly = -0.075)
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
}
\details{
}
\value{
}
\references{
}
\author{
Lutz P Breitling <lutz.breitling@gmail.com>
}
\note{
}


\seealso{
\code{\link{dag.draw}}, \code{\link{write.paths}}
}
\examples{
dag.draw(demo.dag1(), legend=FALSE);
dag.legend(demo.dag1(), lx=0.2, ly=0.01);
}
