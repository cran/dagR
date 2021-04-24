\name{smoothArc}
\Rdversion{1.1}
\alias{smoothArc}
\title{
Draw an undirected assocation in a DAG.
}
\description{
This draws a dashed connection between two points, curving it so that it goes through a third point.
 This is internally used by \code{dag.draw} to draw associations.
}
\usage{
smoothArc(A, B, C, res = 20, gap = 0.05, p = FALSE)
}
\arguments{
  \item{A}{
Vector of length 2, providing xy coordinates of first point.
}
  \item{B}{
Vector of length 2, providing xy coordinates of second point.
}
  \item{C}{
Vector of length 2, indicating xy coordinates through which the association should be curved.
}
  \item{res}{
How smooth should the curve be drawn?
}
  \item{gap}{
How far from point A and B should the line end?
}
  \item{p}{
If TRUE, the point through which the curve goes is drawn (this is to allow better moving it with \code{dag.move}).
}
}
\references{
Breitling LP (2010). dagR: a suite of R functions for directed acyclic graphs. Epidemiology 21(4):586-587.
}
\author{
Lutz P Breitling <l.breitling@posteo.de>
}
\note{
In the version 1.0.1 distributed as online supplemental material with Breitling (2010), the function contains arbitrary default values used during development.
}


\seealso{
\code{\link{dag.draw}}, \code{\link{dag.move}}
}
\examples{
dag.draw(demo.dag1());
smoothArc(c(0.5,0.5), c(1,1), c(0.75,0.6), p=TRUE);
smoothArc(c(0.5,0.5), c(1,1), c(0.65,0.6), p=TRUE);
smoothArc(c(0.5,0.5), c(1,1), c(0.55,0.6), p=TRUE);
smoothArc(c(0.5,0.5), c(1,1), c(0.45,0.6), p=TRUE);
}
