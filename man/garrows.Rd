\name{garrows}
\Rdversion{1.1}
\alias{garrows}
\title{
Draw a directed arc in a DAG.
}
\description{
Internally called by \code{dag.draw} for drawing directed arcs.
}
\usage{
garrows(x0, y0, x1, y1, xgap, ygap, len = 0.1)
}
\arguments{
  \item{x0}{
X coordinate of origin.
}
  \item{y0}{
Y coordinate of origin.
}
  \item{x1}{
X coordinate of target node.
}
  \item{y1}{
Y coordinate of target node.
}
  \item{xgap}{
Space between node and arc ends on x axis.
}
  \item{ygap}{
Space between node and arc ends on y axis.
}
  \item{len}{
Length of arrow whiskers (default=0.1).
}
}
\author{
Lutz P Breitling <l.breitling@posteo.de>
}


\seealso{
\code{\link{dag.draw}}, \code{\link{smoothArc}}
}


