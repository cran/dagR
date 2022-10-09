\name{angle}
\Rdversion{1.1}
\alias{angle}
\title{
Calculate radian angle of line between two points.
}
\description{
Calculates the radian angle of the line connecting two points.
 Internally called by \code{smoothArc}.
}
\usage{
angle(A, B)
}
\arguments{
  \item{A}{
Vector of length two indicating the coordinates of the first point.
}
  \item{B}{
Vector of length two indicating the coordinates of the second point.
}
}
\value{
A numeric value [0, 2*pi).
}
\author{
Lutz P Breitling <l.breitling@posteo.de>
}

\seealso{
\code{\link{smoothArc}}, \code{\link{addAngle}}
}


