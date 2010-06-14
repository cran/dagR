\name{anglePoint}
\Rdversion{1.1}
\alias{anglePoint}
\title{
Calculate coordinates at specific angle and distance.
}
\description{
A function calculating the coordinates of the point that is at
 a specific radian angle in a specific distance from a source point.
 Internally called by \code{smoothArc}.
}
\usage{
anglePoint(A, angl, len)
}
\arguments{
  \item{A}{
Vector of length two with the coordinates of the source point.
}
  \item{angl}{
Radian angle indicating into which direction the new point is to
 be calculated.
}
  \item{len}{
The distance at which the new point is situated from the source point.
}
}
\value{
A vector of length two with the coordinates of the new point.
}
\author{
Lutz P Breitling <lutz.breitling@gmail.com>
}
\note{
Another pretty superfluous helper function...
}


\seealso{
\code{\link{smoothArc}}
}
\examples{
round(anglePoint(c(0,0),      0, 0.5), 2);
round(anglePoint(c(0,0),   pi/2, 0.5), 2);
round(anglePoint(c(0,0),     pi, 0.5), 2);
round(anglePoint(c(0,0), 1.5*pi, 0.5), 2);
}
