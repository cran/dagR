\name{inAngle}
\Rdversion{1.1}
\alias{inAngle}
\title{
Calculate angle between two arcs.
}
\description{
Another rather superfluous helper function, calculating the radian angle between two radian angles. Internally called by \code{smoothArc}.
}
\usage{
inAngle(a, b)
}
\arguments{
  \item{a}{
Radian angle 1.
}
  \item{b}{
Radian angle 2.
}
}
\value{
Numeric in range from -pi to pi.
}
\author{
Lutz P Breitling <l.breitling@posteo.de>
}


\seealso{
\code{\link{smoothArc}}
}


