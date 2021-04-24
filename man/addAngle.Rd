\name{addAngle}
\Rdversion{1.1}
\alias{addAngle}
\title{
Sum up two radian angles.
}
\description{
Adds two radian angles together and applies modulus 2*pi.
 This is internally called by smoothArc, though hardly needed.
}
\usage{
addAngle(a, b)
}
\arguments{
  \item{a}{
Angle 1 in radian.
}
  \item{b}{
Angle 2 in radian.
}
}
\value{
numeric value [0, 2*pi).
}
\author{
Lutz P Breitling <l.breitling@posteo.de>
}


\seealso{
\code{\link{smoothArc}}
}
\examples{
addAngle(0.5*pi, pi);
addAngle(1.5*pi, pi);
}
