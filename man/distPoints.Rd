\name{distPoints}
\Rdversion{1.1}
\alias{distPoints}
\title{
Calculate distance between two points.
}
\description{
Another rather superfluous helper function, internally used by \code{smoothArc}. Calculates the distance between two points.
}
\usage{
distPoints(A, B)
}
\arguments{
  \item{A}{
Vector of length two, indicating x and y of first point.
}
  \item{B}{
Vector of length two, indicating x and y of second point.
}
}
\details{
}
\value{
Distance between the two points.
}
\references{
}
\author{
Lutz P Breitling <lutz.breitling@gmail.com>
}
\note{
}


\seealso{
}
\examples{
dp<-distPoints(c(0,0), c(1,1));
dp;
dp^2;
}
