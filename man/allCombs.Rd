\name{allCombs}
\Rdversion{1.1}
\alias{allCombs}
\title{
Create all combinations of the elements of a vector.
}
\description{
Creates a matrix with all combinations of 1 to all elements of the
 vector provided. Elements to occur in all combinations can be specified.
 This is internally called by \code{brute.search}.
}
\usage{
allCombs(x, force = c(), trace = FALSE)
}
\arguments{
  \item{x}{
A vector of elements of which combinations are to be formed.
}
  \item{force}{
A vector of elements that are supposed to occur in each combination.
}
  \item{trace}{
A boolean indicating if some output should be printed (TRUE) or
 not (FALSE=default).
}
}
\details{
}
\value{
A matrix with one combination per row. For the shorter combinations,
 the columns to the right are filled up with NA.
}
\references{
}
\author{
Lutz P Breitling <lutz.breitling@gmail.com>
}
\note{
}


\seealso{
\code{\link{brute.search}}
}
\examples{
allCombs(1:4, force=3);
}
