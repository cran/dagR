\name{viv}
\alias{viv}
\title{
Is a numeric vector in another vector?
}
\description{
Checks if all numeric elements of a vector occur also in another vector. It is internally used by \code{msas} to check if some adjustment set is contained in another one.
}
\usage{
viv(v1, v2)
}
\arguments{
  \item{v1}{
The vector whose occurrence in v2 is to be checked.
}
  \item{v2}{
The vector in which v1 might occur.
}
}
\details{
If a value occurs more than once in v1, it is counted as contained in v2 if it appears there once.
 \cr An empty v1 (consisting only of NA) is considered to be contained in any v2.
}
\value{
TRUE if v1 occurs in v2, FALSE otherwise.
}
\author{
Lutz P Breitling <l.breitling@posteo.de>
}
\seealso{
\code{\link{msas}}
}
