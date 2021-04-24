\name{is.in}
\Rdversion{1.1}
\alias{is.in}
\title{
Check if a specific numeric value occurs in a vector.
}
\description{
Another trivial helper function, called internally by \code{eval.paths}.
 It checks whether the specified (numeric) value is part of a specified vector of (numeric) values.
}
\usage{
is.in(x, c = NULL)
}
\arguments{
  \item{x}{
A numeric value, for which the presence in a vector is to be checked.
}
  \item{c}{
A vector of numeric values.
}
}
\value{
Boolean; TRUE if value is present, FALSE if not.
}
\author{
Lutz P Breitling <l.breitling@posteo.de>
}


\seealso{
\code{\link{eval.paths}}
}
\examples{
is.in(4, 1:3)==TRUE;
is.in(4, 1:5)==TRUE;
}
