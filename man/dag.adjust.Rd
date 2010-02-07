\name{dag.adjust}
\Rdversion{1.1}
\alias{dag.adjust}
\title{
Adjust an existing DAG for covariables.
}
\description{
The function looks for associations introduced by adjusting for
 the covariables specified, then looks for biasing paths, and
 finally evaluates these paths.
}
\usage{
dag.adjust(dag, A = c())
}
\arguments{
  \item{dag}{
The DAG to be adjusted (or evaluated).
}
  \item{A}{
Vector indicating the adjustment set.
}
}
\details{
If the adjustment set is empty, the function only looks for
 biasing paths and evalutes these.
}
\value{
A DAG with the adjustment set A, and possibly with additional
 associations introduced by adjustment, biasing paths found,
 and the status of these.
}
\references{
}
\author{
Lutz P Breitling <lutz.breitling@gmail.com>
}
\note{
CAVE: Do not apply this to an already adjusted DAG, since
 this might not be handled appropriately (see documentation
 of \code{dag.adjustment} called by \code{dag.adjust}).
}


\seealso{
\code{\link{dag.adjustment}}, \code{\link{find.paths}},
 \code{\link{eval.paths}}
}
\examples{
demo.dag1();
dag.adjust(demo.dag1());
dag.adjust(demo.dag1(), A=3);
}
