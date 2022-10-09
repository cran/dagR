\name{dag.adjust}
\Rdversion{1.1}
\alias{dag.adjust}
\title{
Adjust an existing DAG for covariables.
}
\description{
Looks for associations introduced by adjusting for
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
Vector indicating the adjustment set. \emph{The numbering is according to the nodes vector of the DAG, which is shown e.g. in the legend of a DAG drawn by \code{dag.draw}. This numbering is different (+1) from the one used in \code{dag.init}, because the nodes vector also contains the exposure at position 1 (in contrast to the covariables vector used in \code{dag.init})!}
}
}
\details{
If the adjustment set is empty, the function only looks for
 biasing paths and evalutes these.
}
\value{
A DAG with the adjustment set A, and possibly with additional
 associations introduced by adjustment, biasing paths found,
 and the status of these.\cr
If adjustment set is not empty, \code{searchType} and \code{searchRes} are set to \code{NULL}.
}
\references{
Breitling LP (2010). dagR: a suite of R functions for directed acyclic graphs. Epidemiology 21(4):586-587.\cr
Breitling LP, Duan C, Dragomir AD, Luta G (2022). Using dagR to identify minimal sufficient adjustment sets and
to simulate data based on directed acyclic graphs. Int J Epidemiol 50(6):1772-1777.\cr
Greenland S, Pearl J, Robins JM (1999). Causal diagrams for epidemiologic research. Epidemiology 10(1):37-48.\cr
}
\author{
Lutz P Breitling <l.breitling@posteo.de>
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


