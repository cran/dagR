\name{brute.search}
\Rdversion{1.1}
\alias{brute.search}
\title{
Evaluate all possible adjustment sets of a DAG.
}
\description{
Evaluates all adjustment sets of a DAG, optionally including
 adjustment sets including "unknown" nodes. If the DAG has
 a non-empty adjustment set, only adjustment sets including
 these adjustment variables are evaluated.
}
\usage{
brute.search(dag, allow.unknown = FALSE, trace = TRUE, stop = 0)
}
\arguments{
  \item{dag}{
The DAG to be evaluated.
}
  \item{allow.unknown}{
Boolean indicating "unknown" nodes should be featured in the adjustment
 sets to be evaluated (TRUE) or not (FALSE=default).
}
  \item{trace}{
Boolean indicating if some output should be produced (TRUE=default).
}
  \item{stop}{
If =0, all eligible adjustment sets are evaluated. If =1, evaluations are
 stopped after the first sufficient adjustment set has been evaluated.
 Defaults to 0.
}
}
\value{
A dataframe with the first columns (\code{X1..Xn}) indicating the variables in the respective adjustment set evaluated.
 The column \code{totalPaths} indicates the number of paths found when adjusting for the respective set,
 and \code{openPaths} indicates the number of biasing paths.
}
\author{
Lutz P Breitling <lutz.breitling@gmail.com>
}
\note{
The output produced by \code{brute.search} allows to manually identify
 sufficient and minimal sufficient adjustment sets, which in the future should
 preferably be done by a helper summary function.
 The evaluation of a complicated DAG like \code{demo.dag2} can take quite
 some time, and future functions should either employ more intelligent
 algorithms to search specifically for sufficient sets, or they should
 allow e.g. the evaluation of adjustment sets of specific sizes.
}


\examples{
bs<-brute.search(demo.dag1(), trace=FALSE);
bs;
}
