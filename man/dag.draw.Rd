\name{dag.draw}
\Rdversion{1.1}
\alias{dag.draw}
\title{
Draw a DAG.
}
\description{
Draws a DAG conforming to the dagR format.
 The nodes are represented by 'C' (covariables; numbered with subscripts) and 'U' (unknown/unmeasured covariables; numbered with subscripts), 'X' and 'Y' (exposure and outcome, respectively).
 A legend presents the names of the nodes.
 The X->Y arc is marked with a questionmark as the relationship of interest.
 Adjusted variables are under- and over-lined.
 Undirected associations are drawn with dashed lines.
 If paths have been identified (and evaluated), these (and their status) are written next to the legend.
}
\usage{
dag.draw(dag, legend = TRUE, paths = TRUE, numbering = FALSE,
 p = FALSE, ...)
}
\arguments{
  \item{dag}{
The DAG to be drawn.
}
  \item{legend}{
Boolean indicating whether a node legend should be included.
}
  \item{paths}{
Boolean indicating whether paths (and their status) should be written.
}
  \item{numbering}{
Boolean indicating whether the arcs should be numbered in the DAG.
}
  \item{p}{
Boolean indicating whether the curving points of undirected associations should be drawn.
}
  \item{\dots}{
\emph{Currently not used.}
}
}
\details{
}
\value{
Returns the DAG (for whatever reason...).
}
\references{
Breitling LP (in press). dagR: a suite of R functions for directed acyclic graphs. Epidemiology.\cr
Greenland S, Pearl J, Robins JM (1999). Causal diagrams for epidemiologic research. Epidemiology 10(1):37-48.\cr
}
\author{
Lutz P Breitling <lutz.breitling@gmail.com>
}
\note{
}


\seealso{
\code{\link{dag.letter}}, \code{\link{garrows}},
 \code{\link{smoothArc}}, \code{\link{dag.legend}},
 \code{\link{write.paths}}
}
\examples{
dag.draw(demo.dag1());
dag.draw(dag.adjust(demo.dag1(), 3), numbering=TRUE, p=TRUE);
}
