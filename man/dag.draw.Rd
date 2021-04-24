\name{dag.draw}
\Rdversion{1.1}
\alias{dag.draw}
\title{
Draw a DAG.
}
\description{
Draws a DAG defined in an object of class dagRdag (as of dagR version 1.2.0, the generic function plot.dagRdag can be used for this purpose, but dag.draw is maintained for backwards compatibility).
 The nodes are represented by 'C' (covariables; numbered with subscripts) and 'U' (unknown/unmeasured covariables; numbered with subscripts), 'X' and 'Y' (exposure and outcome, respectively).
 A legend presents the names of the nodes.
 The X->Y arc is marked with a questionmark as the relationship of interest.
 Adjusted variables are under- and over-lined.
 Undirected associations are drawn with dashed lines.
 If paths have been identified (and evaluated), these (and their status) are written next to the legend.
}
\usage{
dag.draw(dag, legend = TRUE, paths = TRUE, numbering = FALSE,
 p = FALSE, alt.symb = TRUE, noxy = 0, ...)
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
  \item{alt.symb}{
Boolean indicating if the alternative node symbols (dag$symbols) should be used. Note that especially the legends and paths will not be formatted nicely if these symbols are longer strings.
}
  \item{noxy}{
Integer to indicate if the X->Y should not be drawn (0=default; 1=no arc; 2=arc, but no question mark).
}
  \item{\dots}{
\emph{Currently not used.}
}
}
\value{
Returns the DAG (for whatever reason...).
}
\references{
Breitling LP (2010). dagR: a suite of R functions for directed acyclic graphs. Epidemiology 21(4):586-587.\cr
Greenland S, Pearl J, Robins JM (1999). Causal diagrams for epidemiologic research. Epidemiology 10(1):37-48.\cr
}
\author{
Lutz P Breitling <l.breitling@posteo.de>
}


\seealso{
\code{\link{dag.letter}}, \code{\link{garrows}},
 \code{\link{smoothArc}}, \code{\link{dag.legend}},
 \code{\link{write.paths}}
}
\examples{
plot(demo.dag1());
plot(dag.adjust(demo.dag1(), 3), numbering=TRUE, p=TRUE);
}
