\name{dag.init}
\Rdversion{1.1}
\alias{dag.init}
\title{
Set up a new DAG.
}
\description{
Allows setting up a new DAG.
 See the \code{demo.dag0} to \code{demo.dag6} functions for some example specifications.
}
\usage{
dag.init(outcome = NULL, exposure = NULL, covs = c(), arcs = c(),
 assocs = c(), xgap = 0.04, ygap = 0.05, len = 0.1, y.name = NULL,
 x.name = NULL, cov.names = c(), symbols = NULL, ...)
}
\arguments{
  \item{outcome}{
\emph{Currently not used!}
}
  \item{exposure}{
\emph{Currently not used!}
}
  \item{covs}{
Vector including an integer for each covariable to be in the DAG (1 for a "standard" covariable, 2 for an unknown/unmeasured one).
}
  \item{arcs}{
Vector of duplets of integers, in which nodes from which an arc or undirected association is to emenate are followed by those to which it is to point. To refer to the exposure, use 0, to refer to the outcome, use -1, to refer to covariables, use and element of 1:length(covs).
}
  \item{assocs}{
A vector of same length as covs, with 0 indicating directed arcs, 1 indicating undirected associations.
}
  \item{xgap}{
How much x space is to be left between arc ends and nodes when drawing?
}
  \item{ygap}{
How much y space is to be left between arc ends and nodes when drawing?
}
  \item{len}{
Length of arrow whiskers when drawing.
}
  \item{y.name}{
Label of outcome.
}
  \item{x.name}{
Label of exposure.
}
  \item{cov.names}{
Vector of covariable labels.
}
  \item{symbols}{
Vector of alternative node symbols. Longer symbols will not be formatted nicely. Note that the first element refers to the exposure, the following ones to the covariables, the last one to the outcome.
}
  \item{\dots}{
\emph{Currently not used.}
}
}
\value{
A DAG (objects of class \code{dagRdag}). Check out some of the demonstration DAGs for details.
 The DAG is actually a list object, with elements
 \code{cov.types} (the \code{covs} vector, with 0 put in front, and -1 at the end).;
 \code{x} and \code{y} (coordinates for drawing the nodes, initially set up more or less in a half-circle above the x->y arc);
 \code{arc} (the arcs, transformed into a matrix);
 \code{arc.type} (the \code{assocs} vector);
 \code{curve.x} and \code{curve.y} (if associations are featured, these provide the coordinates through which to curve);
 \code{xgap}, \code{ygap}, \code{len} (the respective drawing parameters);
 \code{symbols} (alternative node symbols);
 \code{version} (dagR version).
}
\references{
Breitling LP (2010). dagR: a suite of R functions for directed acyclic graphs. Epidemiology 21(4):586-587.\cr
Greenland S, Pearl J, Robins JM (1999). Causal diagrams for epidemiologic research. Epidemiology 10(1):37-48.
}
\author{
Lutz P Breitling <l.breitling@posteo.de>
}

\note{
CAVE: The numbering of the covariables and arc coordinates is different here than in the functions later used on the DAG (e.g. \code{add.arc}, \code{dag.adjust})! The functions generally work according to the indexing of the R objects that they handle. Whereas for \code{dag.init} the \code{n} covariable nodes are numbered \code{1:n}, the node vector of the resulting DAG will also contain the exposure node at the beginning and the outcome node at the end, i.e. it will go from \code{1:(n+2)} with the covariables at \code{2:n+1}. \code{summary_dagRdag} will show the latter numbering. \emph{Example}: when adjusting for the first covariable, \code{dag.adjust} must be handed the adjustment set \code{A=2}, as the first covariable will occupy the second node (the first node is occupied by the exposure).
}

\seealso{
\code{\link{dag.draw}}
}

\examples{
#dag.init(covs = c(1, 1), arcs = c(0, 2, 1, 2, 1, 0, -1, 2))
}
