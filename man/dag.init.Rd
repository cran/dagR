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
 x.name = NULL, cov.names = c(), ...)
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
  \item{\dots}{
\emph{Currently not used.}
}
}
\details{
}
\value{
A DAG. Check out some of the demonstration DAGs for details.
 The DAG is actually a list object, with elements
 \code{cov.types} (the \code{covs} vector, with 0 put in front, and -1 at the end).;
 \code{x} and \code{y} (coordinates for drawing the nodes, initially set up more or less in a half-circle above the x->y arc);
 \code{arc} (the arcs, transformed into a matrix);
 \code{arc.type} (the \code{assocs} vector);
 \code{curve.x} and \code{curve.y} (if associations are featured, these provide the coordinates through which to curve);
 \code{xgap}, \code{ygap}, \code{len} (the respective drawing parameters). 
}
\references{
Breitling LP (in press). dagR: a suite of R functions for directed acyclic graphs. Epidemiology.\cr
Greenland S, Pearl J, Robins JM (1999). Causal diagrams for epidemiologic research. Epidemiology 10(1):37-48.
}
\author{
Lutz P Breitling <lutz.breitling@gmail.com>
}
\note{
}


\seealso{
\code{\link{dag.draw}}
}
\examples{
demo.dag1;
demo.dag1();
demo.dag2;
demo.dag2();
}
