\name{eval.paths}
\Rdversion{1.1}
\alias{eval.paths}
\title{
Evaluate potentially biasing paths in a DAG.
}
\description{
This essentially implements the graphical algorithm described in Greenland (1999) to identify open "backdoor" (or not strictly backdoor, but potentially biasing) paths in a DAG.
 Paths are identified as being 'open', 'blocked by collider', or 'blocked by adjustment'. If both latter conditions apply, 'blocked by collider' is returned.
}
\usage{
eval.paths(dag)
}
\arguments{
  \item{dag}{
A DAG to which \code{find.paths} has already been applied\cr
 (e.g. within \code{dag.adjust}).
}
}
\details{
This function identifies a collider-blocked path as 'blocked by collider' even if it has been unblocked by adjusting for the collider.
 One could argue that this should not be the case.
 However, the biasing seems to be sufficiently represented in the DAG by the introduction of the association "jumping" the collider and potentially opening biasing paths.
}
\value{
A DAG with component \code{path.status} added.
}
\references{
Breitling LP (2010). dagR: a suite of R functions for directed acyclic graphs. Epidemiology 21(4):586-587.\cr
Greenland S, Pearl J, Robins JM (1999). Causal diagrams for epidemiologic research. Epidemiology 10(1):37-48.
}
\author{
Lutz P Breitling <l.breitling@posteo.de>
}


\seealso{
\code{\link{dag.adjust}}, \code{\link{find.paths}}
}
\examples{
dag1<-demo.dag1();
dag1a<-dag.adjustment(dag1, A=3); # normally called via dag.adjust();
dag1f<-find.paths(dag1a);
dag1e<-eval.paths(dag1f);
names(dag1);
names(dag1a);
names(dag1f);
names(dag1e);
}
