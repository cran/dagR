\name{dagR-package}
\Rdversion{1.1}
\alias{dagR-package}
\alias{dagR}
\docType{package}
\title{
R functions for directed acyclic graphs
}
\description{
The package dagR (pronounce "dagger") contains a couple of functions to draw, manipulate and evaluate directed acyclic graphs (DAG), with a focus on epidemiologic applications, namely the assessment of adjustment sets and potentially biasing paths.
 The functions for finding and evaluating paths essentially implement the graphical algorithms outlined in Greenland (1999).\cr\cr
 When using this package for your work, please cite Breitling (2010).\cr\cr
\emph{Note: As spelled out in the license, this suite of functions comes without any warranty, and cautious use is strongly advised.
Although testing was carried out as meticulously as possible, it must be expected that bugs or errors remain, in particular in the early versions of the package.
Please report any problems, concerns, but also suggestions for improvements or extensions to the author.}\cr\cr
Important additions in future versions could be e.g. improved drawing routines with better formatting of alternative node symbols in the DAG (taking into account the string length) and algorithms with intelligent/efficient search for minimal adjustment sets.
}
\details{
\tabular{ll}{
Package: \tab dagR\cr
Type: \tab Package\cr
Version: \tab 1.1.2\cr
Date: \tab 2012-11-04\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
\code{\link{dag.init}} is used for setting up DAGs. See the code of the functions \code{demo.dag0} to \code{demo.dag6} for example code.
 To adjust and/or evalute DAGs for biasing paths, use \code{\link{dag.adjust}}, \code{\link{dag.draw}} for drawing a DAG.
 \code{\link{dag.search}} uses \code{\link{brute.search}} to evaluate all possible adjustment sets, allowing the identification of minimal sufficient adjustment sets using \code{\link{msas}}.
 At present, \code{summary_dagRdag} can summarize a DAG object. This should later become an S3 method.\cr Several helper functions currently are not hidden and should later be made internal.
 \cr \cr \emph{Please see the NEWS file for version changes and known open issues.}
}
\author{
Lutz P Breitling <lutz.breitling@gmail.com>
}
\references{
Breitling LP (2010). dagR: a suite of R functions for directed acyclic graphs. Epidemiology 21(4):586-587.\cr
Greenland S, Pearl J, Robins JM (1999). Causal diagrams for epidemiologic research. Epidemiology 10(1):37-48.
}
\keyword{ package }
\examples{
dag1<-demo.dag1();
dag.draw(dag1);
dag1a<-dag.adjust(dag1, 3);
dag.draw(dag1a);
dag1s<-dag.search(dag1);
summary_dagRdag(dag1);
summary_dagRdag(dag1a);
summary_dagRdag(dag1s);
}
