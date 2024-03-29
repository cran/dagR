\name{dagR-package}
\Rdversion{1.1}
\alias{dagR-package}
\alias{dagR}
\docType{package}
\title{
Directed Acyclic Graphs: Analysis and Data Simulation
}
\description{
The package dagR contains a couple of functions to draw, manipulate and evaluate directed acyclic graphs (DAG), with a focus on epidemiologic applications, namely the assessment of adjustment sets and potentially biasing paths.
 The functions for finding and evaluating paths essentially implement the graphical algorithms outlined in Greenland (1999).\cr\cr
 When using this package for your work, please cite Breitling (2010) and/or Breitling et al. (2022).\cr\cr
 For motivations to use this package in epidemiology teaching and methodological research, please refer
 to Duan et al. (2022).\cr\cr
\emph{Note: As spelled out in the license, this suite of functions comes without any warranty, and cautious use is strongly advised.
Although testing was carried out as meticulously as possible, it must be expected that bugs or errors remain, in particular in the early versions of the package.
Please report any problems, concerns, but also suggestions for improvements or extensions to the author.}\cr\cr
Important additions in future versions could be e.g. improved drawing routines with better formatting of alternative node symbols in the DAG (taking into account the string length) and algorithms with intelligent/efficient search for minimal adjustment sets.
}
\details{
\tabular{ll}{
Package: \tab dagR\cr
Type: \tab Package\cr
Version: \tab 1.2.1\cr
Date: \tab 2022-10-09\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
\code{\link{dag.init}} is used for setting up DAGs. See the code of the functions \code{demo.dag0} to \code{demo.dag6} for example code.
 To adjust and/or evalute DAGs for biasing paths, use \code{\link{dag.adjust}}, \code{\link{dag.draw}} for drawing a DAG.
 \code{\link{dag.search}} uses \code{\link{brute.search}} to evaluate all possible adjustment sets, allowing the identification of minimal sufficient adjustment sets using \code{\link{msas}}.
 \code{\link{dag.sim}} simulates data (normally distributed or binary) according to
 the causal structure given by a DAG object.\cr 
 In version 1.2.0, generic S3 methods (print, plot, summary) for dagR-DAGs were implemented, but the original functions \code{summary_dagRdag} to summarize and \code{dag.draw} to plot a DAG object were preserved for backwards compatibility. Export functions to other packages were added upon a reviewer request.\cr Several helper functions currently are not hidden and should later be made internal.
 \cr \cr \emph{Please see the NEWS file for version changes and known open issues.}
}
\author{
Lutz P Breitling <l.breitling@posteo.de>
}
\references{
Breitling LP (2010). dagR: a suite of R functions for directed acyclic graphs. Epidemiology 21(4):586-587.\cr
Breitling LP, Duan C, Dragomir AD, Luta G (2022). Using dagR to identify minimal sufficient adjustment sets and to simulate data based on directed acyclic graphs. Int J Epidemiol  50(6):1772-1777 <\doi{10.1093/ije/dyab167}>.\cr
Duan C, Dragomir AD, Luta G, Breitling LP (2022). Reflection on modern methods: Understanding bias and data analytical strategies through DAG-based data simulations. Int J Epidemiol 50(6):2091-2097 <\doi{10.1093/ije/dyab096}>.\cr
Greenland S, Pearl J, Robins JM (1999). Causal diagrams for epidemiologic research. Epidemiology 10(1):37-48.\cr
}
\keyword{ package }


