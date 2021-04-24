\name{find.paths}
\Rdversion{1.1}
\alias{find.paths}
\title{
Find potentially biasing paths in a DAG.
}
\description{
This identifies paths linking exposure and outcome in a DAG.
 Forward paths (including a directed arc emanating from the exposure) are \emph{not} identified.
}
\usage{
find.paths(dag)
}
\arguments{
  \item{dag}{
A DAG for which paths should be found.
}
}
\value{
A DAG with components \code{pathsN} (number of paths identified) and \code{paths} (matrix with each row describing one path by indicating the arcs forming the path; ends with \code{NA} as some other function recognize the end of the path that way) added. 
}
\references{
Breitling LP (2010). dagR: a suite of R functions for directed acyclic graphs. Epidemiology 21(4):586-587.\cr
Greenland S, Pearl J, Robins JM (1999). Causal diagrams for epidemiologic research. Epidemiology 10(1):37-48.
}
\author{
Lutz P Breitling <l.breitling@posteo.de>
}


\seealso{
\code{\link{dag.adjust}}, \code{\link{eval.paths}}
}
\examples{
demo.dag1();
find.paths(demo.dag1());
}
