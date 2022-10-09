\name{dagR2dagitty}
\alias{dagR2dagitty}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create dagitty code from a dagR DAG
}
\description{
Translates a DAG as defined in a dagRdag object created by dagR into
the dagitty package format. Node labeling follows the rules used for
plotting dagRdag objects, but adjusted nodes are marked by a preceding
underscore instead of under- and over-line.
}
\usage{
dagR2dagitty(x, alt.symb = TRUE, only.code = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The dagR DAG to be translated.
}
  \item{alt.symb}{
Boolean indicating if the alternative node symbols should be used.
}
  \item{only.code}{
If TRUE, a string with R dagitty function call is returned, which should be
checked by the user (and possibly edited as required) before running
it to create an equivalent dagitty DAG. If FALSE and the dagitty
package has been installed and loaded, the dagitty function is called
directly and the resulting dagitty DAG is returned.
}
}
\value{
Either a string containing dagitty syntax to translate the dagR DAG
into dagitty format, or a dagitty object.
}
\references{
Breitling LP (2010). dagR: a suite of R functions for directed acyclic graphs. Epidemiology 21(4):586-587.\cr
Breitling LP, Duan C, Dragomir AD, Luta G (2022). Using dagR to identify minimal sufficient adjustment sets and
to simulate data based on directed acyclic graphs. Int J Epidemiol 50(6):1772-1777.\cr
\url{https://cran.r-project.org/package=dagitty}
}
\author{
Lutz P Breitling <l.breitling@posteo.de>
}

\seealso{
\code{\link{dag.letter2}}
}

