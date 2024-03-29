\name{dag.letter}
\Rdversion{1.1}
\alias{dag.letter}
\title{
Write a DAG node symbol.
}
\description{
Writes the node symbols, 'X' and 'Y' for exposure and outcome, 'C' and 'U' (with consecutive subscripts) for known and unknown covariables. Since v1.1.2, alt.symb allows the use of custom node symbols.
 Unknownness is identified by either node name 'unknown' or covariable type '2' in the DAG object.
 Note that adjusted nodes are marked by bar and underline; this currently does not apply to those marked as unknown.
}
\usage{
dag.letter(dag, letter, x, y, alt.symb = TRUE)
}
\arguments{
  \item{dag}{
The DAG for which a node is to be written.
}
  \item{letter}{
The node that is of interest.
}
  \item{x}{
X position.
}
  \item{y}{
Y position.
}
  \item{alt.symb}{
Boolean indicating if custom symbols (dag$symbols) should be used if available.
}
}
\author{
Lutz P Breitling <l.breitling@posteo.de>
}


\seealso{
\code{\link{dag.draw}}, \code{\link{dag.legend}},
 \code{\link{write.paths}}
}


