\name{dag.letter2}
\alias{dag.letter2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Return a DAG node symbol.
}
\description{
Similar to dag.letter(), but returning a string to label a DAG node.
Adjusted nodes are marked by a preceding underscore.
}
\usage{
dag.letter2(dag, letter, alt.symb)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dag}{
The dagRdag object for which a node symbol is to be returned.
}
  \item{letter}{
The number of the node for which the symbol (often a single letter...) is to be returned.
}
  \item{alt.symb}{
If TRUE, the alternative node symbols of the DAG object will be used.
}
}
\value{
A string containing the DAG letter or alternative symbol.
}
\author{
Lutz P Breitling <l.breitling@posteo.de>
}
\seealso{
\code{\link{dag.letter}}
}