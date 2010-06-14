\name{dag.letter}
\Rdversion{1.1}
\alias{dag.letter}
\title{
Write a DAG node symbol.
}
\description{
Writes the node symbols, 'X' and 'Y' for exposure and outcome, 'C' and 'U' (with consecutive subscripts) for known and unknown covariables.
 Unknownness is identified by either node name 'unknown' or covariable type '2' in the DAG object.
}
\usage{
dag.letter(dag, letter, x, y)
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
}
\author{
Lutz P Breitling <lutz.breitling@gmail.com>
}


\seealso{
\code{\link{dag.draw}}, \code{\link{dag.legend}},
 \code{\link{write.paths}}
}
\examples{
dag2a<-dag.adjust(demo.dag2(), 3);
dag.draw(dag2a, paths=FALSE);
dag.letter(dag2a,  1,  0.2, 1);
dag.letter(dag2a,  2, 0.25, 1);
dag.letter(dag2a,  3,  0.3, 1);
dag.letter(dag2a,  4, 0.35, 1);
dag.letter(dag2a, 13,  0.4, 1);
}
