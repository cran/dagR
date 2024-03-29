\name{write.paths}
\Rdversion{1.1}
\alias{write.paths}
\title{
Write the paths into a DAG drawing.
}
\description{
Writes the paths into a DAG drawing, using the symbols ('C', 'U', 'X', 'Y') used in the drawing, indicating directed arcs by '<' and '>', undirected ones by '-'. Since version 1.1.2, alt.symb allow usage of custom node symbols, though multi-character symbols will not be formatted well.
 Adjusted variables are under- and over-lined.
 If the paths have been evaluated using \code{eval.paths}, the status are also written.
}
\usage{
write.paths(dag, px = 0.5, py = -0.06, alt.symb = TRUE)
}
\arguments{
  \item{dag}{
The DAG that has been drawn.
}
  \item{px}{
An x coordinate to change the position of the path writing.
}
  \item{py}{
A y coordinate to change the position of the path writing.
}
  \item{alt.symb}{
Boolean indicating if alternative node symbols (dag$symbols) should be used.
}
}
\author{
Lutz P Breitling <l.breitling@posteo.de>
}


\seealso{
\code{\link{dag.draw}}, \code{\link{find.paths}},
 \code{\link{eval.paths}}, \code{\link{dag.legend}}
}


