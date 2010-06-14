\name{write.paths}
\Rdversion{1.1}
\alias{write.paths}
\title{
Write the paths into a DAG drawing.
}
\description{
This function writes the paths into a DAG drawing, using the symbols ('C', 'U', 'X', 'Y') used in the drawing, indicating directed arcs by '<' and '>', undirected ones by '-'.
 Adjusted variables are under- and over-lined.
 If the paths have been evaluated using \code{eval.paths}, the status are also written.
}
\usage{
write.paths(dag, px = 0.5, py = -0.06)
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
}
\author{
Lutz P Breitling <lutz.breitling@gmail.com>
}


\seealso{
\code{\link{dag.draw}}, \code{\link{find.paths}},
 \code{\link{eval.paths}}, \code{\link{dag.legend}}
}
\examples{
dag1<-demo.dag1();
dag.draw(dag1);
dag1a<-dag.adjust(dag1);
write.paths(dag1a);
}
