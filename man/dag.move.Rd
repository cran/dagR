\name{dag.move}
\Rdversion{1.1}
\alias{dag.move}
\title{
Interactively move a node or curving point in a DAG.
}
\description{
This allows to reposition a node or association curving point of a DAG graphically.
 First, select a node or curving point by left-clicking close to it.
 Then reposition it to any other position by left-clicking.
 Once you are happy with the new position, right-click to exit.
}
\usage{
dag.move(dag)
}
\arguments{
  \item{dag}{
The DAG to be modified.
}
}
\details{
}
\value{
The same DAG, but with the feature repositioned.
}
\references{
}
\author{
Lutz P Breitling <lutz.breitling@gmail.com>
}
\note{
}


\seealso{
}
\examples{
\dontrun{dag1m<-dag.move(dag.adjust(demo.dag1(), 3));}
}
