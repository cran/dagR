\name{demo.dag7}
\Rdversion{1.1}
\alias{demo.dag7}
\title{
Set up demo DAG #7.
}
\description{
Initializes a DAG motivated by the manual for the software DAG v0.11
 (Kn\"uppel 2009). This DAG has been featured in Breitling (2010).
 The DAG is the same as DAG #3, but #7 demonstrates the use of alternative node symbols.
}
\usage{
demo.dag7()
}
\value{
Returns a DAG.
}
\references{
Breitling LP (2010). dagR: a suite of R functions for directed acyclic graphs. Epidemiology 21(4):586-587.\cr
Kn\"uppel S (2009). DAG v0.11 documentation (Oct 21, 2009). \url{http://epi.dife.de/dag/}
}
\author{
Lutz P Breitling <lutz.breitling@gmail.com>
}


\seealso{
\code{demo.dag3}
}
\examples{
dag7<-demo.dag7();
summary_dagRdag(dag7);
dag.draw(dag7, legend=FALSE);
dag.draw(dag7, alt.symb=FALSE);
}
