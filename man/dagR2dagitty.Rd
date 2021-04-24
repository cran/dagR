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
https://cran.r-project.org/web/packages/dagitty/index.html
}
\author{
Lutz P Breitling <l.breitling@posteo.de>
}

\seealso{
\code{\link{dag.letter2}}
}
\examples{
ex2<-dag.adjust(demo.dag4(),3);
plot(ex2);
dd2<-dagR2dagitty(ex2);
class(dd2);
dd2;

\dontrun{
dd2<-dagR2dagitty(ex2, only.code=FALSE);
class(dd2);
plot(dd2);
}
}