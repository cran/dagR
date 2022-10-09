\name{dag.adjustment}
\Rdversion{1.1}
\alias{dag.adjustment}
\title{
Adjust a DAG for one or more variables.
}
\description{
Identifies the associations introduced by adjustment for the
 variables specified, and returns the DAG with these associations
 added. Note that this is called internally by \code{dag.adjust}, which
 makes sure that biasing paths are looked for and evaluted afterwards.
 Thus, \code{dag.adjustment} should 1.) \emph{not} be called directly, and
 2.) \emph{not} be called on an already adjusted DAG!
}
\usage{
dag.adjustment(dag, A=NULL)
}
\arguments{
  \item{dag}{
The DAG to be adjusted.
}
  \item{A}{
The adjustment set to be applied.
}
}
\details{
The adjustment set A specified when calling \code{dag.adjustment} overrules the
 adjustment variables that are present in the DAG. To keep these in
 the adjustment set, one has to add them to A.
}
\value{
A DAG with A as the adjustment set and the associations introduced by
 adjustment for A added to the DAG.
}
\author{
Lutz P Breitling <l.breitling@posteo.de>
}
\note{
You should \emph{not} use \code{dag.adjustment} on an already adjusted
 DAG, since it cannot identify associations that had been introduced
 by the earlier adjustment. If the new adjustment set does not include
 the adjustment variables present in the first set, the new DAG might
 feature associations that actually only would be introduced when
 adjusting for the variables featured in the first but not second
 adjustment set.
}


\seealso{
\code{\link{dag.adjust}}, \code{\link{find.paths}},
 \code{\link{eval.paths}}
}



