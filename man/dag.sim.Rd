\name{dag.sim}
\Rdversion{1.1}
\alias{dag.sim}
\title{
Simulate data based on a DAG.
}
\description{
Simulates data according to a DAG object. This function may be replaced by dag.sim2 in the future.
}
\usage{
dag.sim(dag, b = rep(0, nrow(dag$arc)), bxy = 0, n, 
            mu = rep(0, length(dag$x)),
        binary = rep(0, length(dag$x)),
         stdev = rep(0, length(dag$x)), naming = 2, seed = NA, verbose = FALSE)
}
\arguments{
  \item{dag}{
The DAG object according to which data is to be simulated.
}
  \item{b}{
Vector of coefficients defining the direct effects of the DAG arcs.
}
  \item{bxy}{
Coefficient defining the direct effect of main exposure X on outcome Y.
}
  \item{n}{
Number of observations to be simulated.
}
  \item{mu}{
Vector of means that are to be simulated for the different DAG nodes.
For binary nodes without an ancestor, the mean is taken as the prevalence to be simulated.
For binary nodes with ancestors, the mean is similarly interpreted (see details in Value section).
}
  \item{binary}{
Vector indicating which nodes are to be continuous (=0) and binary (=1).
}
  \item{stdev}{
Vector of standard deviations for each node.
For nodes without ancestors, continuous data are drawn from a Normal distribution with this standard deviation.
For nodes with ancestors, this is the standard deviation of the residual noise that is added to the calculated observation values.
}
  \item{naming}{
If =2, the alternative DAG node symbols are used for naming the variables in the output dataframe.
Otherwise, the output dataframe variables are named X1...Xn.
}
  \item{seed}{
Seed to initialize the random number generator.
}
  \item{verbose}{
If =TRUE, additional output is given during the simulation, in particular showing the different calculation steps.
}
}

\value{
A dataframe with n (rows) observations featuring simulated data for each node (columns) in the DAG.
Simulation steps:
1. simulate data for nodes i without ancestors, drawing from Normal distribution with mean mu[i] and stdev[i]
 (continuous node), or drawing from Bernoulli events with probability mu[i] (binary node).
2. simulate data for nodes i for which all ancestors already have been simulated by multiplying the ancestor values
 with the corresponding arc coefficients and summing them up, shifting the resulting values to the mean mu[i] specified for the
 currently simulated node (logit-transformed if binary), then adding noise drawn from a Normal distribution with mean 0
 and standard deviation stdev[i], finally using the inverse logit of the resulting values as success probabilities
 for simulating binary data if node is binary.
}

\note{
Undirected arcs are ignored in these simulations!
}

\references{
Breitling LP, Duan C, Dragomir AD, Luta G (2022). Using dagR to identify minimal sufficient adjustment sets and
to simulate data based on directed acyclic graphs. Int J Epidemiol 50(6):1772-1777.\cr
Duan C, Dragomir AD, Luta G, Breitling LP (2022). Reflection on modern methods: Understanding bias and data
analytical strategies through DAG-based data simulations. Int J Epidemiol 50(6):2091-2097.\cr
}

\author{
Lutz P Breitling <l.breitling@posteo.de>
}

\seealso{
\code{\link{dag.sim2}}
}

