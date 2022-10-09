\name{dag.sim2}
\Rdversion{1.1}
\alias{dag.sim2}
\title{
Simulate data based on a DAG.
}
\description{
Simulates data according to a DAG object.
}
\usage{
dag.sim2(dag, b = rep(0, nrow(dag$arc)), bxy = 0, n, 
                      distr  = rep(0, length(dag$x)),
                      mu     = rep(0, length(dag$x)),
                      stdev  = rep(0, length(dag$x)),
                      nu     = NA,
                      lambda = NA,
                      binary = NA,
                      naming = 2, seed = NA, verbose = FALSE) 
}


\arguments{
  \item{dag}{
The DAG object according to which data is to be simulated.
}
  \item{b}{
Vector of coefficients defining the direct effects of the DAG arcs (on linear scale).
}
  \item{bxy}{
Coefficient defining the direct effect of main exposure X on outcome Y (on linear scale).
}
  \item{n}{
Number of observations to be simulated.
}
  \item{distr}{
0 for Normal distribution continuous nodes,\cr
1 for binary nodes simulated from logistic model,\cr
1.1 for binary nodes simulated from logistic model (see mu),\cr
2 for binary nodes simulated from linear risk difference model,\cr
2.1 for binary nodes simulated from linear risk difference model (see mu)
}
  \item{mu}{
Vector of means that are to be simulated for the different DAG nodes:\cr
For normally distributed continuous variables, the overall mean simulated.\cr
For binary variables w/ distr=1 or distr=2, overall proportion of successes simulated.\cr
For binary variables w/ distr=1.1 or distr=2.1, proportion of successes simulated in the reference category (sum of coef-weighted predictors =0).
}
  \item{stdev}{
Vector of standard deviations for each node.\cr
For nodes without ancestors, continuous data are drawn from a Normal distribution with this standard deviation.
For continuous nodes with ancestors, this is the standard deviation of the residual noise that is added to the calculated observation value.\cr
If used on binary variables with ancestors, this would analogously add residual noise to the calculated predictor, diluting the direct effects.
}
  \item{nu}{
Not used.
}
  \item{lambda}{
Not used.
}
  \item{binary}{
For backwards compatibility: Vector indicating which nodes are to be continuous (=0) and binary (=1). If given, this is passed to argument "distr" and a warning is issued.
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
A dataframe with n (rows) observations featuring simulated data for each node (columns) in the DAG.\cr\cr
Simulation steps:\cr
1. simulate data for nodes i without ancestors, drawing from Normal distribution with mean mu[i] and stdev[i]
 (continuous node), or drawing from Bernoulli events with probability mu[i] (binary node).\cr
2. simulate data for nodes i for which all ancestors already have been simulated by multiplying the ancestor values
 with the corresponding arc coefficients and summing them up, shifting the resulting values to the mean mu[i] (exceptions: distr=1.1 or
 distr=2.1, as detailed in "mu" above) specified for the
 currently simulated node (logit-transformed if binary based on logistic model), then adding noise drawn from a Normal distribution with mean 0
 and standard deviation stdev[i], finally using the resulting values (inverse logit, if binary based on logistic model) as success probabilities
 for simulating binary data if node is binary.\cr\cr
As the noise is added after shifting to the mean, the mean of the simulated data will not be exact. Also, the noise is added before calculating descendant nodes, i.e. it is sort of true inter-individual variation, rather than measurement error.\cr\cr
For the risk difference model, the success probability calculated by summing the weighted ancestors can easily be <0 (or >1).
If this happens, the probability is set to 0 (or 1), and a warning is issued.
}

\note{
Undirected arcs are ignored in these simulations.
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
\code{\link{dag.sim}}
}
