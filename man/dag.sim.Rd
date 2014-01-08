\name{dag.sim}
\Rdversion{1.1}
\alias{dag.sim}
\title{
Simulate data based on a DAG.
}
\description{
This function simulates data according to a DAG object.
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
Breitling LP (submitted). Understanding confounding and data analytical strategies through DAG-based data simulations.
}
\author{
Lutz P Breitling <lutz.breitling@gmail.com>
}


\examples{
dag1<-dag.init(covs=c(1), arcs=c(1,0, 1,-1), symbols=c("x","c","y"));
dag.draw(dag1, numbering=TRUE);
# manual recalculation
sim1<-dag.sim(dag1, b=c(0.5, 0.3), stdev=c(1,1,1), seed=1, n=3, verbose=TRUE);
set.seed(1);
cc<-rnorm(3);cc;
noise1<-rnorm(3);noise1;
xx<-(0.5*cc-mean(0.5*cc))+noise1;xx;
noise2<-rnorm(3);noise2;
yy<-(0.3*cc-mean(0.3*cc))+noise2;yy;
# larger dataset
sim1<-dag.sim(dag1, b=c(0.4, 0.2), stdev=c(0,1,0), seed=1, n=10000); 
sapply(sim1, mean);sapply(sim1, sd);
coef(lm(x~c, data=sim1));
coef(lm(y~c, data=sim1));
coef(lm(y~x, data=sim1));
sim1<-dag.sim(dag1, b=c(0.3, 0.6), stdev=c(0,1,0), seed=1, n=10000); 
sapply(sim1, mean);sapply(sim1, sd);
coef(lm(x~c, data=sim1));
coef(lm(y~c, data=sim1));
coef(lm(y~x, data=sim1));

# larger DAG and a binary node;
dag4<-demo.dag4();
dag4$symbols<-c("x","c1","c2","c3","y")
dag.draw(dag4, numbering=TRUE);
sim4<-dag.sim(dag4, b=c(1, 2, 3, log(4), 5), mu=c(10, 20, 30, 0.4, 50), stdev=c(1,2,3,0,5),
                     binary=c(0,0,0,1,0), seed=4, n=1000);
summary(sim4);
coef(lm(c1~x, data=sim4));
coef(lm(c2~x, data=sim4));
coef(lm(c2~c1, data=sim4));
exp(coef(glm(c3~c1, data=sim4, family=binomial(link=logit))));
coef(lm(y~c3, data=sim4));
summary(lm(y~x, data=sim4));
# manual recheck
logit<-function(p) log(p/(1-p));            # helper function;
inv.logit<-function(l) exp(l)/(1+exp(l));   # helper function;
sim4<-dag.sim(dag4, b=c(1, 2, 3, log(4), 5), mu=c(10, 20, 30, 0.4, 50), stdev=c(1,2,3,0,5),
                     binary=c(0,0,0,1,0), seed=4, n=5, verbose=TRUE);
set.seed(4);
xx<-rnorm(m=10, sd=1, n=5); xx;
c1.noise<-rnorm(sd=2, n=5); c1.noise;
c1<- 1*xx - mean(1*xx) + 20 + c1.noise; c1;
c2.noise<-rnorm(sd=3, n=5); c2.noise;
c2<- 2*xx + 3*c1 - mean(2*xx+3*c1) + 30 + c2.noise; c2;
c3.raw<- log(4)*c1 - mean(log(4)*c1) + logit(0.4); c3.raw;
noise.dummy<-rnorm(m=0, sd=0, n=5);
c3<-sapply(c3.raw, FUN=function(x) rbinom(n=1,size=1,p=inv.logit(x))); c3;
yy.noise<-rnorm(sd=5, n=5); yy.noise;
yy<- 0*xx + 5*c3 - mean(5*c3) + 50 + yy.noise; yy;
sim4;
data.frame(xx,c1,c2,c3,yy);
}
