

\name{sqbench.tarr1}
\alias{sqbench.tarr1}
\alias{sqbench.tarr2}
\alias{sqbench.tarr3}
\title{Benchmark Problems from Tarr (2012)}
\usage{
sqbench.tarr1(n, df=2)

sqbench.tarr2(n, dist="chi1")

sqbench.tarr3(n, rho=0.5, alpha=0.5)
}
\arguments{
\item{n}{number of observations}
\item{df}{Degrees-of-freedom in chi-square error distribution (tarr1)}
\item{dist}{Covariate distribution, 'chi1', 'chi2' or 'lnorm'.}
\item{rho}{Correlation between covariates (tarr3)}
\item{alpha}{Heteroscedacity parameter (tarr3)} 
}
\description{
The regression problems in Tarr (2012). 
}
\details{


\code{sqbench.tarr1} simulates data from the model 
\deqn{y = 10 + 10 x + e}
where x is N(0,1) and e is t-distributed with degrees-of-freedom \code{df}.


\code{sqbench.tarr2} simulates data from the model 
\deqn{y = 10 + 10 x + e}
where x is chisq(1), chisq(2), or exp(N(0,1)) and e is N(0,1).



\code{sqbench.tarr3} simulates data from the model 
\deqn{y = 10 + 10 x1 + 0 x2 + e(1+alpha x1)}
where (x1,x2) is bivariate normal with correlation coefficient \code{rho}  and e is N(0,1).

}
\value{Returns a data-frame with covariates \code{x} and response \code{y} 

}
\references{

Tarr, Garth (2012) Small sample performance of quantile regression confidence intervals. Journal of Statistical Computation and Simulation 82(1) , pages 81-94. 
}
\keyword{datagen}




