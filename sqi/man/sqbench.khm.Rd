
\name{sqbench.khm}
\alias{sqbench.khm}
\title{Benchmark Problems from Kocherginsky, He and Mu (2005)}
\usage{
sqbench.khm(n, scenario = 1)

}
\arguments{
\item{n}{number of observations}
\item{scenario}{An integer from 1 to 7 (see below)}

}
\description{
The regression problems in Kocherginsky, He and Mu (2005). 
}
\details{


\code{senario=1} simulates data from the model 
\deqn{y = 1 + x1 +x2 + e}
where \eqn{x1} is \eqn{N(0,1}), \eqn{x2 = x1 + U(0,1) + N(0,1) } and e is \eqn{N(0,1)}.


\code{senario=2}  simulates data from the model 
\deqn{y = 1 + x1 + x2 + x3 + (1+x2) e}
where x1 is N(0,1), x2 is U(0,1), \eqn{x3 = x1 + x2 + N(0,1)}, and e is \eqn{N(0,1)}.



\code{senario=3}  simulates data from the model 
\deqn{y = 1 + x1 + x2 + ... + x7 + e}
where x1 and x2 are \eqn{Bernoulli(.4)}, x3 and x4 are \eqn{exp(N(0,1))}, (x5,x6) are bivariate-normal with mean \eqn{ (2,2) }, unit variance and correlation 0.8, x7 is Chisq(1) is U(0,1),  and e is \eqn{N(0,1)}.




\code{senario=4}  simulates data from the model 
\deqn{y = 1 + x1 + x2 + ... + x7 + e (1 + x3 + x5 + x7)}
and otherwise equal to \code{senario=3}.



\code{senario=5}  simulates data from the model 
\deqn{y = 1 + x1 + x2 + ... + x7 + (e + 0.674)  (x5^2)  }
and otherwise equal to \code{senario=3}.



\code{senario=6}  simulates data from the model 
\deqn{y = 1 + x1 + e  (1.1 + x1)  }
and x1 is U(-1,1) and e is \eqn{N(0,1)}.





\code{senario=7}  simulates data from the model 
\deqn{y = 1 + x1 + x2 + x3 + e  }
and x1 and x2  are \eqn{N(0,1)}, x3 is the absolute value of  t(df=2)  and e is \eqn{N(0,1)}.





}
\value{Returns a data-frame with covariates \code{x} and response \code{y} 

}
\references{

Kocherginsky, He and Mu (2005).  Practical confidence intervals for regression quantiles, Journal
of Computational and Graphical Statistics 14 (2005), pp. 41–55.

}
\keyword{datagen}




