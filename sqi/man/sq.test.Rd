\name{sq.test}
\alias{sq.test}
\title{Two-sample testing via quantile differences}
\description{Performs quantile based two-sample testing.}
\usage{


\method{sq.test}{default}(x, y = NULL,
            alternative = c("two.sided", "less", "greater"),
            mu = 0, paired = FALSE, exact = NULL, correct = TRUE,
            conf.int = FALSE, conf.level = 0.95,tau=seq(.1,.9,by=.1), \dots)

\method{sq.test}{formula}(formula, data, subset, na.action,tau=seq(.1,.9,by=.1), \dots)


}
\arguments{
\item{formula}{A formula object}
\item{data}{Data frame.}
\item{tau}{Sequence of quantiles}
\item{method}{Name of response variable, alt. its column number in \code{x}}
}

\details{


 
}

\value{ }  

\references{
}

\author{Joe Sexton \email{joesexton0@gmail.com}}

\seealso{ \code{\link{sqr}, \code{\link{summary.sqr}}. }
 \examples{


data(airquality)

airquality=as.data.frame(na.omit(airquality))

sq.test(Ozone~I(Month>6),data=airquality)



}


