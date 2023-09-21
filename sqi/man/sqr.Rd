\name{sqr}
\alias{sqr}
\title{Simultaneous multi-quantile regression}
\description{Fits linear quantile regression model across multiple quantiles.}
\usage{



sqr(formula,
   data,
   tau=seq(.1,.9,by=.1),
   method="br"
   )
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

\seealso{  }
 \examples{

\dontrun{
library(sqi)

data(airquality)

airquality=as.data.frame(na.omit(airquality))

fit=sqr(Ozone~Solar.R+Wind+Temp+I(factor(Month)),data=airquality)
plot(fit)


par(mfrow = c(2,2)) 
for(k in 1:4) plot(fit,xindx=k)


}
}


