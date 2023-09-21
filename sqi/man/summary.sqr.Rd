\name{mqr}
\alias{mqr}
\title{Multi quantile regression}
\description{Fits linear quantile regression model across multiple quantiles.}
\usage{



mqr(formula,
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

\seealso{ \code{\link{predict_hrf}}, \code{\link{partdep_hrf}},
          \code{\link{varimp_hrf}}. }
 \examples{

\dontrun{


}
}


