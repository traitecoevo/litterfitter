\name{plot.litFit}
\alias{plot.litFit}
\title{Plot decomposition trajectory and fit}
\usage{
  plot.litFit(x,...)
  }
\arguments{
  \item{x}{litFit object}
  \item{...}{additional arguments passed to plot.default}
}
\value{
  test
}
\description{
  Testing
}
\details{
  This function is an example of a help file
}
\examples{
fit<-decompFit(time=c(0,1,2,3,4,5,6),mass.remaining=c(1,0.9,1.01,0.4,0.6,0.2,0.01),"neg.exp",iters=250)
plot.litFit(fit)
}

