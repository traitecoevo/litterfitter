\name{decompFit}
\alias{decompFit}
\title{fit different models to decomposition data}
\usage{
  decompFit(time, mass.remaining, model=c("neg.exp","wei","discrete.parallel","discrete.series","cont.quality.1","cont.quality.2"),iters=200,...)
}
\arguments{
  \item{time}{time since decomposition began $t_{i}-t_{0}$}
  \item{mass.remaining}{proportional mass loss $m_{i}/m_{0}$}
  \item{model}{ADD LOTS OF INFO HERE}
  \item{iters}{Number of random starts for the fitting.  Use higher numbers for models with larger numbers of parameters and for difficult fits.}
  \item{...}{additional arguments}
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
fit<-decompFit(time=c(0,1,2,3,4,5,6),mass.remaining=c(1,0.9,1.01,0.4,0.6,0.2,0.01),"discrete.parallel",iters=250)
}

