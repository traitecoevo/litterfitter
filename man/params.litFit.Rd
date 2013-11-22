\name{params.litFit}
\alias{params}

\title{Output fitted parameters from a litFit object}

\usage{
  	\method{params}{litFit}(x,...)
}

\arguments{

  	\item{x}{litFit object}
  	\item{...}{additional arguments}
}

\value{
  	test
}

\description{
  	foo
}

\details{
  	woo
}

\seealso{\code{\link{litterFitter}}}

\author{Will Cornwell <wcornwell@gmail.com>}

\examples{
    fit<-litterFitter(time=c(0,1,2,3,4,5,6),mass.remaining=c(1,0.9,1.01,0.4,0.6,0.2,0.01),"neg.exp",iters=250)
	
}

