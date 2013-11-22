\name{plot.litFit}

\alias{plot.litFit}

\title{Plot decomposition trajectory and fit}

\usage{
  	\method{plot}{litFit}(x,...)
}

\arguments{

  	\item{x}{litFit object}
  	\item{...}{additional arguments passed to plot.default}
}

\value{
  	test
}

\description{
  	Plotting the data for a decomposition trajectory and a line of particular type fit. 
}

\details{
  	The data and the line fit plotted from a litFit object.  Designed to give a quick visual check if model fitting is adequate.
}

\seealso{\code{\link{litterFitter}}}

\author{Will Cornwell <wcornwell@gmail.com>}

\examples{
    fit<-litterFitter(time=c(0,1,2,3,4,5,6),mass.remaining=c(1,0.9,1.01,0.4,0.6,0.2,0.01),"neg.exp",iters=250)
	plot(fit)
}

