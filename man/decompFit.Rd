\name{decompFit}
\alias{decompFit}
\title{fit different models to decomposition data}
\usage{
  decompFit(time, mass.remaining, model=c("neg.exp","weibull","discrete.parallel","discrete.series","cont.quality.1","cont.quality.2"),iters=200,...)
}
\arguments{
  \item{time}{time since decomposition began ti-t0}
  \item{mass.remaining}{proportional mass loss mi/m0}
  \item{model}{The five models currently implemented are the negative exponential }
  \item{iters}{Number of random starts for the fitting.  Use higher numbers for models with larger numbers of parameters and for models that are less identifiable.}
  \item{...}{additional arguments}
}
\value{
  \code{decompFit} returns a list with the following elements: 
}
\description{
  Testing
}
\details{
 The model likelihood is maximized using methods available in \code{\link[stats]{optim}}.  Optimization methods to be used within optim can be specified through the control object (i.e., control$method).  
 The default method is L-BFGS-B with bounds specific to each model.  

\itemize{
\item{\bold{neg.exp} The classic negative exponential model--one parameter}
\item{\bold{weibull} The Weibull residence time model--two parameters}
\item{\bold{discrete.parallel} Two pools in parallel with a term for the fraction of initial mass in each pool--three parameter}
\item{\bold{discrete.series} A three parameter model in which there is the possibility of two sequential pools}
}
For a more in depth discussion of the models see Manzoni et al. (2012).  Equations follow Table 1 within Cornwell and Weedon (2013).  

\emph{Warning}: difficulty in finding the optimal solution is determined by an interaction between the nature and complexity of the 
likelihood space (which is data- and model-dependent) as well as the optimization methods.  There is can never be a 
guarantee that the optimal solution is found, but using many random starting points will increase these odds.  There are many models, which are inherently less identifiable and include flat ridges in likelihood space.  
The confidence in the fit should be very low in these cases (see Cornwell and Weedon 2013).   A number of random starting points are used in optimization and are given through the iters.  The function checks whether the the top 10 optimizations 
 have converged on the same likelihood, and if they have not it raises a warning.  


}

\author{
Will Cornwell and James Weedon
}

\references{

Cornwell, William K., and James T. Weedon. 2013.  Decomposition trajectories of diverse litter types: a model selection analysis. Methods in Ecology and Evolution.

Manzoni, S., Pineiro, G., Jackson, R. B., Jobbagy, E. G., Kim, J. H., & Porporato, A. (2012). Analytical models of soil and litter decomposition: Solutions for mass loss and time-dependent decay rates. Soil Biology and Biochemistry, 50, 66-76.

}

\seealso{
\code{\link{steadyState.litFit}}

}

\examples{
fit<-decompFit(time=c(0,1,2,3,4,5,6),mass.remaining=c(1,0.9,1.01,0.4,0.6,0.2,0.01),"weibull",iters=250)
}

