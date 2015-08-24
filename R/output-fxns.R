##' Plot a fit of a curve to a litter decomposition trajectory
##' 
##' @title Plot decomposition trajectory and curve fit
##' 
##' @usage \method{plot}{litfit}(x,...)
##' 
##' @param x litfit object
##' 
##' @param ... additional arguments passed to plot.default
##' 
##' @details The data and the line fit plotted from a litFit object.  Designed to give a quick visual check if model fitting is adequate.
##' 
##' @seealso \code{\link{fit_litter}}
##' 
##' @author Will Cornwell
##' 
##' @examples fit<-fit_litter(time=c(0,1,2,3,4,5,6),mass.remaining=c(1,0.9,1.01,0.4,0.6,0.2,0.01),"neg.exp",iters=250)
##' plot(fit)
##' 
##' @export

plot.litfit<-function(x,...){
  plot(x$mass~x$time,pch=16,xlab="Time",ylab="Propotion mass remaining",xlim=c(0,max(x$time)),main=x$model,...)
  mod<-eval(parse(text=paste("litterfitter:::",x$model,sep="")))
  lines(seq(0,max(x$time),0.01),do.call(mod, c(list(seq(0,max(x$time),0.01)), as.list(x$optimFit$par))))
}


coef.litfit<-function(object, ...){
  #add names of parameters to output
  return(object$optimFit$par)
}


fitted.litfit<-function(object, ...){
  return(object$predicted)
}

summary.litfit <- function(x,...){
  # prototype, to be expanded and improved
  cat("Summary of litFit object\n")
  cat(paste("Model type:", x$model,"\n"))
  cat(paste("Number of observations: ", length(x$time),"\n"))
  cat(paste("AIC: ", round(x$fitAIC,4), "\n"))
  cat(paste("AICc: ", round(x$fitAICc,4), "\n"))
  cat(paste("BIC: ", round(x$fitBIC,4), "\n"))
}


##' Estimate the steady state biomass as a proportion of the annual input,
##' based on the particular model fit.
##' 
##' @title Steady-state estimating from a lit fit object
##' 
##' @usage steady_state(x,...)
##' 
##' @param x litfit object
##' 
##' @param ... additional parameters
##' 
##' @details Right now only implemented for a subset of models.  More coming soon...
##' 
##' @seealso \code{\link{fit_litter}}
##' 
##' @author Will Cornwell
##' 
##' @examples fit<-fit_litter(time=c(0,1,2,3,4,5,6),mass.remaining=c(1,0.9,1.01,0.4,0.6,0.2,0.01),"neg.exp",iters=250)
##' steady_state(fit)
##' 
##' 
##' @export steady_state
steady_state<-function(x,...){
  if (class(fit)!="litfit"){
    message("Something went wrong -- litterfitter::steady_state takes a 'litfit' object")
    return(NULL)
  }
  out<-switch(x$model,
         neg.exp=negexp.steadystate(x$optimFit$par),
         weibull=weibull.steadystate(x$optimFit$par[1],x$optimFit$par[2]),
         discrete.parallel=discrete.parallel.steadystate(x$optimFit$par[1],x$optimFit$par[2],x$optimFit$par[3]),
         discrete.series=discrete.series.steadystate(x$optimFit$par[1],x$optimFit$par[2],x$optimFit$par[3]),
         cont.quality.2=cont.quality.2.steadystate(x$optimFit$par[1],x$optimFit$par[2]),
         cont.quality.1="not yet implemented",
         neg.exp.limit="not yet implemented")
  names(out)<-x$model
  return(out)
}






