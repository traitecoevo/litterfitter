

litterFitter <- function(time, mass.remaining, model=c("neg.exp","weibull","discrete.parallel","discrete.series","cont.quality.1","cont.quality.2"),iters=200,...){
  if (length(time)!=length(mass.remaining)){
    print("time vector must match mass remaining vector")
    return(NULL)
  }
  if (min(mass.remaining)>1|max(mass.remaining)>2){
    print("check mass remaining vector; must be in proportional mass remaining.")
    return(NULL)
  }
  fit <- multioptimFit(time, mass.remaining, model,iters=iters,...)
  if(is.null(fit)) return(NULL)
  predicted_vals <- do.call(model, c(list(time), as.list(fit$par)))
  LL <- calculateLL(predicted_vals, mass.remaining, sqrt(fit$value/length(time)))
  fit.out<-list(optimFit = fit, logLik=LL, time=time, mass=mass.remaining, predicted=predicted_vals, model=model,nparams = length(fit$par))
  class(fit.out)<-"litFit"
  return(fit.out)
}



