

litterFitter <- function(time, mass.remaining, model=c("neg.exp","weibull","discrete.parallel","discrete.series","cont.quality.1","cont.quality.2","neg.exp.limit"),iters=500,...){
  if (length(time)!=length(mass.remaining)){
    stop("Time vector must have the same length and correspond to the mass remaining vector")
  }
  if (min(mass.remaining)>1|max(mass.remaining)>2){
    stop("Check mass remaining vector; must be in proportional mass remaining.")
  }
  fit <- multioptimFit(time, mass.remaining, model,iters=iters,...)
  if(is.null(fit)) return(NULL)
  predicted_vals <- do.call(model, c(list(time), as.list(fit$par)))
  LL <- calculateLL(predicted_vals, mass.remaining, sqrt(fit$value/length(time)))
  nps <- length(fit$par)
  model.AIC <- calculateAIC(LL, nps)
  fit.out<-list(optimFit = fit, logLik=LL, fitAIC=model.AIC, time=time, mass=mass.remaining, predicted=predicted_vals, model=model,nparams = nps)
  class(fit.out)<-"litFit"
  return(fit.out)
}



