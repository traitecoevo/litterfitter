plot.litFit<-function(x,...){
  plot(x$mass~x$time,pch=16,xlab="Time",ylab="Propotion mass remaining",xlim=c(0,max(x$time)),main=x$model,...)
  mod<-eval(parse(text=paste("litterFitter:::",x$model,sep="")))
  lines(seq(0,max(x$time),0.01),do.call(mod, c(list(seq(0,max(x$time),0.01)), as.list(x$optimFit$par))))
}

params <- function (x, ...) {
  UseMethod("params", x)
}

params.litFit<-function(x,...){
  #add names of parameters to output
  return(x$optimFit$par)
}

fitted <- function (x, ...) {
  UseMethod("fitted", x)
}

fitted.litFit<-function(x,...){
  return(x$predicted)
}

summary.litFit <- function(x,...){
  # prototype, to be expanded and improved
  cat("Summary of litFit object\n")
  cat(paste("Model type:", x$model,"\n"))
  cat(paste("Number of observations: ", length(x$time),"\n"))
  cat(paste("AIC: ", round(x$fitAIC,4), "\n"))
  cat(paste("AICc: ", round(x$fitAICc,4), "\n"))
  cat(paste("BIC: ", round(x$fitBIC,4), "\n"))
}

#WKC: I am a bit confused about whether to do this S3 or not; not a standard R function, but does exist in other packages
steadyState <- function (x, ...) {
  UseMethod("steadyState", x)
}

steadyState.litFit<-function(x,...){
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








