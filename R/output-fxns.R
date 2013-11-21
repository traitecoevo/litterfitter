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
         cont.quality.1="not implemented")
  names(out)<-x$model
  return(out)
}








