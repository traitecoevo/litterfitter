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
  UseMethod("params", x)
}

fitted.litFit<-function(x,...){
  return(x$predicted)
}
