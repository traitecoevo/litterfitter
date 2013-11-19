plot.litFit<-function(x,...){
  plot(x$mass~x$time,pch=16,xlab="Time",ylab="Propotion of mass remaining",xlim=c(0,max(x$time)),...)
  mod<-eval(parse(text=paste("litterFitter:::",x$model,sep="")))
  lines(seq(0,max(x$time),0.01),do.call(mod, c(list(seq(0,max(x$time),0.01)), as.list(x$optimFit$par))))
}