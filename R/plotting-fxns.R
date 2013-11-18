plot.litFit<-function(x,xlab="Time",ylab="Propotion of mass remaining"){
  plot(x$mass~x$time,pch=16,xlab=xlab,ylab=ylab,xlim=c(0,max(x$time)))
  mod<-get(x$model)
  lines(seq(0,max(x$time),0.01),do.call(mod, c(list(seq(0,max(x$time),0.01)), as.list(x$optimFit$par))))
  #add more from James's code here
}