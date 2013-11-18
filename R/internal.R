## internal functions 
## base functions for models in Manzoni 2012 Table A1

discrete.parallel <- function(x,R,K1,K2,upper=c((1-1e-4),1000,1000), lower=c(0.0001,0.0001,0.0001)){
  (((1-R)*K1*exp(-K2*x)) - ((K2-K1*R)*exp(-K1*x)))/(K1-K2)
}

discrete.series <- function(t,A, K1, K2,upper=c((1-1e-4),100,100),lower=c(0.0001,0.0001,0.0001) ){
  A*exp(-K1*t) + (1-A)*exp(-K2*t)
}

weibull<-function(x,beta,alpha, upper=c(10,10),lower=c(0.0001,0.0001)){
  exp(- (x/beta)^alpha)
}

neg.exp<-function(x,k,upper=c(5),lower=c(0.0001)){
  exp(-k*x)
}

cont.quality.1<-function(x,b,a, upper=c(10^10,10^4)){
  (b^a)/(b+x)^a
}

cont.quality.2<-function(x,b,a, upper=c(10^10,100), lower=c(0.0001,1.0001)){
  1/((1+b*x)^a)
}

predict.from.fit <- function(model_fn, params, target_time){
  prediction <- do.call(model_fn, c(list(target_time), as.list(params)))
  return(prediction)
}

# Functions for steady state calculation (according to Reviewer 2)
wei.steadystate <- function(beta, alpha) { beta/alpha * gamma(1/alpha)}
negexp.steadystate <- function(k) { 1/k}
D2.steadystate <- function(R, K1, K2) { (1/K1) + (1-R)/K2 }
D3.steadystate <- function(A, K1, K2) { (A/K1) + (1-A)/K2}
agren.C1.steadystate <- function(b, a) {1/b * 1/(a-1)}


obj_func <- function(x, ind, dep, curve){
  #print(x)
  try(predicted <- do.call(curve,(c(list(ind), as.list(x)))))
  return(sum((predicted - dep)^2))
}


calculateLL <- function(predicted, observed, sigma){
  return(sum(dnorm(observed, mean=predicted, sd=sigma, log=T)))
}


multioptimFit <- function(time_data, mass_data, model, iters=200,...){
  nArgs <- length(formals(model)) - 3 
  # need this to allow variation in number of parameters (changed to 3 because "upper" and "lower" are now  formal arguments)
  fit<-list()
  upper_bounds<-eval(as.list(formals(model))$upper)
  lower_bounds<-eval(as.list(formals(model))$lower)
  for (i in 1:iters){
    starter <- runif(nArgs, min=lower_bounds, max=lower_bounds+0.9998) 
    #always start near lower bound--empirically works better
    fit[[i]]<-tryCatch(optim(starter,obj_func, ind=time_data, dep=mass_data, curve = model,
                             method="L-BFGS-B",lower=lower_bounds,upper=upper_bounds),error=function(e) NULL) 
  }
  successes<-unlist(sapply(fit,function(x) {ifelse(is.null(x), return(FALSE), return(x$convergence==0))}))
  cat(paste("Number of successful fits: ", sum(successes)," out of", iters, "\n"))
  if(sum(successes)==0) {
    print("All attempts failed to converge") 
    return(NULL)
  }
  s.fit<-subset(fit,successes)
  likes<-unlist(sapply(s.fit,function(x)x$value))
  likes[likes==0]<-NA
  if(sd(sort(likes,, decreasing = TRUE)[1:5])>0.01) {
    warning("May not have found global best fit; increase iterations") 
  }
  best.one<-which.min(likes[likes>0])
  #cat(paste("Best one index", best.one, "\n"))
  return(s.fit[[best.one]])
}



