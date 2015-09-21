#' Title
#'
#' @param fit 
#' @param nboot 
#'
#' @return NULL
#' @export
#'
#' @examples NULL
#' 
#' 
#' 
bootstrap.parameters <- function(x, nboot=1000,upper=NULL, lower=NULL){
  # basic error checking
  if (class(x) != "litfit") {
    stop("Something went wrong -- litterfitter::bootstrap.parameters
         takes a 'litfit' object")
  }
  
  # extract necessary objects
  
  fit.model <- x$model
  obs.time <- x$time
  obs.mass <- x$mass
  nobs <- length(obs.time)
  fit.params <- x$optimFit$par
  fit.nparams <- x$nparams
  
  output <- matrix(ncol = fit.nparams + 1, nrow=nboot)
  
  for(i in 1:nboot)
  { # do bootstrapping
    
    boot.inds <- sample(x = nobs, size=nobs, replace=TRUE)
    boot.time <- obs.time[boot.inds]
    boot.mass <- obs.mass[boot.inds]
    
    ifelse(!is.null(upper), upper_bounds <- upper, upper_bounds <- eval(as.list(formals(fit.model))$upper))
    
    ifelse(!is.null(lower), lower_bounds <- lower, lower_bounds <- eval(as.list(formals(fit.model))$lower))
    
    boot.fit <- tryCatch(optim(fit.params, obj_func, ind = boot.time,
                               dep = boot.mass, 
                   curve = fit.model, method = "L-BFGS-B", lower = lower_bounds, upper = upper_bounds), error = function(e) NULL)
    output[i,1:fit.nparams] <- boot.fit$par
    #output[i, fit.nparams+1] <- steady_state(boot.fit)
    ## doesn't work yet due to steady_state() requiring a litfit object
    ## need to either make steady_state() more portable OR
    ## adapt the creation of boot.fit <- (above) to use fit_litter()
    ## i'm inclined towards the former, in the interests of code speed
    
  }
  return(output)
}

