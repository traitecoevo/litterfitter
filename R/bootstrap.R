#' Create a bootstrap distribution of a particular coefficient from a model fit
#'
#' @usage bootstrap_parameters(x,nboot,upper,lower,...)
#'
#' @param x an object of class "\code{litfit}"
#'
#' @param nboot number of bootstrap replications
#'
#' @param upper optional vector of upper bounds for the bootstrap replicates
#'
#' @param lower optional vector of lower bounds for the bootstrap replicates
#'
#' @param ... passed to \code{\link{optim}}
#'
#' @return returns a \code{litfit_bootstrap object}
#'
#'
#'
#' @examples
#' fit <- fit_litter(time=pineneedles$Year,
#' mass.remaining=pineneedles$Mass.remaining, model='neg.exp', iters=100)
#' boot1 <- bootstrap_parameters(fit, nboot = 500)
#'
#'
#'
#' @export
bootstrap_parameters <-
  function(x,
           nboot = 1000,
           upper = NULL,
           lower = NULL,
           ...) {
    # basic error checking
    if (!is(x, "litfit")) {
      stop(
        "Something went wrong -- litterfitter::bootstrap.parameters
         takes a 'litfit' object"
      )
    }
    
    # extract necessary objects
    
    fit.model <- x$model
    obs.time <- x$time
    obs.mass <- x$mass
    nobs <- length(obs.time)
    fit.params <- x$optimFit$par
    fit.nparams <- x$nparams
    
    output <- matrix(ncol = fit.nparams + 1, nrow = nboot)
    
    for (i in 1:nboot)
    {
      # do bootstrapping
      
      boot.inds <- sample(x = nobs,
                          size = nobs,
                          replace = TRUE)
      boot.time <- obs.time[boot.inds]
      boot.mass <- obs.mass[boot.inds]
      
      ifelse(!is.null(upper),
             upper_bounds <-
               upper,
             upper_bounds <- eval(as.list(formals(fit.model))$upper))
      
      ifelse(!is.null(lower),
             lower_bounds <-
               lower,
             lower_bounds <- eval(as.list(formals(fit.model))$lower))
      
      boot.fit <-
        tryCatch(
          optim(
            fit.params,
            obj_func,
            ind = boot.time,
            dep = boot.mass,
            curve = fit.model,
            method = "L-BFGS-B",
            lower = lower_bounds,
            upper = upper_bounds,
            ...
          ),
          error = function(e)
            NULL
        )
      output[i, 1:fit.nparams] <- boot.fit$par
      output[i, fit.nparams + 1] <-
        steady_state(pars = boot.fit$par, model = fit.model)
      
    }
    class(output) <- "litfit_bootstrap"
    return(output)
  }

#' Create a bootstrap distribution of a particular coefficient from a model fit for elementfit
#'
#' @usage bootstrap_parameters_element(x,nboot,upper,lower,...)
#'
#' @param x an object of class "\code{elementfit}"
#'
#' @param nboot number of bootstrap replications
#'
#' @param upper optional vector of upper bounds for the bootstrap replicates
#'
#' @param lower optional vector of lower bounds for the bootstrap replicates
#'
#' @param ... passed to \code{\link{optim}}
#'
#' @return returns a \code{litfit_bootstrap object}
#'
#'
#'
#' @examples
#' fit <- fit_element(mass.remaining=pineneedles$Mass.remaining,
#' element.remaining=c(1.1,1.2,1.3,1,0.8,0.6),
#' model="import.model",iters=1000)
#' boot1 <- bootstrap_parameters_element(fit, nboot = 500)
#'
#'
#'
#' @export
bootstrap_parameters_element <-
  function(x,
           nboot = 1000,
           upper = NULL,
           lower = NULL,
           ...) {
    # basic error checking
    if (!is(x, "elementfit")) {
      stop(
        "Something went wrong -- litterfitter::bootstrap.parameters.element
         takes a 'elementfit' object"
      )
    }
    
    # extract necessary objects
    
    fit.model <- x$model
    obs.time <- x$element.remaining
    obs.mass <- x$mass
    nobs <- length(obs.time)
    fit.params <- x$optimFit$par
    fit.nparams <- x$nparams
    
    output <- matrix(ncol = fit.nparams, nrow = nboot)
    
    for (i in 1:nboot)
    {
      # do bootstrapping
      
      boot.inds <- sample(x = nobs,
                          size = nobs,
                          replace = TRUE)
      boot.time <- obs.time[boot.inds]
      boot.mass <- obs.mass[boot.inds]
      
      ifelse(!is.null(upper),
             upper_bounds <-
               upper,
             upper_bounds <- eval(as.list(formals(fit.model))$upper))
      
      ifelse(!is.null(lower),
             lower_bounds <-
               lower,
             lower_bounds <- eval(as.list(formals(fit.model))$lower))
      
      boot.fit <-
        tryCatch(
          optim(
            fit.params,
            obj_func,
            ind = boot.time,
            dep = boot.mass,
            curve = fit.model,
            method = "L-BFGS-B",
            lower = lower_bounds,
            upper = upper_bounds,
            ...
          ),
          error = function(e)
            NULL
        )
      output[i, 1:fit.nparams] <- boot.fit$par
      
    }
    class(output) <- "elementfit_bootstrap"
    return(output)
  }


##' Plot a bootstrap distribution of a particular coefficient
##'
##' @title Plot the bootstrap distribution for a parameter from a litfit object
##'
##' @usage \method{plot}{litfit_bootstrap}(x,coef.index,bw,...)
##'
##' @param x litfit object
##'
##' @param coef.index  coefficient number to plot from the \code{litfit} object, see order of coefficients for that particular model.  Default is to plot the first parameter for that model.
##'
##' @param bw bandwidth (or bandwidth algorithm see \code{\link{density}}) for the density plot
##'
##' @param ... additional arguments passed to plot.default
##'
##' @details The grey fill goes from 0.025 quantile to the 0.975 quantile of the distribution.  Red line shows the mean.  Blue line shows the median.
##'
##' @seealso \code{\link{fit_litter}} \code{\link{bootstrap_parameters}} \code{\link{density}}
##'
##' @author James Weedon
##'
##' @examples
##' fit <- fit_litter(time=pineneedles$Year,
##' mass.remaining=pineneedles$Mass.remaining, model='neg.exp', iters=200)
##' boot1 <- bootstrap_parameters(fit, nboot = 500)
##' plot(boot1)
##'
##' @export

plot.litfit_bootstrap <- function(x,
                                  coef.index = 1,
                                  bw = "nrd0",
                                  ...) {
  coef.of.interest <- x[, coef.index]
  dens <- density(coef.of.interest, bw = bw)
  plot(dens, main = "Bootstrap distribution", ...)
  
  qfs <- quantile(coef.of.interest, probs = c(0.025, 0.975))
  x1 <- min(which(dens$x >= qfs[1]))
  x2 <- max(which(dens$x <  qfs[2]))
  with(dens, polygon(
    x = c(x[c(x1, x1:x2, x2)]),
    y = c(0, y[x1:x2], 0),
    col = "gray"
  ))
  abline(v = mean(x[, coef.index]), col = "red", lty = 2)
  abline(v = median(x[, coef.index]), col = "blue", lty = 2)
}
