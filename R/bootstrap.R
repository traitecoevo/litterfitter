#' Create a bootstrap distribution of a particular coefficient from a model fit
#'
#' @usage bootstrap_parameters(x,nboot,upper,lower,...)
#'
#' @param x an object of class "\code{litfit}" (from \code{\link{fit_litter}}) or
#'   "\code{elementfit}" (from \code{\link{fit_element}})
#' @param nboot number of bootstrap replications
#' @param upper optional vector of upper bounds for the bootstrap replicates
#' @param lower optional vector of lower bounds for the bootstrap replicates
#' @param ... passed to \code{\link{optim}}
#'
#' @return a \code{litfit_bootstrap} object: a matrix with one row per bootstrap
#'   replicate and one column per parameter, plus derived quantities: the implied
#'   steady state for a \code{litfit}, or the peak element remaining and the mass
#'   remaining at the peak for an \code{elementfit}.
#'   Failed replicates are recorded as \code{NA}.
#'
#' @examples
#' fit <- fit_litter(
#'   time = pineneedles$Year,
#'   mass.remaining = pineneedles$Mass.remaining, model = "neg.exp", iters = 100
#' )
#' boot1 <- bootstrap_parameters(fit, nboot = 500)
#'
#' @export
bootstrap_parameters <-
  function(x,
           nboot = 1000,
           upper = NULL,
           lower = NULL,
           ...) {
    if (!inherits(x, c("litfit", "elementfit"))) {
      stop("litterfitter::bootstrap_parameters takes a 'litfit' or 'elementfit' object")
    }

    fit.model <- x$model
    xy <- fit_xy(x)
    obs.time <- xy$x
    obs.mass <- xy$y
    nobs <- length(obs.time)
    derived <- derived_names(x)
    fit.params <- x$optimFit$par
    fit.nparams <- x$nparams

    upper_bounds <- if (!is.null(upper)) upper else model_bounds(fit.model, "upper")
    lower_bounds <- if (!is.null(lower)) lower else model_bounds(fit.model, "lower")

    output <- matrix(NA_real_, ncol = fit.nparams + length(derived), nrow = nboot)
    colnames(output) <- c(model_param_names(fit.model), derived)

    for (i in seq_len(nboot)) {
      boot.inds <- sample(x = nobs, size = nobs, replace = TRUE)

      boot.fit <- tryCatch(
        run_optim(
          fit.params,
          make_objective(fit.model, obs.time[boot.inds], obs.mass[boot.inds]),
          lower_bounds,
          upper_bounds,
          ...
        ),
        error = function(e) NULL
      )
      if (is.null(boot.fit) || boot.fit$value >= obj_penalty) {
        next
      }
      output[i, seq_len(fit.nparams)] <- boot.fit$par
      output[i, fit.nparams + seq_along(derived)] <- derived_values(x, boot.fit$par)
    }

    n_failed <- sum(is.na(output[, 1]))
    if (n_failed > 0) {
      warning(paste(n_failed, "of", nboot, "bootstrap replicates failed and are recorded as NA"))
    }
    class(output) <- "litfit_bootstrap"
    attr(output, "model") <- fit.model
    return(output)
  }


#' Summarise a bootstrap distribution of parameters from a litfit object
#'
#' @param object a \code{litfit_bootstrap} object from \code{\link{bootstrap_parameters}}
#' @param probs quantiles to report (default gives a 95 percent interval and the median)
#' @param ... unused
#'
#' @return a matrix with one row per parameter (plus steady state) giving the mean
#'   and the requested quantiles of the bootstrap distribution, ignoring failed replicates
#'
#' @examples
#' fit <- fit_litter(
#'   time = pineneedles$Year,
#'   mass.remaining = pineneedles$Mass.remaining, model = "neg.exp", iters = 100
#' )
#' boot1 <- bootstrap_parameters(fit, nboot = 200)
#' summary(boot1)
#'
#' @export
summary.litfit_bootstrap <- function(object, probs = c(0.025, 0.5, 0.975), ...) {
  m <- unclass(object)
  attr(m, "model") <- NULL
  t(apply(m, 2, function(col) {
    c(mean = mean(col, na.rm = TRUE), stats::quantile(col, probs = probs, na.rm = TRUE))
  }))
}


##' Plot a bootstrap distribution of a particular coefficient
##'
##' @title Plot the bootstrap distribution for a parameter from a litfit object
##'
##' @usage \method{plot}{litfit_bootstrap}(x,coef.index,bw,...)
##'
##' @param x litfit object
##'
##' @param coef.index  coefficient number to plot from the \code{litfit} object, see order of coefficients for that particular model.  Default is to plot the first parameter for that model
##' @param bw bandwidth (or bandwidth algorithm see \code{\link{density}}) for the density plot
##' @param ... additional arguments passed to plot.default
##'
##' @details The grey fill goes from 0.025 quantile to the 0.975 quantile of the distribution.  Red line shows the mean.  Blue line shows the median.
##' @return plot of litfit_bootstrap object, returns invisibly
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
##' @importFrom graphics polygon
##' @export

plot.litfit_bootstrap <- function(x,
                                  coef.index = 1,
                                  bw = "nrd0",
                                  ...) {
  coef.of.interest <- unclass(x)[, coef.index]
  coef.of.interest <- coef.of.interest[is.finite(coef.of.interest)]
  if (length(coef.of.interest) < 2) {
    stop("Not enough finite bootstrap values to plot this coefficient")
  }
  dens <- density(coef.of.interest, bw = bw)
  plot(dens, main = "Bootstrap distribution", ...)

  qfs <- quantile(coef.of.interest, probs = c(0.025, 0.975))
  x1 <- min(which(dens$x >= qfs[1]))
  x2 <- max(which(dens$x < qfs[2]))
  with(dens, polygon(
    x = c(x[c(x1, x1:x2, x2)]),
    y = c(0, y[x1:x2], 0),
    col = "gray"
  ))
  abline(v = mean(coef.of.interest), col = "red", lty = 2)
  abline(v = median(coef.of.interest), col = "blue", lty = 2)
  invisible(x)
}
