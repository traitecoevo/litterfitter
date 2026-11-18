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
    check_weibull_scale_bound(fit.model, lower_bounds)

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
          ...,
          default_gr = make_gradient(fit.model, obs.time[boot.inds], obs.mass[boot.inds])
        ),
        error = function(e) NULL
      )
      if (is.null(boot.fit) || boot.fit$convergence != 0 || boot.fit$value >= obj_penalty) {
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
