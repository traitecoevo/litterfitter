## internal functions see base functions for models in Manzoni 2012 Table A1


discrete.series <- function(x, R, K1, K2, upper = c((1 - 1e-04), 1000, 1000), lower = c(
                              1e-04,
                              1e-04, 1e-04
                            )) {
  (((1 - R) * K1 * exp(-K2 * x)) - ((K2 - K1 * R) * exp(-K1 * x))) / (K1 - K2)
}

discrete.parallel <- function(t, A, K1, K2, upper = c((1 - 1e-04), 100, 100), lower = c(
                                1e-04,
                                1e-04, 1e-04
                              )) {
  A * exp(-K1 * t) + (1 - A) * exp(-K2 * t)
}


weibull <- function(x, beta, alpha, upper = c(10, 10), lower = c(1e-04, 1e-04)) {
  exp(-(x / beta)^alpha)
}

neg.exp <- function(x, k, upper = c(5), lower = c(1e-04)) {
  exp(-k * x)
}

# function from greg freschet for testing purposes
neg.exp.limit <- function(x, k, A, B, upper = c(5, 100, 1), lower = c(
                            1e-06, 1e-06,
                            1e-06
                          )) {
  A * exp(-k * x) + B
}


cont.quality <- function(x, b, a, upper = c(10^10, 100), lower = c(1e-04, 1.0001)) {
  1 / ((1 + b * x)^a)
}

#' Predict Using a Specific Model Function
#'
#' This function computes predictions by applying the specified model function (`model_fn`)
#' with provided parameters (`params`) at a target time (`target_time`). It is an internal
#' utility function and is NOT a generic or S3 method.
#'
#' @keywords internal
#' @param model_fn A function representing the model used for prediction.
#' @param params A list of parameters to be passed to the model function.
#' @param target_time A numeric value representing the target time for the prediction.
#' @return Numeric. The predicted value computed by the model function.
predict_from_fit <- function(model_fn, params, target_time) {
  prediction <- do.call(model_fn, c(list(target_time), as.list(params)))
  return(prediction)
}

# Functions for steady state calculation (according to Reviewer 2)
weibull.steadystate <- function(beta, alpha) {
  beta / alpha * gamma(1 / alpha)
}

negexp.steadystate <- function(k) {
  1 / k
}

discrete.series.steadystate <- function(R, K1, K2) {
  (1 / K1) + (1 - R) / K2
}

discrete.parallel.steadystate <- function(A, K1, K2) {
  (A / K1) + (1 - A) / K2
}

cont.quality.steadystate <- function(b, a) {
  1 / b * 1 / (a - 1)
}

# A fraction B never decomposes, so the steady state (the integral of mass
# remaining over time) is A / k + B * Inf. B's lower bound is just above zero,
# so a B below `tol` is treated as zero and the steady state is A / k.
neg.exp.limit.steadystate <- function(k, A, B, tol = 1e-4) {
  if (B > tol) Inf else A / k
}

# Warn (with classed conditions) about proportional mass remaining values
# outside 0-1, which are kept in the fit and treated as measurement error.
check_mass_range <- function(mass) {
  n <- length(mass)
  above <- mass > 1
  if (any(above)) {
    msg <- sprintf(
      paste0(
        "%d of %d mass remaining values are above 1 (max %s), i.e. apparent mass gain. ",
        "They are kept in the fit and treated as measurement error."
      ),
      sum(above), n, format(signif(max(mass), 3))
    )
    if (min(mass) > 0.8) {
      msg <- paste0(
        msg, sprintf(
          paste0(
            " The data also show little mass loss (minimum %s), so parameters may be ",
            "poorly constrained and time to mass loss thresholds and steady state ",
            "will be extrapolations."
          ),
          format(signif(min(mass), 3))
        )
      )
    }
    warning(warningCondition(msg, class = "litterfitter_mass_above_one"))
  }
  below <- mass < 0
  if (any(below)) {
    warning(warningCondition(
      sprintf(
        paste0(
          "%d of %d mass remaining values are below 0 (min %s). ",
          "They are kept in the fit and treated as measurement error."
        ),
        sum(below), n, format(signif(min(mass), 3))
      ),
      class = "litterfitter_mass_below_zero"
    ))
  }
  invisible(NULL)
}


# penalty returned for parameter values where the model cannot be evaluated;
# large but finite so finite-difference gradients stay finite
obj_penalty <- 1e10

obj_func <- function(x, ind, dep, curve) {
  make_objective(curve, ind, dep)(x)
}

# Build the objective (sum of squared errors) once per fit. Calling the model
# function directly, rather than via do.call()/as.list()/tryCatch() on every
# evaluation, removes most of the per-evaluation overhead inside optim().
make_objective <- function(curve, ind, dep) {
  f <- if (is.character(curve)) get(curve, envir = topenv(), mode = "function") else curve
  sse <- function(pred) {
    s <- sum((pred - dep)^2)
    # L-BFGS-B cannot handle non-finite values; return a large penalty so
    # the optimiser moves away from problematic regions of parameter space
    if (is.finite(s)) s else obj_penalty
  }
  switch(length(model_param_names(f)),
    function(p) sse(f(ind, p[1])),
    function(p) sse(f(ind, p[1], p[2])),
    function(p) sse(f(ind, p[1], p[2], p[3])),
    stop("models with more than 3 parameters are not supported")
  )
}

# Run L-BFGS-B with a finite-difference step suited to the small rate
# parameters these models often have. optim's default step (ndeps = 1e-3) is
# too coarse when rates are ~1e-3 (e.g. time measured in days); 1e-4 gives
# equal or better fits on all models. A user-supplied control$ndeps wins.
run_optim <- function(par, fn, lower, upper, ...) {
  dots <- list(...)
  control <- if (is.null(dots$control)) list() else dots$control
  dots$control <- NULL
  if (is.null(control$ndeps)) {
    control$ndeps <- rep(1e-4, length(par))
  }
  do.call(stats::optim, c(
    list(par = par, fn = fn, method = "L-BFGS-B", lower = lower, upper = upper, control = control),
    dots
  ))
}

# Names of the free parameters for a given model function
model_param_names <- function(model) {
  args <- names(formals(model))
  args[-c(1, length(args) - 1, length(args))]
}

# Default bounds for a given model function
model_bounds <- function(model, which = c("upper", "lower")) {
  which <- match.arg(which)
  eval(formals(model)[[which]])
}


calculateLL <- function(predicted, observed, sigma) {
  return(sum(stats::dnorm(observed, mean = predicted, sd = sigma, log = T)))
}

calculateAIC <- function(LL, nparams) {
  return(-2 * LL + 2 * nparams)
}

calculateAICc <- function(LL, nparams, samplesize) {
  AIC <- calculateAIC(LL, nparams)
  AICc <- AIC + (2 * nparams * (nparams + 1)) / (samplesize - nparams - 1)
  return(AICc)
}

calculateBIC <- function(LL, nparams, samplesize) {
  return(-2 * LL + nparams * log(samplesize))
}

multioptimFit <- function(
  time_data, mass_data, model, iters = 200, upper = NULL,
  lower = NULL, quiet = FALSE, ...
) {
  nArgs <- length(model_param_names(model))
  upper_bounds <- if (!is.null(upper)) upper else model_bounds(model, "upper")
  lower_bounds <- if (!is.null(lower)) lower else model_bounds(model, "lower")

  objective <- make_objective(model, time_data, mass_data)
  fit <- vector("list", iters)
  for (i in seq_len(iters)) {
    # always start near lower bound--empirically works better
    starter <- runif(nArgs,
      min = lower_bounds,
      max = pmin(lower_bounds + 0.9998, upper_bounds)
    )
    fit[[i]] <- tryCatch(
      run_optim(starter, objective, lower_bounds, upper_bounds, ...),
      error = function(e) NULL
    )
  }
  successes <- vapply(fit, function(x) {
    !is.null(x) && x$convergence == 0 && x$value < obj_penalty
  }, logical(1))
  if (!quiet) {
    message(paste("Number of successful fits: ", sum(successes), " out of", iters))
  }
  if (sum(successes) == 0) {
    stop("All attempts failed to converge")
  }
  s.fit <- fit[successes]
  likes <- vapply(s.fit, function(x) x$value, numeric(1))
  # check whether the best few fits converged on the same solution
  if (length(likes) >= 5 && sd(sort(likes)[1:5]) > 0.01) {
    warning("May not have found global best fit; increase iterations")
  }
  s.fit[[which.min(likes)]]
}

are.within.ten.percent.of <- function(x, y) {
  return(y < 1.1 * x & y > 0.9 * x)
}

rnd.to.text <- function(x, digits = 4) {
  format(round(x, digits), scientific = F)
}


#' Simulate and Check Model Fitting with Litter Data
#'
#' This internal function first fits a model to the `pineneedles` data from the `litterfitter` package
#' using the `fit_litter` function. Next, it introduces random noise to the mass remaining data and refits
#' the model to this perturbed data. The function returns whether the time to proportion mass remaining
#' values from the two fits are within ten percent of each other.
#'
#' @keywords internal
#' @param model A character string representing the model type to be fitted using `fit_litter`.
#' @return Logical. TRUE if the time to proportion mass remaining values from the original and simulated
#' fits are within ten percent of each other; FALSE otherwise.
#' @seealso \code{\link[litterfitter]{fit_litter}}, \code{\link[litterfitter]{pineneedles}}
#'
simulate_and_check <- function(model) {
  suppressWarnings(fit <- fit_litter(
    time = litterfitter::pineneedles$Year, mass.remaining = litterfitter::pineneedles$Mass.remaining,
    model = model, iters = 1000, quiet = TRUE
  ))
  mass.with.error <- pmax(litterfitter::pineneedles$Mass.remaining + rnorm(
    length(predict(fit)), 0,
    1e-03
  ), 0)
  suppressWarnings(simulated.fit <- fit_litter(
    time = litterfitter::pineneedles$Year, mass.remaining = mass.with.error,
    model = model, iters = 1000, quiet = TRUE
  ))
  return(are.within.ten.percent.of(time_to_prop_mass_remaining(fit), time_to_prop_mass_remaining(simulated.fit)))
}
