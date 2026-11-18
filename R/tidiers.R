#' @importFrom generics tidy
#' @export
generics::tidy

#' @importFrom generics glance
#' @export
generics::glance

#' @importFrom generics augment
#' @export
generics::augment


# ---- internal helpers -------------------------------------------------------

check_conf_level <- function(conf.level) {
  if (!is.numeric(conf.level) || length(conf.level) != 1 || is.na(conf.level) ||
    conf.level <= 0 || conf.level >= 1) {
    stop("conf.level must be a single number between 0 and 1")
  }
  invisible(conf.level)
}

# Bootstrap parameter matrix (failed replicates removed), checked against a fit
boot_param_matrix <- function(fit, boot) {
  if (!inherits(boot, "litfit_bootstrap")) {
    stop("boot must be a 'litfit_bootstrap' object from bootstrap_parameters()")
  }
  boot_model <- attr(boot, "model")
  if (!is.null(boot_model) && !identical(boot_model, fit$model)) {
    stop(paste0(
      "boot was generated from a '", boot_model, "' fit but the fit is '",
      fit$model, "'"
    ))
  }
  m <- unclass(boot)
  attr(m, "model") <- NULL
  par_names <- names(coef(fit))
  if (!is.null(colnames(m))) {
    if (!all(par_names %in% colnames(m))) {
      stop("boot does not have the right parameters for this fit")
    }
    m <- m[, par_names, drop = FALSE]
  } else {
    if (ncol(m) < length(par_names)) {
      stop("boot does not have the right parameters for this fit")
    }
    m <- m[, seq_along(par_names), drop = FALSE]
    colnames(m) <- par_names
  }
  m <- m[stats::complete.cases(m), , drop = FALSE]
  if (nrow(m) < 2) {
    stop("boot has fewer than 2 successful replicates")
  }
  m
}

# Predicted curve for a parameter vector
predict_with_pars <- function(fit, pars, times) {
  do.call(fit$model, c(list(times), as.list(unname(pars))))
}

# Pointwise percentile band of curves predicted from bootstrap replicates
predict_band <- function(fit, boot, times, conf.level = 0.95) {
  check_conf_level(conf.level)
  m <- boot_param_matrix(fit, boot)
  preds <- matrix(
    vapply(seq_len(nrow(m)), function(i) predict_with_pars(fit, m[i, ], times),
      numeric(length(times))
    ),
    nrow = length(times)
  )
  alpha <- (1 - conf.level) / 2
  data.frame(
    .lower = apply(preds, 1, stats::quantile, probs = alpha, na.rm = TRUE, names = FALSE),
    .upper = apply(preds, 1, stats::quantile, probs = 1 - alpha, na.rm = TRUE, names = FALSE)
  )
}

newdata_times <- function(newdata, var = "time") {
  if (is.data.frame(newdata)) {
    if (!var %in% names(newdata)) {
      stop(sprintf("newdata must have a '%s' column", var))
    }
    times <- newdata[[var]]
  } else if (is.numeric(newdata)) {
    times <- newdata
  } else {
    stop(sprintf(
      "newdata must be a numeric vector or a data frame with a '%s' column", var
    ))
  }
  if (!is.numeric(times)) {
    stop(sprintf("the '%s' column of newdata must be numeric", var))
  }
  times
}


# ---- litfit methods ---------------------------------------------------------

#' Tidy, glance and augment methods for litfit objects
#'
#' These methods return the results of a \code{\link{fit_litter}} fit as data frames,
#' following the conventions of the \pkg{broom} package.
#'
#' \itemize{
#'   \item \code{tidy()}: one row per parameter.
#'   \item \code{glance()}: a one-row summary of the fit, including information criteria and
#'     the derived quantities time to 50\% mass loss and steady state.
#'   \item \code{augment()}: the data with fitted values and residuals, or predictions at
#'     new time points, optionally with a bootstrap uncertainty band.
#' }
#'
#' @param x a \code{litfit} object
#' @param boot optional \code{litfit_bootstrap} object from
#'   \code{\link{bootstrap_parameters}} for the same fit. In \code{tidy()} it adds bootstrap
#'   standard errors and percentile intervals; in \code{augment()} it adds a pointwise
#'   percentile band for the fitted curve.
#' @param conf.level confidence level for intervals and bands (default 0.95)
#' @param newdata optional numeric vector of times, or a data frame with a \code{time}
#'   column, at which to predict. If omitted, the original data are returned.
#' @param ... unused
#'
#' @return A \code{data.frame}.
#'   \describe{
#'     \item{\code{tidy()}}{\code{term}, \code{estimate}; with \code{boot} also
#'       \code{std.error}, \code{conf.low}, \code{conf.high}.}
#'     \item{\code{glance()}}{\code{model}, \code{nobs}, \code{nparams}, \code{sigma}
#'       (residual standard deviation used in the likelihood), \code{logLik}, \code{AIC},
#'       \code{AICc}, \code{BIC}, \code{converged}, \code{time_to_50}, \code{steady_state}.
#'       \code{time_to_50} is \code{NA} if the curve never reaches 50\% mass remaining, and
#'       \code{steady_state} can be \code{Inf} (see \code{\link{steady_state}}).}
#'     \item{\code{augment()}}{without \code{newdata}: \code{time}, \code{mass},
#'       \code{.fitted}, \code{.resid}. With \code{newdata}: the columns of \code{newdata}
#'       (or \code{time}) plus \code{.fitted}. With \code{boot}: also \code{.lower} and
#'       \code{.upper}.}
#'   }
#'
#' @seealso \code{\link{fit_litter}}, \code{\link{bootstrap_parameters}},
#'   \code{\link{fit_litter_all}}
#'
#' @examples
#' fit <- fit_litter(
#'   time = pineneedles$Year, mass.remaining = pineneedles$Mass.remaining,
#'   model = "weibull", iters = 100, quiet = TRUE
#' )
#' tidy(fit)
#' glance(fit)
#' augment(fit)
#'
#' boot <- bootstrap_parameters(fit, nboot = 100)
#' tidy(fit, boot = boot)
#' head(augment(fit, newdata = seq(0, 5, by = 0.5), boot = boot))
#'
#' @name litfit_tidiers
NULL

#' @rdname litfit_tidiers
#' @export
tidy.litfit <- function(x, boot = NULL, conf.level = 0.95, ...) {
  check_conf_level(conf.level)
  est <- coef(x)
  out <- data.frame(term = names(est), estimate = unname(est), stringsAsFactors = FALSE)
  if (!is.null(boot)) {
    m <- boot_param_matrix(x, boot)
    alpha <- (1 - conf.level) / 2
    out$std.error <- unname(apply(m, 2, stats::sd))
    out$conf.low <- unname(apply(m, 2, stats::quantile, probs = alpha, names = FALSE))
    out$conf.high <- unname(apply(m, 2, stats::quantile, probs = 1 - alpha, names = FALSE))
  }
  out
}

#' @rdname litfit_tidiers
#' @export
glance.litfit <- function(x, ...) {
  data.frame(
    model = x$model,
    nobs = length(x$mass),
    nparams = x$nparams,
    sigma = sqrt(x$optimFit$value / length(x$mass)),
    logLik = x$logLik,
    AIC = x$fitAIC,
    AICc = x$fitAICc,
    BIC = x$fitBIC,
    converged = isTRUE(x$optimFit$convergence == 0),
    time_to_50 = tryCatch(
      suppressWarnings(time_to_prop_mass_remaining(x, 0.5)),
      error = function(e) NA_real_
    ),
    steady_state = unname(suppressWarnings(steady_state(x))),
    stringsAsFactors = FALSE
  )
}

#' @rdname litfit_tidiers
#' @export
augment.litfit <- function(x, newdata = NULL, boot = NULL, conf.level = 0.95, ...) {
  check_conf_level(conf.level)
  if (is.null(newdata)) {
    times <- x$time
    out <- data.frame(
      time = x$time, mass = x$mass,
      .fitted = unname(fitted(x)), .resid = unname(residuals(x))
    )
  } else {
    times <- newdata_times(newdata)
    out <- if (is.data.frame(newdata)) newdata else data.frame(time = times)
    out$.fitted <- predict(x, newdata = times)
  }
  if (!is.null(boot)) {
    band <- predict_band(x, boot, times, conf.level)
    out$.lower <- band$.lower
    out$.upper <- band$.upper
  }
  rownames(out) <- NULL
  out
}


# ---- litfit_bootstrap methods -----------------------------------------------

#' Tidy a bootstrap distribution
#'
#' Returns the bootstrap replicates from \code{\link{bootstrap_parameters}} in long
#' format, one row per replicate and parameter (including the implied steady state).
#' Failed replicates are dropped.
#'
#' @param x a \code{litfit_bootstrap} object
#' @param ... unused
#'
#' @return A \code{data.frame} with columns \code{replicate}, \code{term} and \code{value}.
#'
#' @examples
#' fit <- fit_litter(
#'   time = pineneedles$Year, mass.remaining = pineneedles$Mass.remaining,
#'   model = "neg.exp", iters = 50, quiet = TRUE
#' )
#' boot <- bootstrap_parameters(fit, nboot = 50)
#' head(tidy(boot))
#'
#' @export
tidy.litfit_bootstrap <- function(x, ...) {
  m <- unclass(x)
  attr(m, "model") <- NULL
  keep <- which(!is.na(m[, 1]))
  m <- m[keep, , drop = FALSE]
  data.frame(
    replicate = rep(keep, times = ncol(m)),
    term = rep(colnames(m), each = length(keep)),
    value = as.vector(m),
    stringsAsFactors = FALSE
  )
}
