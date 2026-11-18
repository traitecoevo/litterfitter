#' Fit element (nutrient) dynamics during decomposition
#'
#' Fits a curve describing how the proportion of an element remaining (for example
#' nitrogen, \eqn{N/N_0}) changes with the proportion of litter mass remaining
#' (\eqn{M/M_0}) in a single decomposition cohort. Elements are often immobilised early in
#' decomposition (element remaining rises above 1) and released later.
#'
#' @param mass.remaining proportional mass remaining, \eqn{M/M_0}. Values slightly above
#'   1 or below 0 are kept and treated as measurement error, with a warning (see
#'   \code{\link{fit_litter}}).
#' @param element.remaining proportional element remaining, \eqn{N/N_0}. Values above 1
#'   (net immobilisation) are expected.
#' @param model the element model; currently only \code{"import.model"} (see Details)
#' @param iters number of random starts for the fitting
#' @param upper,lower optional bounds for the parameters, in the order given in Details
#' @param quiet if \code{TRUE}, suppress the message reporting the number of successful fits
#' @param ... additional arguments passed to \code{\link{optim}}
#'
#' @details
#' \strong{import.model} is the nitrogen release model of Parton et al. (2007), written
#' here for proportional mass remaining \eqn{x}:
#' \deqn{f(x) = \frac{x/b}{\sqrt{(2ax/b)^2 + (1 - (x/b)^2)^2}}, \qquad
#'   y = f(x) / f(1)}
#' so the curve passes through 1 at \eqn{x = 1} (no mass lost). \eqn{b} (bounds 0.01--1)
#' sets the mass remaining around which the immobilisation peak is centred, and \eqn{a}
#' (bounds 0.1--10) controls the height and width of the peak; small \eqn{a} gives a
#' tall, narrow peak. The original formulation used percent mass remaining (normalised
#' at 100, with \eqn{b} between 1 and 100); this version is equivalent with \eqn{b}
#' divided by 100.
#'
#' Fitting uses the same multi-start likelihood approach as \code{\link{fit_litter}}, and
#' gives the same warnings for parameters on a boundary and for mass values outside 0--1.
#'
#' @return An \code{elementfit} object, with methods for \code{print}, \code{plot},
#'   \code{coef}, \code{fitted}, \code{residuals}, \code{predict}, \code{nobs},
#'   \code{logLik} (so \code{AIC()} and \code{BIC()} work), \code{tidy}, \code{glance}
#'   and \code{augment}, and support in \code{\link{bootstrap_parameters}}. Its elements
#'   include \code{optimFit}, \code{logLik}, \code{fitAIC}, \code{fitAICc},
#'   \code{fitBIC}, \code{mass}, \code{element}, \code{predicted}, \code{model} and
#'   \code{nparams}.
#'
#' @references Parton, W., Silver, W. L., Burke, I. C., et al. (2007) Global-scale
#'   similarities in nitrogen release patterns during long-term decomposition.
#'   \emph{Science}, 315, 361--364.
#'
#' @author Baptiste Wijas, Will Cornwell
#'
#' @seealso \code{\link{fit_litter}} for mass loss through time,
#'   \code{\link{elementfit_methods}}, \code{\link{bootstrap_parameters}}
#'
#' @examples
#' fit <- fit_element(
#'   mass.remaining = pineneedles$Mass.remaining,
#'   element.remaining = c(1.1, 1.2, 1.3, 1, 0.8, 0.6),
#'   iters = 200, quiet = TRUE
#' )
#' fit
#' plot(fit)
#' glance(fit)
#'
#' @export
fit_element <- function(mass.remaining, element.remaining, model = c("import.model"),
                        iters = 500, upper = NULL, lower = NULL, quiet = FALSE, ...) {
  model <- match.arg(model)

  if (length(mass.remaining) != length(element.remaining)) {
    stop("Element remaining vector must have the same length and correspond to the mass remaining vector")
  }
  if (!is.numeric(mass.remaining) || !is.numeric(element.remaining)) {
    stop("mass.remaining and element.remaining must be numeric vectors")
  }
  if (!is.numeric(iters) || length(iters) != 1 || is.na(iters) || iters < 1) {
    stop("iters must be a single positive number")
  }

  missing_obs <- is.na(mass.remaining) | is.na(element.remaining)
  if (any(missing_obs)) {
    warning(warningCondition(
      paste(sum(missing_obs), "observation(s) with missing mass or element removed"),
      class = "litterfitter_missing_removed"
    ))
    mass.remaining <- mass.remaining[!missing_obs]
    element.remaining <- element.remaining[!missing_obs]
  }

  nparams_model <- length(model_param_names(model))
  if (length(mass.remaining) <= nparams_model) {
    stop(paste(
      "Too few observations to fit model", model, "- need more than",
      nparams_model, "data points"
    ))
  }
  if (any(!is.finite(mass.remaining)) || any(!is.finite(element.remaining))) {
    stop("mass.remaining and element.remaining must not contain infinite values")
  }
  if (any(mass.remaining < -0.1) || max(mass.remaining) > 2) {
    stop(paste(
      "Check mass remaining vector; must be in proportional mass remaining (between 0 and ~1).",
      "If it is in percent, divide by 100."
    ))
  }
  if (any(element.remaining < -0.1) || max(element.remaining) > 10) {
    stop(paste(
      "Check element remaining vector; must be in proportional element remaining (N/N0).",
      "If it is in percent, divide by 100."
    ))
  }
  if (length(unique(mass.remaining)) < 2) {
    stop("Need observations at two or more distinct values of mass remaining")
  }
  check_mass_range(mass.remaining)
  if (any(element.remaining < 0)) {
    warning(warningCondition(
      sprintf(
        "%d of %d element remaining values are below 0 (min %s). They are kept in the fit and treated as measurement error.",
        sum(element.remaining < 0), length(element.remaining),
        format(signif(min(element.remaining), 3))
      ),
      class = "litterfitter_element_below_zero"
    ))
  }

  default_upper <- model_bounds(model, "upper")
  default_lower <- model_bounds(model, "lower")
  if (!is.null(upper) && length(upper) != length(default_upper)) {
    stop(paste("Incorrect number of upper bounds supplied, should be:", length(default_upper)))
  }
  if (!is.null(lower) && length(lower) != length(default_lower)) {
    stop(paste("Incorrect number of lower bounds supplied, should be:", length(default_lower)))
  }
  test.upper <- if (is.null(upper)) default_upper else upper
  test.lower <- if (is.null(lower)) default_lower else lower
  if (any(test.lower >= test.upper)) {
    stop("Each lower bound must be smaller than the corresponding upper bound")
  }

  fit <- multioptimFit(mass.remaining, element.remaining, model,
    iters = as.integer(iters), upper = upper, lower = lower, quiet = quiet, ...
  )
  names(fit$par) <- model_param_names(model)

  predicted_vals <- do.call(model, c(list(mass.remaining), as.list(unname(fit$par))))
  n <- length(mass.remaining)
  LL <- calculateLL(predicted_vals, element.remaining, sqrt(fit$value / n))
  nps <- length(fit$par)
  fit.out <- list(
    optimFit = fit, logLik = LL,
    fitAIC = calculateAIC(LL, nps),
    fitAICc = calculateAICc(LL, nps, n),
    fitBIC = calculateBIC(LL, nps, n),
    mass = mass.remaining, element = element.remaining,
    predicted = predicted_vals, model = model, nparams = nps
  )
  class(fit.out) <- "elementfit"

  on_bound <- fit$par == test.lower | fit$par == test.upper
  if (any(on_bound)) {
    warning(paste0(
      "one or more parameters fit on the boundary (",
      paste(names(fit$par)[on_bound], collapse = ", "), "), check fit closely"
    ))
  }
  fit.out
}


#' Methods for elementfit objects
#'
#' Methods for element fits from \code{\link{fit_element}}. The predictor is proportional
#' mass remaining, so \code{newdata} gives mass remaining values.
#'
#' \code{glance()} reports, besides information criteria, the peak proportional element
#' remaining (\code{peak_element}) and the mass remaining at which it occurs
#' (\code{mass_at_peak}). A peak of 1 at a mass of 1 means no net immobilisation.
#'
#' @param x,object an \code{elementfit} object
#' @param newdata optional numeric vector of mass remaining values, or (for
#'   \code{augment()}) a data frame with a \code{mass} column
#' @param boot optional \code{litfit_bootstrap} object from
#'   \code{\link{bootstrap_parameters}} for the same fit
#' @param conf.level confidence level for intervals and bands
#' @param ... unused
#'
#' @return \code{tidy()}, \code{glance()} and \code{augment()} return a \code{data.frame}
#'   (see \code{\link{litfit_tidiers}} for the shared columns; \code{augment()} uses
#'   \code{mass} and \code{element} columns). \code{plot()} and \code{print()} return
#'   \code{x} invisibly.
#'
#' @examples
#' fit <- fit_element(
#'   mass.remaining = pineneedles$Mass.remaining,
#'   element.remaining = c(1.1, 1.2, 1.3, 1, 0.8, 0.6),
#'   iters = 200, quiet = TRUE
#' )
#' coef(fit)
#' predict(fit, newdata = c(1, 0.75, 0.5, 0.25))
#' tidy(fit)
#' glance(fit)
#' head(augment(fit, newdata = seq(1, 0, by = -0.1)))
#'
#' @name elementfit_methods
NULL

#' @rdname elementfit_methods
#' @export
print.elementfit <- function(x, ...) {
  cat("elementfit object: model", x$model, "fit to", length(x$mass), "observations\n")
  cat("Parameters:\n")
  print(signif(coef(x), 4))
  pk <- element_peak(x$model, coef(x))
  cat(sprintf(
    "Peak element remaining: %s at mass remaining %s\n",
    signif(pk$peak_element, 3), signif(pk$mass_at_peak, 3)
  ))
  cat(paste0(
    "logLik: ", round(x$logLik, 3), "   AIC: ", round(x$fitAIC, 3),
    "   AICc: ", round(x$fitAICc, 3), "   BIC: ", round(x$fitBIC, 3), "\n"
  ))
  invisible(x)
}

#' @rdname elementfit_methods
#' @export
coef.elementfit <- function(object, ...) {
  pars <- object$optimFit$par
  names(pars) <- model_param_names(object$model)
  pars
}

#' @rdname elementfit_methods
#' @export
fitted.elementfit <- function(object, ...) {
  object$predicted
}

#' @rdname elementfit_methods
#' @export
residuals.elementfit <- function(object, ...) {
  object$element - object$predicted
}

#' @rdname elementfit_methods
#' @export
nobs.elementfit <- function(object, ...) {
  length(object$element)
}

#' @rdname elementfit_methods
#' @export
logLik.elementfit <- function(object, ...) {
  structure(object$logLik,
    df = object$nparams,
    nobs = length(object$element),
    class = "logLik"
  )
}

#' @rdname elementfit_methods
#' @export
predict.elementfit <- function(object, newdata = NULL, ...) {
  X <- if (is.null(newdata)) object$mass else newdata
  do.call(object$model, c(list(X), as.list(unname(object$optimFit$par))))
}

#' @rdname elementfit_methods
#' @export
tidy.elementfit <- function(x, boot = NULL, conf.level = 0.95, ...) {
  tidy.litfit(x, boot = boot, conf.level = conf.level)
}

#' @rdname elementfit_methods
#' @export
glance.elementfit <- function(x, ...) {
  pk <- element_peak(x$model, coef(x))
  data.frame(
    model = x$model,
    nobs = length(x$element),
    nparams = x$nparams,
    sigma = sqrt(x$optimFit$value / length(x$element)),
    logLik = x$logLik,
    AIC = x$fitAIC,
    AICc = x$fitAICc,
    BIC = x$fitBIC,
    converged = isTRUE(x$optimFit$convergence == 0),
    peak_element = pk$peak_element,
    mass_at_peak = pk$mass_at_peak,
    stringsAsFactors = FALSE
  )
}

#' @rdname elementfit_methods
#' @export
augment.elementfit <- function(x, newdata = NULL, boot = NULL, conf.level = 0.95, ...) {
  check_conf_level(conf.level)
  if (is.null(newdata)) {
    masses <- x$mass
    out <- data.frame(
      mass = x$mass, element = x$element,
      .fitted = unname(fitted(x)), .resid = unname(residuals(x))
    )
  } else {
    masses <- newdata_times(newdata, var = "mass")
    out <- if (is.data.frame(newdata)) newdata else data.frame(mass = masses)
    out$.fitted <- predict(x, newdata = masses)
  }
  if (!is.null(boot)) {
    band <- predict_band(x, boot, masses, conf.level)
    out$.lower <- band$.lower
    out$.upper <- band$.upper
  }
  rownames(out) <- NULL
  out
}
