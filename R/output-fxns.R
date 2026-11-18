#' Plot a Litter Decomposition Trajectory with Curve Fit
#'
#' Visualizes the litter decomposition trajectory data and its curve fit
#' derived from a `litfit` object. This function is designed to provide
#' a quick visual check on the adequacy of model fitting.
#'
#' @title Plot Decomposition Trajectory and Curve Fit
#' @param x A `litfit` object.
#' @param formulae.cex Size scaling factor for the formula display on the plot.
#' @param ... Additional arguments passed to \code{\link[graphics]{plot.default}}.
#' @return A plot visualizing the data and curve fit from a `litfit` object.
#'         The result is returned invisibly.
#' @details The plot displays data points from the `litfit` object along with
#'          the curve fit. The formula for the fit is displayed on the plot.
#' @seealso \code{\link{fit_litter}} for generating `litfit` objects.
#' @author Will Cornwell
#' @examples
#' fit <- fit_litter(
#'   time = c(0, 1, 2, 3, 4, 5, 6),
#'   mass.remaining = c(1, 0.9, 1.01, 0.4, 0.6, 0.2, 0.01),
#'   "neg.exp",
#'   iters = 250
#' )
#' plot(fit)
#'
#' @export
#' @importFrom graphics abline grconvertX grconvertY legend lines strwidth text
#'


plot.litfit <- function(x, formulae.cex = 1, ...) {
  plot(
    x$mass ~ x$time,
    pch = 16,
    xlab = "Time",
    ylab = "Proportion mass remaining",
    xlim = c(0, max(x$time)),
    ylim = c(0, max(1, x$mass)),
    main = x$model,
    ...
  )
  time.vec <- seq(0, max(x$time), length.out = 500)
  lines(time.vec, predict(x, newdata = time.vec))
  pt.pos <-
    c(grconvertX(0.5, from = "npc"), grconvertY(0.95, from = "npc"))

  formula.text <-
    switch(x$model,
      neg.exp = substitute(paste(y == e^A), list(A = paste(
        "-",
        rnd.to.text(x$optimFit$par, 3), "t",
        sep = ""
      ))),
      weibull = substitute(paste(y ==
        e^{
          -(frac(t, A))^B
        }), list(
        A = round(x$optimFit$par[1], 3),
        B = round(
          x$optimFit$par[2],
          3
        )
      )),
      discrete.parallel = substitute(
        paste(y == A * e^{
          B * t
        } + C * e^{
          D * t
        }),
        list(
          A = rnd.to.text(x$optimFit$par[1], 4),
          B = rnd.to.text(
            -1 * x$optimFit$par[2],
            4
          ),
          C = rnd.to.text(1 - x$optimFit$par[1], 4),
          D = rnd.to.text(
            -1 * x$optimFit$par[3],
            4
          )
        )
      ),
      discrete.series = substitute(
        paste(y == frac(A * e^{
          C * t
        } * sign * D * e^{
          F * t
        }, G)),
        list(
          A = rnd.to.text((1 - x$optimFit$par[1]) * x$optimFit$par[2]),
          C = rnd.to.text(-1 *
            x$optimFit$par[3]),
          D = rnd.to.text(x$optimFit$par[3] - x$optimFit$par[2] *
            x$optimFit$par[1]),
          F = rnd.to.text(-1 * x$optimFit$par[2]),
          G = rnd.to.text(x$optimFit$par[2] -
            x$optimFit$par[3]),
          sign = ifelse(x$optimFit$par[3] - x$optimFit$par[2] *
            x$optimFit$par[1] > 0, "-", "")
        )
      ),
      cont.quality = substitute(paste(y == frac(1, (
        1 + B * t
      )^A)), list(
        A = rnd.to.text(x$optimFit$par[2]),
        B = rnd.to.text(x$optimFit$par[1])
      )),
      neg.exp.limit = substitute(
        paste(y ==
          A * e^{
            -K * t
          } + B),
        list(
          K = rnd.to.text(x$optimFit$par[1]),
          A = rnd.to.text(x$optimFit$par[2]),
          B = rnd.to.text(x$optimFit$par[3])
        )
      )
    )
  text(pt.pos[1], pt.pos[2], label = formula.text, cex = formulae.cex)
  invisible(x)
}

#' @export
coef.litfit <- function(object, ...) {
  pars <- object$optimFit$par
  names(pars) <- model_param_names(object$model)
  pars
}

#' @export
residuals.litfit <- function(object, ...) {
  object$mass - object$predicted
}

#' @export
nobs.litfit <- function(object, ...) {
  length(object$mass)
}

#' Log-likelihood of a litfit object
#'
#' Returns the log-likelihood of the fitted model, which allows the standard
#' \code{\link[stats]{AIC}} and \code{\link[stats]{BIC}} functions to be used to
#' compare several \code{litfit} objects, e.g. \code{AIC(fit1, fit2)}.
#'
#' @param object a \code{litfit} object
#' @param ... unused
#'
#' @details The degrees of freedom are the number of fitted curve parameters
#'   (the residual standard deviation is not counted), consistent with the
#'   \code{fitAIC}, \code{fitAICc} and \code{fitBIC} values stored in the object
#'   and with Cornwell & Weedon (2013).
#'
#' @return an object of class \code{logLik}
#' @export
logLik.litfit <- function(object, ...) {
  structure(object$logLik,
    df = object$nparams,
    nobs = length(object$mass),
    class = "logLik"
  )
}

#' @export
print.litfit <- function(x, ...) {
  cat("litfit object: model", x$model, "fit to", length(x$mass), "observations\n")
  cat("Parameters:\n")
  print(signif(coef(x), 4))
  cat(paste0(
    "logLik: ", round(x$logLik, 3), "   AIC: ", round(x$fitAIC, 3),
    "   AICc: ", round(x$fitAICc, 3), "   BIC: ", round(x$fitBIC, 3), "\n"
  ))
  invisible(x)
}

#' @export
fitted.litfit <- function(object, ...) {
  object$predicted
}

#' @export
print.summary.litfit <- function(x, ...) {
  # prototype, to be expanded and improved
  cat("Summary of litFit object\n")
  cat(paste("Model type:", x$model, "\n"))
  cat(paste("Number of observations: ", x$num.obs, "\n"))
  pars <- x$optimFit$par
  par_names <- model_param_names(x$model)
  for (i in seq_along(pars)) {
    cat(paste0("Parameter fit (", par_names[i], "): ", signif(pars[i], 3), "\n"))
  }
  ttf <- if (is.null(x$time_to_50)) NA_real_ else x$time_to_50
  cat(paste("Time to 50% mass loss:", signif(ttf, 3), "\n"))
  cat(paste(
    "Implied steady state litter mass:",
    signif(x$steady_state, 3),
    "in units of yearly input",
    "\n"
  ))
  cat(paste("AIC: ", round(x$fitAIC, 4), "\n"))
  cat(paste("AICc: ", round(x$fitAICc, 4), "\n"))
  cat(paste("BIC: ", round(x$fitBIC, 4), "\n"))
  invisible(x)
}

#' @export
summary.litfit <- function(object, ...) {
  ans <- object
  class(ans) <- "summary.litfit"
  ans$num.obs <- sum(!is.na(ans$mass) & !is.na(ans$time))
  ans$mass <- NULL
  ans$time <- NULL
  ans$predicted <- NULL
  ans$steady_state <- unname(steady_state(object))
  ans$time_to_50 <- tryCatch(
    time_to_prop_mass_remaining(object, 0.5),
    error = function(e) NA_real_
  )
  ans
}

#' Generated predicted values for (new) time points from a litfit model fit
#'
#' @title Predict method for litfit objects
#'
#' @usage \method{predict}{litfit}(object,newdata=NULL,...)
#'
#' @param object litfit object
#'
#' @param newdata optional vector of new Time points at which to predict mass remaining. If not specified, Time points from the original fit are used.
#'
#' @param ... further arguments passed to or from other methods.
#'
#' @details to do
#'
#' @seealso \code{\link{fit_litter}}
#'
#' @author Will Cornwell
#' @author James Weedon
#'
#' @examples fit <- fit_litter(
#'   time = c(0, 1, 2, 3, 4, 5, 6), mass.remaining = c(1, 0.9, 1.01, 0.4, 0.6, 0.2, 0.01),
#'   "neg.exp", iters = 250
#' )
#' predict(fit, newdata = 1:10)
#'
#' @return predicted values from a litfit object
#' @export
predict.litfit <- function(object, newdata = NULL, ...) {
  if (is.null(newdata)) {
    X <- object$time
  } else {
    X <- newdata
  }

  do.call(object$model, c(list(X), as.list(unname(object$optimFit$par))))
}


#' Estimate Steady State Biomass
#'
#' Computes the steady state biomass, as a proportion of the annual input, based on a given model fit or parameters.
#'
#' @title Estimate Steady State Biomass
#' @param x A `litfit` object. If provided, `pars` and `model` parameters are extracted from this object.
#' @param pars A numeric vector of parameters for the model. Only needed if `x` is not provided.
#' @param model A character string specifying the decomposition model. Must be one of the following:
#'   "neg.exp", "weibull", "discrete.parallel", "discrete.series", "cont.quality" or "neg.exp.limit".
#'   Only needed if `x` is not provided.
#' @return A named numeric value representing the estimated steady state biomass from the specified model.
#' @details For \code{neg.exp.limit}, a fraction \eqn{B} of the litter never decomposes, so
#'   the implied steady state is infinite whenever \eqn{B > 0}.
#' @seealso \code{\link{fit_litter}} for generating `litfit` objects.
#' @examples
#' # Example with litfit object
#' fit <- fit_litter(
#'   time = c(0, 1, 2, 3, 4, 5, 6),
#'   mass.remaining = c(1, 0.9, 1.01, 0.4, 0.6, 0.2, 0.01),
#'   model = "neg.exp",
#'   iters = 250
#' )
#' steady_state(fit)
#'
#' # Example with specific model and parameter values
#' steady_state(pars = c(6, 2), model = "weibull")
#'
#' @author Will Cornwell
#' @export
steady_state <- function(x = NULL,
                         pars = NULL,
                         model = NULL) {
  if (!inherits(x, "litfit") &&
    (!is.character(model) || !is.numeric(pars))) {
    stop(
      "Something went wrong -- litterfitter::steady_state requires either a litfit
             object, or a model name and the appropriate numeric vector of parameter values"
    )
  }
  if (inherits(x, "litfit")) {
    model <- x$model
    pars <- x$optimFit$par
  }

  out <- switch(model,
    neg.exp = negexp.steadystate(pars),
    weibull = weibull.steadystate(pars[1], pars[2]),
    discrete.parallel = discrete.parallel.steadystate(pars[1], pars[2], pars[3]),
    discrete.series = discrete.series.steadystate(pars[1], pars[2], pars[3]),
    cont.quality = cont.quality.steadystate(pars[1], pars[2]),
    neg.exp.limit = neg.exp.limit.steadystate(pars[1], pars[2], pars[3])
  )
  if (is.null(out)) {
    stop("model name not in the current set of implemented models, see ?fit_litter")
  }
  out <- unname(out)
  names(out) <- model
  out
}


#' Plot multiple fits of decomposition trajectories on one graph with model selection results displayed
#'
#' @title Plot multiple fits on one graph with model selection results displayed
#'
#' @usage plot_multiple_fits(time,mass.remaining,model,color,iters,bty,...)
#'
#' @param time vector of time points
#'
#' @param mass.remaining vector of mass remaining
#'
#' @param model vector of models to fit and plot (see \code{\link{fit_litter}})
#'
#' @param color a vector of colors the same length as the number of models
#'
#' @param iters parameter passed to \code{\link{fit_litter}}
#'
#' @param bty bty
#'
#' @param ... additional parameters passed to \code{\link{plot}}
#'
#' @details this function is designed to compare a variety of curve shapes visually and with AIC and BIC simultaneously
#' @return plot of multiple fits; invisibly returns the list of \code{litfit} objects
#'
#' @seealso \code{\link{fit_litter}} \code{\link{plot.litfit}}
#'
#' @author Liu Guofang
#'
#' @examples data(pineneedles, package = "litterfitter")
#'
#' plot_multiple_fits(
#'   time = pineneedles$Year,
#'   mass.remaining = pineneedles$Mass.remaining,
#'   bty = "n", model = c("neg.exp", "weibull"),
#'   xlab = "Time", ylab = "Proportion mass remaining", iters = 200
#' )
#'
#' @export plot_multiple_fits
plot_multiple_fits <-
  function(time,
           mass.remaining,
           model = c(
             "neg.exp",
             "weibull",
             "discrete.parallel",
             "discrete.series",
             "cont.quality",
             "neg.exp.limit"
           ),
           color = NULL,
           iters = 500,
           bty = "o",
           ...) {
    model <- match.arg(model, several.ok = TRUE)
    fixed_string_width <- function(string) {
      empty.char <- strwidth(" ", units = "inch")
      value <- strwidth(string, units = "inch")
      diff <- max(value) - value
      number <- round(diff / empty.char, 0)
      return(lapply(seq_along(string), function(i) {
        paste(
          string[i], paste(rep(
            " ",
            number[i]
          ), collapse = ""),
          sep = ""
        )
      }))
    }

    mod.lst <-
      lapply(model, function(x) {
        fit_litter(
          time = time,
          mass.remaining = mass.remaining,
          model = x,
          iters = iters,
          quiet = TRUE
        )
      })

    N <- length(model)
    if (is.null(color)) {
      color <- grDevices::palette()[1:N]
    }
    plot(time,
      mass.remaining,
      pch = 19,
      ylim = c(0, max(1, mass.remaining, na.rm = TRUE)),
      ...
    )

    time.vec <- seq(0, max(time, na.rm = TRUE), length.out = 500)
    for (i in seq_len(N)) {
      lines(time.vec, predict(mod.lst[[i]], newdata = time.vec), col = color[i])
    }

    values <- do.call(rbind, lapply(mod.lst, function(x) {
      data.frame(
        AIC = round(x$fitAIC, 2),
        BIC = round(x$fitBIC, 2)
      )
    }))
    values <- data.frame(model, values)
    values <- values[order(values$AIC), ]
    legend(
      "topright",
      lty = c(NA, rep(1, N)),
      legend = sprintf(
        "%s %s  %s",
        fixed_string_width(c(
          "",
          as.character(values$model)
        )),
        fixed_string_width(c("AIC", values$AIC)),
        fixed_string_width(c(
          "BIC",
          values$BIC
        ))
      ),
      col = c(NA, color[as.numeric(row.names(values))]),
      bty = bty
    )
    invisible(mod.lst)
  }


#' Get estimated time to 0.5 (or an alternate threshold) mass loss from a particular fit to a litter decomposition trajectory
#'
#'
#' @title Get the predicted time until half mass loss for a litter decomposition trajectory
#'
#' @usage time_to_prop_mass_remaining(x,threshold.mass=0.5)
#'
#' @param x a litfit object
#'
#' @param threshold.mass mass loss threshold in proportion, default is 0.5
#'
#' @details this function finds the time to a specified mass loss percentage
#' @return numeric value that represents time to a specified mass loss percentage
#' @seealso \code{\link{fit_litter}} \code{\link{plot.litfit}}
#'
#' @author Will Cornwell
#'
#' @examples
#'
#' fit <- fit_litter(
#'   time = pineneedles$Year, mass.remaining = pineneedles$Mass.remaining,
#'   model = "neg.exp", iters = 1000
#' )
#' time_to_prop_mass_remaining(fit, threshold.mass = 0.5)
#'
#' @export
#'
time_to_prop_mass_remaining <- function(x, threshold.mass = 0.5) {
  if (!inherits(x, "litfit")) {
    stop("litterfitter::time_to_prop_mass_remaining takes a 'litfit' object")
  }
  if (!is.numeric(threshold.mass) || length(threshold.mass) != 1 ||
    threshold.mass <= 0 || threshold.mass >= 1) {
    stop("threshold.mass must be a single number between 0 and 1")
  }
  f <- function(t) predict(x, newdata = t) - threshold.mass

  t_max <- max(x$time)
  if (f(t_max) > 0) {
    warning("Not predicted to reach threshold mass (usu 50% loss) within the time range of the data.")
    # extend the search window until the curve crosses the threshold
    found <- FALSE
    for (i in seq_len(20)) {
      t_max <- t_max * 2
      val <- f(t_max)
      if (is.finite(val) && val <= 0) {
        found <- TRUE
        break
      }
    }
    if (!found) {
      warning("Model never predicts reaching the threshold mass; returning NA")
      return(NA_real_)
    }
  }
  if (f(0) <= 0) {
    return(0)
  }
  stats::uniroot(f, lower = 0, upper = t_max, tol = .Machine$double.eps^0.5)$root
}
