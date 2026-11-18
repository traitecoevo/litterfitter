# ---- shared plotting helpers ------------------------------------------------

# Colour-blind safe palette (Okabe-Ito without black, which is used for data)
okabe_ito <- function(n) {
  cols <- grDevices::palette.colors(9, palette = "Okabe-Ito")[-1]
  unname(rep_len(cols, n))
}

# Add default arguments that the user has not supplied in `...`
merge_dots <- function(dots, defaults) {
  for (nm in names(defaults)) {
    if (!nm %in% names(dots)) dots[[nm]] <- defaults[[nm]]
  }
  dots
}

fmt_num <- function(v, digits = 3) {
  vapply(v, function(z) format(signif(z, digits)), character(1), USE.NAMES = FALSE)
}

#' Equation of a fitted curve, for plot labels
#'
#' @param fit a \code{litfit} or \code{elementfit} object
#' @param type \code{"plotmath"} returns a language object for base graphics;
#'   \code{"text"} returns a plain character string (used by \code{autoplot()})
#' @return a call (plotmath) or a character string (text)
#' @keywords internal
#' @noRd
model_equation <- function(fit, type = c("plotmath", "text")) {
  type <- match.arg(type)
  p <- unname(fit$optimFit$par)
  if (fit$model == "discrete.series" && p[2] == p[3]) {
    k <- p[2]
    transfer <- (1 - p[1]) * k
    if (type == "text") {
      return(sprintf("y = exp(-%s t) (1 + %s t)", fmt_num(k), fmt_num(transfer)))
    }
    return(substitute(y == e^(-K * t) * (1 + C * t),
                      list(K = signif(k, 3), C = signif(transfer, 3))))
  }
  if (type == "text") {
    return(switch(fit$model,
      neg.exp = sprintf("y = exp(-%s t)", fmt_num(p[1])),
      weibull = sprintf("y = exp(-(t / %s)^%s)", fmt_num(p[1]), fmt_num(p[2])),
      discrete.parallel = sprintf(
        "y = %s exp(-%s t) + %s exp(-%s t)",
        fmt_num(p[1]), fmt_num(p[2]), fmt_num(1 - p[1]), fmt_num(p[3])
      ),
      discrete.series = sprintf(
        "y = [%s exp(-%s t) - %s exp(-%s t)] / %s",
        fmt_num((1 - p[1]) * p[2]), fmt_num(p[3]), fmt_num(p[3] - p[2] * p[1]),
        fmt_num(p[2]), fmt_num(p[2] - p[3])
      ),
      cont.quality = sprintf("y = 1 / (1 + %s t)^%s", fmt_num(p[1]), fmt_num(p[2])),
      neg.exp.limit = sprintf(
        "y = %s exp(-%s t) + %s", fmt_num(p[2]), fmt_num(p[1]), fmt_num(p[3])
      ),
      import.model = sprintf(
        "Parton et al. (2007) import model: a = %s, b = %s", fmt_num(p[1]), fmt_num(p[2])
      ),
      paste(paste(names(fit$optimFit$par), "=", fmt_num(p)), collapse = ", ")
    ))
  }
  switch(fit$model,
    neg.exp = substitute(y == e^A, list(A = paste0("-", rnd.to.text(p[1], 3), "t"))),
    weibull = substitute(y == e^{
      -(frac(t, A))^B
    }, list(A = round(p[1], 3), B = round(p[2], 3))),
    discrete.parallel = substitute(
      y == A * e^{
        B * t
      } + C * e^{
        D * t
      },
      list(
        A = rnd.to.text(p[1], 4), B = rnd.to.text(-p[2], 4),
        C = rnd.to.text(1 - p[1], 4), D = rnd.to.text(-p[3], 4)
      )
    ),
    discrete.series = substitute(
      y == frac(A * e^{
        C * t
      } * sign * D * e^{
        F * t
      }, G),
      list(
        A = rnd.to.text((1 - p[1]) * p[2]), C = rnd.to.text(-p[3]),
        D = rnd.to.text(abs(p[3] - p[2] * p[1])), F = rnd.to.text(-p[2]),
        G = rnd.to.text(p[2] - p[3]),
        sign = if (p[3] - p[2] * p[1] > 0) "-" else "+"
      )
    ),
    cont.quality = substitute(y == frac(1, (1 + B * t)^A), list(
      A = rnd.to.text(p[2]), B = rnd.to.text(p[1])
    )),
    neg.exp.limit = substitute(y == A * e^{
      -K * t
    } + B, list(
      K = rnd.to.text(p[1]), A = rnd.to.text(p[2]), B = rnd.to.text(p[3])
    )),
    import.model = substitute(paste(a == A, ",  ", b == B), list(
      A = rnd.to.text(p[1], 3), B = rnd.to.text(p[2], 3)
    )),
    model_equation(fit, "text")
  )
}

# Shade a band given x, lower and upper vectors
draw_band <- function(x, lower, upper, col = grDevices::adjustcolor("grey60", alpha.f = 0.35)) {
  ok <- is.finite(lower) & is.finite(upper)
  graphics::polygon(c(x[ok], rev(x[ok])), c(lower[ok], rev(upper[ok])), col = col, border = NA)
}


# ---- litfit -----------------------------------------------------------------

#' Plot a decomposition trajectory and its fitted curve
#'
#' Plots the data and fitted curve from a \code{litfit} object, with the fitted equation.
#' If a bootstrap is supplied, a pointwise percentile band for the curve is added.
#'
#' @param x a \code{litfit} object from \code{\link{fit_litter}}
#' @param formulae.cex size of the equation text; use \code{NA} to omit the equation
#' @param boot optional \code{litfit_bootstrap} object from
#'   \code{\link{bootstrap_parameters}} for the same fit, to draw an uncertainty band
#' @param conf.level confidence level for the band (default 0.95)
#' @param ... additional arguments passed to \code{\link[graphics]{plot.default}}, which
#'   override the defaults (e.g. \code{xlab}, \code{ylab}, \code{main}, \code{pch})
#' @return \code{x}, invisibly
#' @seealso \code{\link{fit_litter}}, \code{\link{plot.litfit_list}};
#'   \code{autoplot()} methods for ggplot2 output (\code{\link{litterfitter_autoplot}})
#' @author Will Cornwell
#' @examples
#' fit <- fit_litter(
#'   time = pineneedles$Year, mass.remaining = pineneedles$Mass.remaining,
#'   model = "weibull", iters = 100, quiet = TRUE
#' )
#' plot(fit)
#'
#' boot <- bootstrap_parameters(fit, nboot = 100)
#' plot(fit, boot = boot, main = "Weibull fit with 95% band")
#'
#' @importFrom graphics abline legend lines points polygon
#' @export
plot.litfit <- function(x, formulae.cex = 1, boot = NULL, conf.level = 0.95, ...) {
  grid <- augment(x,
    newdata = seq(0, max(x$time), length.out = 500), boot = boot, conf.level = conf.level
  )
  ymax <- max(1, x$mass, grid$.fitted, grid$.upper, na.rm = TRUE)
  args <- merge_dots(list(...), list(
    pch = 16, xlab = "Time", ylab = "Proportion mass remaining",
    xlim = c(0, max(x$time)), ylim = c(0, ymax), main = x$model
  ))
  do.call(graphics::plot, c(list(x = x$time, y = x$mass, type = "n"), args))
  if (!is.null(boot)) draw_band(grid$time, grid$.lower, grid$.upper)
  lines(grid$time, grid$.fitted)
  points(x$time, x$mass, pch = args$pch, col = if (is.null(args$col)) "black" else args$col)
  if (!is.na(formulae.cex)) {
    legend("topright",
      legend = as.expression(model_equation(x, "plotmath")),
      bty = "n", cex = formulae.cex
    )
  }
  invisible(x)
}


# ---- elementfit -------------------------------------------------------------

#' Plot an element fit
#'
#' Plots proportional element remaining against proportional mass remaining, with mass
#' remaining decreasing from left to right so that decomposition proceeds along the x
#' axis. A dashed line marks the initial amount of the element.
#'
#' @param x an \code{elementfit} object from \code{\link{fit_element}}
#' @param formulae.cex size of the parameter text; use \code{NA} to omit it
#' @param boot optional \code{litfit_bootstrap} object for the same fit, to draw an
#'   uncertainty band
#' @param conf.level confidence level for the band (default 0.95)
#' @param ... additional arguments passed to \code{\link[graphics]{plot.default}}
#' @return \code{x}, invisibly
#' @seealso \code{\link{fit_element}}
#' @examples
#' fit <- fit_element(
#'   mass.remaining = pineneedles$Mass.remaining,
#'   element.remaining = c(1.1, 1.2, 1.3, 1, 0.8, 0.6),
#'   iters = 200, quiet = TRUE
#' )
#' plot(fit)
#' @export
plot.elementfit <- function(x, formulae.cex = 1, boot = NULL, conf.level = 0.95, ...) {
  grid <- augment(x,
    newdata = seq(0, max(1, x$mass), length.out = 500), boot = boot, conf.level = conf.level
  )
  ymax <- max(1, x$element, grid$.fitted, grid$.upper, na.rm = TRUE)
  args <- merge_dots(list(...), list(
    pch = 16, xlab = "Proportion mass remaining", ylab = "Proportion element remaining",
    xlim = c(max(1, x$mass), 0), ylim = c(0, ymax), main = x$model
  ))
  do.call(graphics::plot, c(list(x = x$mass, y = x$element, type = "n"), args))
  abline(h = 1, lty = 2, col = "grey50")
  if (!is.null(boot)) draw_band(grid$mass, grid$.lower, grid$.upper)
  lines(grid$mass, grid$.fitted)
  points(x$mass, x$element, pch = args$pch, col = if (is.null(args$col)) "black" else args$col)
  if (!is.na(formulae.cex)) {
    legend("bottomleft",
      legend = as.expression(model_equation(x, "plotmath")),
      bty = "n", cex = formulae.cex
    )
  }
  invisible(x)
}


# ---- litfit_list ------------------------------------------------------------

#' Plot several fits to the same data
#'
#' Plots the data with the fitted curve of every model in a \code{litfit_list}, and a
#' legend table of AIC, AICc and BIC sorted by AICc.
#'
#' @param x a \code{litfit_list} from \code{\link{fit_litter_all}} or
#'   \code{\link{as_litfit_list}}
#' @param col colours for the curves, recycled; defaults to the colour-blind safe
#'   Okabe-Ito palette
#' @param legend.pos position of the legend, passed to \code{\link[graphics]{legend}};
#'   use \code{NA} to omit it
#' @param legend.bty box type of the legend (\code{"o"} or \code{"n"})
#' @param legend.cex size of the legend text
#' @param ... additional arguments passed to \code{\link[graphics]{plot.default}}
#' @details Data points are taken from the first fit; fits in a \code{litfit_list} are
#'   normally made to the same data.
#' @return \code{x}, invisibly
#' @seealso \code{\link{plot_multiple_fits}}, \code{\link{glance.litfit_list}}
#' @examples
#' fits <- fit_litter_all(
#'   time = pineneedles$Year, mass.remaining = pineneedles$Mass.remaining,
#'   models = c("neg.exp", "weibull", "cont.quality"), iters = 100
#' )
#' plot(fits)
#' @export
plot.litfit_list <- function(x, col = NULL, legend.pos = "topright", legend.bty = "o",
                             legend.cex = 0.8, ...) {
  first <- x[[1]]
  n <- length(x)
  col <- if (is.null(col)) okabe_ito(n) else rep_len(col, n)
  times <- seq(0, max(first$time), length.out = 500)
  curves <- lapply(unclass(x), predict, newdata = times)
  ymax <- max(1, first$mass, unlist(curves), na.rm = TRUE)
  args <- merge_dots(list(...), list(
    pch = 19, xlab = "Time", ylab = "Proportion mass remaining",
    xlim = c(0, max(first$time)), ylim = c(0, ymax)
  ))
  do.call(graphics::plot, c(list(x = first$time, y = first$mass), args))
  for (i in seq_len(n)) lines(times, curves[[i]], col = col[i], lwd = 1.5)

  if (!is.na(legend.pos)) {
    g <- glance(x)
    idx <- match(g$fit, names(x))
    num <- function(v) formatC(v, format = "f", digits = 2)
    labels <- paste(
      format(c("", g$fit)),
      format(c("AIC", num(g$AIC)), justify = "right"),
      format(c("AICc", num(g$AICc)), justify = "right"),
      format(c("BIC", num(g$BIC)), justify = "right"),
      sep = "  "
    )
    op <- graphics::par(family = "mono")
    on.exit(graphics::par(op), add = TRUE)
    legend(legend.pos,
      legend = labels, lty = c(NA, rep(1, n)), lwd = 1.5,
      col = c(NA, col[idx]), bty = legend.bty, cex = legend.cex
    )
  }
  invisible(x)
}

#' Fit and plot several models on one graph with model selection results
#'
#' Fits several models to the same data with \code{\link{fit_litter_all}} and plots them
#' together with \code{\link{plot.litfit_list}}. Existing fits can be plotted instead
#' by passing a \code{litfit_list} (or a list of \code{litfit} objects) as the first
#' argument, which avoids refitting.
#'
#' @param time vector of time points, or a \code{litfit_list} / list of \code{litfit}
#'   objects to plot without refitting
#' @param mass.remaining vector of mass remaining (ignored when \code{time} is a list of
#'   fits)
#' @param model vector of models to fit and plot (see \code{\link{fit_litter}})
#' @param color colours for the curves; defaults to the Okabe-Ito palette
#' @param iters number of random starts, passed to \code{\link{fit_litter}}
#' @param bty box type of the legend
#' @param ... additional arguments passed to \code{\link[graphics]{plot.default}}
#'
#' @return the fits, invisibly, as a \code{litfit_list}
#'
#' @seealso \code{\link{fit_litter_all}}, \code{\link{plot.litfit_list}}
#'
#' @author Liu Guofang
#'
#' @examples
#' plot_multiple_fits(
#'   time = pineneedles$Year,
#'   mass.remaining = pineneedles$Mass.remaining,
#'   bty = "n", model = c("neg.exp", "weibull"), iters = 200
#' )
#'
#' # plot existing fits without refitting
#' fits <- fit_litter_all(pineneedles$Year, pineneedles$Mass.remaining,
#'   models = c("neg.exp", "cont.quality"), iters = 100
#' )
#' plot_multiple_fits(fits)
#'
#' @export
plot_multiple_fits <- function(time,
                               mass.remaining,
                               model = c(
                                 "neg.exp", "weibull", "discrete.parallel",
                                 "discrete.series", "cont.quality", "neg.exp.limit"
                               ),
                               color = NULL,
                               iters = 500,
                               bty = "o",
                               ...) {
  if (inherits(time, "litfit_list")) {
    fits <- time
  } else if (inherits(time, "litfit") || (is.list(time) && !is.data.frame(time))) {
    fits <- as_litfit_list(time)
  } else {
    fits <- fit_litter_all(time, mass.remaining, models = model, iters = iters)
  }
  plot(fits, col = color, legend.bty = bty, ...)
  invisible(fits)
}


# ---- litfit_bootstrap -------------------------------------------------------

#' Plot bootstrap distributions
#'
#' Plots the density of the bootstrap distribution of one or more parameters (or derived
#' quantities) from \code{\link{bootstrap_parameters}}.
#'
#' @param x a \code{litfit_bootstrap} object
#' @param coef.index which column(s) to plot: a number, a column name such as \code{"k"}
#'   or \code{"steady_state"}, a vector of either, or \code{"all"}. Several columns are
#'   drawn in a grid of panels. With \code{"all"}, columns without at least two finite
#'   values (e.g. an always-infinite steady state) are skipped with a warning.
#' @param bw bandwidth (or bandwidth rule, see \code{\link{density}}) for the density
#' @param conf.level width of the shaded central interval (default 0.95)
#' @param ... additional arguments passed to \code{\link[graphics]{plot.default}}
#'
#' @details The grey fill covers the central \code{conf.level} interval of the
#'   distribution. The red dashed line shows the mean and the blue dashed line the median.
#'   Non-finite values (e.g. an infinite steady state) and failed replicates are ignored.
#' @return \code{x}, invisibly
#' @seealso \code{\link{bootstrap_parameters}}, \code{\link{density}}
#'
#' @author James Weedon
#'
#' @examples
#' fit <- fit_litter(
#'   time = pineneedles$Year, mass.remaining = pineneedles$Mass.remaining,
#'   model = "weibull", iters = 100, quiet = TRUE
#' )
#' boot <- bootstrap_parameters(fit, nboot = 200)
#' plot(boot)
#' plot(boot, coef.index = "all")
#'
#' @export
plot.litfit_bootstrap <- function(x, coef.index = 1, bw = "nrd0", conf.level = 0.95, ...) {
  check_conf_level(conf.level)
  m <- unclass(x)
  attr(m, "model") <- NULL
  nms <- if (is.null(colnames(m))) paste("coefficient", seq_len(ncol(m))) else colnames(m)

  if (identical(coef.index, "all")) {
    enough <- colSums(is.finite(m)) >= 2
    if (!any(enough)) {
      stop("Not enough finite bootstrap values to plot any coefficient")
    }
    if (!all(enough)) {
      warning(paste0(
        "skipping column(s) without enough finite values: ",
        paste(nms[!enough], collapse = ", ")
      ))
    }
    cols <- which(enough)
  } else if (is.character(coef.index)) {
    cols <- match(coef.index, nms)
    if (anyNA(cols)) {
      stop(paste0(
        "unknown coefficient(s): ", paste(coef.index[is.na(cols)], collapse = ", "),
        "; available: ", paste(nms, collapse = ", ")
      ))
    }
  } else {
    if (!is.numeric(coef.index) || any(is.na(coef.index)) ||
      any(coef.index < 1 | coef.index > ncol(m))) {
      stop(paste("coef.index must be between 1 and", ncol(m), "or a column name"))
    }
    cols <- coef.index
  }

  if (length(cols) > 1) {
    op <- graphics::par(mfrow = grDevices::n2mfrow(length(cols)))
    on.exit(graphics::par(op), add = TRUE)
  }
  for (j in cols) {
    plot_boot_density(m[, j], nms[j], bw = bw, conf.level = conf.level, ...)
  }
  invisible(x)
}

plot_boot_density <- function(values, name, bw, conf.level, ...) {
  values <- values[is.finite(values)]
  if (length(values) < 2) {
    stop(paste0("Not enough finite bootstrap values to plot '", name, "'"))
  }
  dens <- stats::density(values, bw = bw)
  args <- merge_dots(list(...), list(
    main = paste("Bootstrap distribution:", name), xlab = name
  ))
  do.call(graphics::plot, c(list(x = dens), args))
  alpha <- (1 - conf.level) / 2
  qfs <- stats::quantile(values, probs = c(alpha, 1 - alpha))
  inside <- dens$x >= qfs[1] & dens$x <= qfs[2]
  if (sum(inside) >= 2) {
    polygon(
      x = c(dens$x[inside][1], dens$x[inside], utils::tail(dens$x[inside], 1)),
      y = c(0, dens$y[inside], 0),
      col = "gray"
    )
  }
  lines(dens)
  abline(v = mean(values), col = "red", lty = 2)
  abline(v = stats::median(values), col = "blue", lty = 2)
}
