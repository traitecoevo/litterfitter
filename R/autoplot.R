#' ggplot2 plots of litterfitter fits
#'
#' \code{autoplot()} methods that draw fits, model comparisons and bootstrap
#' distributions with \pkg{ggplot2}. They require ggplot2 to be installed and loaded
#' (\code{library(ggplot2)}); the result is a regular ggplot object that can be modified
#' with further layers, scales and themes.
#'
#' \itemize{
#'   \item \code{litfit}: the data, fitted curve and (optionally) a bootstrap band, with
#'     the fitted equation as the subtitle.
#'   \item \code{elementfit}: as above, with mass remaining decreasing from left to right
#'     and a dashed line at the initial element amount.
#'   \item \code{litfit_list}: the curves of all models, coloured with the Okabe-Ito
#'     palette and labelled with their difference in AICc from the best model ("delta AICc").
#'   \item \code{litfit_bootstrap}: the bootstrap distribution of each parameter and
#'     derived quantity in its own panel, with the central interval shaded and lines at
#'     the mean (red) and median (blue).
#' }
#'
#' @param object a \code{litfit}, \code{elementfit}, \code{litfit_list} or
#'   \code{litfit_bootstrap} object
#' @param boot optional \code{litfit_bootstrap} from \code{\link{bootstrap_parameters}}
#'   for the same fit (for a \code{litfit_list}, a named list of them), to draw an
#'   uncertainty band
#' @param conf.level confidence level for bands and shaded intervals (default 0.95)
#' @param equation if \code{TRUE}, show the fitted equation as the subtitle
#' @param n number of points used to draw curves
#' @param terms for bootstraps, the parameters or derived quantities to show (default all)
#' @param ... unused
#'
#' @return a \code{ggplot} object
#'
#' @seealso \code{\link{augment.litfit}} and \code{\link{tidy.litfit_bootstrap}}, which
#'   return the data behind these plots for building custom figures;
#'   \code{\link{plot.litfit}} for base graphics
#'
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' library(ggplot2)
#' fit <- fit_litter(
#'   time = pineneedles$Year, mass.remaining = pineneedles$Mass.remaining,
#'   model = "weibull", iters = 100, quiet = TRUE
#' )
#' boot <- bootstrap_parameters(fit, nboot = 200)
#' autoplot(fit, boot = boot)
#' autoplot(boot)
#'
#' fits <- fit_litter_all(pineneedles$Year, pineneedles$Mass.remaining,
#'   models = c("neg.exp", "weibull", "cont.quality"), iters = 100
#' )
#' autoplot(fits) + theme_bw()
#'
#' @name litterfitter_autoplot
NULL

check_n <- function(n) {
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 2) {
    stop("n must be a single number of at least 2")
  }
  as.integer(n)
}

#' @rdname litterfitter_autoplot
#' @exportS3Method ggplot2::autoplot
autoplot.litfit <- function(object, boot = NULL, conf.level = 0.95, equation = TRUE,
                            n = 200, ...) {
  n <- check_n(n)
  obs <- augment(object)
  grid <- augment(object,
    newdata = seq(0, max(object$time), length.out = n), boot = boot, conf.level = conf.level
  )
  p <- ggplot2::ggplot(grid, ggplot2::aes(x = time, y = .fitted))
  if (!is.null(boot)) {
    p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = .lower, ymax = .upper),
      fill = "grey70", alpha = 0.5
    )
  }
  p +
    ggplot2::geom_line() +
    ggplot2::geom_point(data = obs, ggplot2::aes(x = time, y = mass)) +
    ggplot2::labs(
      x = "Time", y = "Proportion mass remaining", title = object$model,
      subtitle = if (equation) model_equation(object, "text") else NULL
    )
}

#' @rdname litterfitter_autoplot
#' @exportS3Method ggplot2::autoplot
autoplot.elementfit <- function(object, boot = NULL, conf.level = 0.95, equation = TRUE,
                                n = 200, ...) {
  n <- check_n(n)
  obs <- augment(object)
  grid <- augment(object,
    newdata = seq(0, max(1, object$mass), length.out = n), boot = boot,
    conf.level = conf.level
  )
  p <- ggplot2::ggplot(grid, ggplot2::aes(x = mass, y = .fitted)) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50")
  if (!is.null(boot)) {
    p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = .lower, ymax = .upper),
      fill = "grey70", alpha = 0.5
    )
  }
  pk <- element_peak(object$model, coef(object))
  p +
    ggplot2::geom_line() +
    ggplot2::geom_point(data = obs, ggplot2::aes(x = mass, y = element)) +
    ggplot2::scale_x_reverse() +
    ggplot2::labs(
      x = "Proportion mass remaining", y = "Proportion element remaining",
      title = object$model,
      subtitle = if (equation) {
        sprintf(
          "%s\npeak element remaining %s at mass remaining %s",
          model_equation(object, "text"),
          fmt_num(pk$peak_element), fmt_num(pk$mass_at_peak)
        )
      }
    )
}

#' @rdname litterfitter_autoplot
#' @exportS3Method ggplot2::autoplot
autoplot.litfit_list <- function(object, boot = NULL, conf.level = 0.95, n = 200, ...) {
  n <- check_n(n)
  g <- glance(object)
  labels <- sprintf(
    "%s (delta AICc %s)", g$fit, formatC(g$delta_AICc, format = "f", digits = 1)
  )
  first <- object[[1]]
  curves <- augment(object,
    newdata = seq(0, max(first$time), length.out = n), boot = boot, conf.level = conf.level
  )
  curves$fit <- factor(curves$fit, levels = g$fit, labels = labels)
  # colours follow the order of the list, as in plot.litfit_list(), so a model keeps
  # its colour across both plotting systems; the legend is ordered by AICc
  by_fit <- stats::setNames(okabe_ito(length(object)), names(object))
  cols <- stats::setNames(by_fit[g$fit], labels)

  p <- ggplot2::ggplot(curves, ggplot2::aes(x = time, y = .fitted, colour = fit))
  if (!is.null(boot)) {
    p <- p +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .lower, ymax = .upper, fill = fit),
        alpha = 0.15, colour = NA
      ) +
      ggplot2::scale_fill_manual(values = cols, name = "Model")
  }
  p +
    ggplot2::geom_line() +
    ggplot2::geom_point(
      data = augment(first), ggplot2::aes(x = time, y = mass), inherit.aes = FALSE
    ) +
    ggplot2::scale_colour_manual(values = cols, name = "Model") +
    ggplot2::labs(x = "Time", y = "Proportion mass remaining")
}

#' @rdname litterfitter_autoplot
#' @exportS3Method ggplot2::autoplot
autoplot.litfit_bootstrap <- function(object, terms = NULL, conf.level = 0.95, ...) {
  check_conf_level(conf.level)
  td <- tidy(object)
  if (!is.null(terms)) {
    unknown <- setdiff(terms, unique(td$term))
    if (length(unknown)) {
      stop(paste0(
        "unknown term(s): ", paste(unknown, collapse = ", "),
        "; available: ", paste(unique(td$term), collapse = ", ")
      ))
    }
    td <- td[td$term %in% terms, , drop = FALSE]
  }
  td <- td[is.finite(td$value), , drop = FALSE]
  term_levels <- unique(td$term)
  alpha <- (1 - conf.level) / 2

  pieces <- lapply(term_levels, function(tm) {
    v <- td$value[td$term == tm]
    if (length(v) < 2) {
      return(NULL)
    }
    d <- stats::density(v)
    q <- stats::quantile(v, c(alpha, 1 - alpha), names = FALSE)
    list(
      dens = data.frame(term = tm, x = d$x, y = d$y, inside = d$x >= q[1] & d$x <= q[2]),
      stats = data.frame(term = tm, mean = mean(v), median = stats::median(v))
    )
  })
  pieces <- pieces[!vapply(pieces, is.null, logical(1))]
  if (!length(pieces)) {
    stop("Not enough finite bootstrap values to plot")
  }
  dens <- do.call(rbind, lapply(pieces, `[[`, "dens"))
  stats_df <- do.call(rbind, lapply(pieces, `[[`, "stats"))
  kept <- unique(dens$term)
  dens$term <- factor(dens$term, levels = kept)
  stats_df$term <- factor(stats_df$term, levels = kept)

  ggplot2::ggplot(dens, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_area(fill = "grey92") +
    ggplot2::geom_area(data = dens[dens$inside, ], fill = "grey65") +
    ggplot2::geom_line() +
    ggplot2::geom_vline(
      data = stats_df, ggplot2::aes(xintercept = mean),
      colour = "red", linetype = "dashed"
    ) +
    ggplot2::geom_vline(
      data = stats_df, ggplot2::aes(xintercept = median),
      colour = "blue", linetype = "dashed"
    ) +
    ggplot2::facet_wrap(~term, scales = "free") +
    ggplot2::labs(
      x = NULL, y = "Density", title = "Bootstrap distributions",
      subtitle = sprintf(
        "Shaded: central %s%%; red: mean; blue: median", 100 * conf.level
      )
    )
}
