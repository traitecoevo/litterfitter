#' Fit several decomposition models to the same data
#'
#' Fits each of the requested models with \code{\link{fit_litter}} and returns them
#' together as a \code{litfit_list}, ready for model comparison with
#' \code{\link[=glance.litfit_list]{glance}}, \code{\link[=tidy.litfit_list]{tidy}} or
#' \code{\link[=augment.litfit_list]{augment}}.
#'
#' Warnings about the data (for example values above 1, or missing values removed) are
#' given once rather than once per model. Warnings specific to one model, such as
#' parameters on a boundary, are prefixed with the model name. A model that cannot be
#' fitted (for example, too few observations for its number of parameters) is dropped with
#' a warning; an error is raised only if no model can be fitted.
#'
#' @param time time since decomposition began
#' @param mass.remaining proportional mass remaining
#' @param models models to fit (see \code{\link{fit_litter}}); defaults to all six
#' @param iters number of random starts for each model
#' @param ... further arguments passed to \code{\link{fit_litter}}, such as \code{upper},
#'   \code{lower} or optim \code{control}
#'
#' @return A \code{litfit_list}: a named list of \code{litfit} objects.
#'
#' @seealso \code{\link{as_litfit_list}} to combine existing fits,
#'   \code{\link{litfit_list_methods}}
#'
#' @examples
#' fits <- fit_litter_all(
#'   time = pineneedles$Year, mass.remaining = pineneedles$Mass.remaining,
#'   models = c("neg.exp", "weibull", "cont.quality"), iters = 100
#' )
#' fits
#' glance(fits)
#'
#' @export
fit_litter_all <- function(time, mass.remaining,
                           models = c(
                             "neg.exp", "weibull", "discrete.parallel",
                             "discrete.series", "cont.quality", "neg.exp.limit"
                           ),
                           iters = 500, ...) {
  models <- unique(match.arg(models, several.ok = TRUE))

  data_warning_classes <- c(
    "litterfitter_mass_above_one", "litterfitter_mass_below_zero",
    "litterfitter_missing_removed", "litterfitter_origin_only"
  )
  data_warnings <- list()
  model_warnings <- list()
  errors <- list()

  fits <- lapply(models, function(m) {
    withCallingHandlers(
      tryCatch(
        fit_litter(time, mass.remaining, model = m, iters = iters, quiet = TRUE, ...),
        error = function(e) {
          errors[[m]] <<- conditionMessage(e)
          NULL
        }
      ),
      warning = function(w) {
        if (inherits(w, data_warning_classes)) {
          data_warnings[[conditionMessage(w)]] <<- w
        } else {
          model_warnings[[length(model_warnings) + 1]] <<- paste0(m, ": ", conditionMessage(w))
        }
        invokeRestart("muffleWarning")
      }
    )
  })
  names(fits) <- models

  if (length(errors) == length(models)) {
    msgs <- unique(unlist(errors))
    stop(if (length(msgs) == 1) msgs else paste(names(errors), unlist(errors), sep = ": ", collapse = "\n"))
  }

  for (w in data_warnings) warning(w)
  for (msg in model_warnings) warning(msg, call. = FALSE)
  for (m in names(errors)) {
    warning(paste0(m, " could not be fitted and was dropped: ", errors[[m]]), call. = FALSE)
  }

  structure(fits[!vapply(fits, is.null, logical(1))], class = "litfit_list")
}


#' Combine existing fits into a litfit_list
#'
#' @param ... \code{litfit} objects, or a single list of them. Names are used as fit
#'   names; unnamed fits are named by their model (made unique if needed).
#'
#' @details Information criteria are only comparable between fits to the same data, so a
#'   warning is given if the fits were made to different data.
#'
#' @return A \code{litfit_list}.
#'
#' @examples
#' f1 <- fit_litter(pineneedles$Year, pineneedles$Mass.remaining, "neg.exp",
#'   iters = 50, quiet = TRUE
#' )
#' f2 <- fit_litter(pineneedles$Year, pineneedles$Mass.remaining, "weibull",
#'   iters = 50, quiet = TRUE
#' )
#' fits <- as_litfit_list(f1, f2)
#' glance(fits)
#'
#' @export
as_litfit_list <- function(...) {
  fits <- list(...)
  if (length(fits) == 1 && is.list(fits[[1]]) && !inherits(fits[[1]], "litfit")) {
    fits <- fits[[1]]
  }
  if (length(fits) == 0) {
    stop("as_litfit_list() needs at least one litfit object")
  }
  if (!all(vapply(fits, inherits, logical(1), what = "litfit"))) {
    stop("all elements must be 'litfit' objects from fit_litter()")
  }
  nm <- names(fits)
  if (is.null(nm)) nm <- rep("", length(fits))
  model_names <- vapply(fits, function(f) f$model, character(1))
  nm[nm == ""] <- model_names[nm == ""]
  names(fits) <- make.unique(nm)

  same_data <- vapply(fits, function(f) {
    identical(f$time, fits[[1]]$time) && identical(f$mass, fits[[1]]$mass)
  }, logical(1))
  if (!all(same_data)) {
    warning("the fits were made to different data, so their AIC/BIC values are not comparable")
  }
  structure(fits, class = "litfit_list")
}


#' Methods for litfit_list objects
#'
#' Model comparison tools for several fits to the same data, as returned by
#' \code{\link{fit_litter_all}} or \code{\link{as_litfit_list}}.
#'
#' \itemize{
#'   \item \code{glance()}: one row per fit, sorted by AICc, with \code{delta_AICc}
#'     (difference from the best model).
#'   \item \code{tidy()}: parameter estimates for every fit.
#'   \item \code{augment()}: fitted values (or predictions at \code{newdata}) for every fit,
#'     stacked, for plotting.
#' }
#' Every table starts with a \code{fit} column holding the names of the list.
#'
#' @param x a \code{litfit_list}
#' @param newdata optional times at which to predict (see \code{\link{augment.litfit}})
#' @param boot optional named list of \code{litfit_bootstrap} objects, one per fit (names
#'   matching the fits), to add uncertainty bands
#' @param conf.level confidence level for bands (default 0.95)
#' @param i indices or names of fits to keep
#' @param object a \code{litfit_list}
#' @param ... unused
#'
#' @return \code{glance()}, \code{tidy()} and \code{augment()} return a \code{data.frame};
#'   \code{coef()} returns a named list of parameter vectors; \code{[} returns a
#'   \code{litfit_list}.
#'
#' @examples
#' fits <- fit_litter_all(
#'   time = pineneedles$Year, mass.remaining = pineneedles$Mass.remaining,
#'   models = c("neg.exp", "weibull"), iters = 100
#' )
#' glance(fits)
#' tidy(fits)
#' head(augment(fits, newdata = seq(0, 5, by = 0.5)))
#' coef(fits)
#' fits["weibull"]
#'
#' @name litfit_list_methods
NULL

#' @rdname litfit_list_methods
#' @export
print.litfit_list <- function(x, ...) {
  g <- glance(x)
  cat(sprintf(
    "litfit_list: %d model%s fit to %d observations\n\n",
    length(x), if (length(x) == 1) "" else "s", g$nobs[1]
  ))
  cols <- c("fit", "model", "nparams", "logLik", "AICc", "delta_AICc")
  if (identical(g$fit, g$model)) cols <- setdiff(cols, "fit")
  shown <- g[, cols]
  num <- vapply(shown, is.numeric, logical(1))
  shown[num] <- lapply(shown[num], function(v) round(v, 3))
  print(shown, row.names = FALSE)
  invisible(x)
}

#' @rdname litfit_list_methods
#' @export
glance.litfit_list <- function(x, ...) {
  out <- do.call(rbind, lapply(names(x), function(n) {
    cbind(fit = n, glance(x[[n]]), stringsAsFactors = FALSE)
  }))
  out$delta_AICc <- out$AICc - min(out$AICc)
  out <- out[order(out$AICc), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' @rdname litfit_list_methods
#' @export
tidy.litfit_list <- function(x, ...) {
  out <- do.call(rbind, lapply(names(x), function(n) {
    cbind(fit = n, model = x[[n]]$model, tidy(x[[n]]), stringsAsFactors = FALSE)
  }))
  rownames(out) <- NULL
  out
}

#' @rdname litfit_list_methods
#' @export
augment.litfit_list <- function(x, newdata = NULL, boot = NULL, conf.level = 0.95, ...) {
  if (!is.null(boot)) {
    if (!is.list(boot) || inherits(boot, "litfit_bootstrap") || is.null(names(boot)) ||
      !all(names(x) %in% names(boot))) {
      stop("boot must be a named list of litfit_bootstrap objects, one for each fit in x")
    }
  }
  out <- do.call(rbind, lapply(names(x), function(n) {
    a <- augment(x[[n]], newdata = newdata, boot = boot[[n]], conf.level = conf.level)
    cbind(fit = n, model = x[[n]]$model, a, stringsAsFactors = FALSE)
  }))
  rownames(out) <- NULL
  out
}

#' @rdname litfit_list_methods
#' @export
coef.litfit_list <- function(object, ...) {
  lapply(unclass(object), coef)
}

#' @rdname litfit_list_methods
#' @export
`[.litfit_list` <- function(x, i) {
  out <- unclass(x)[i]
  if (!all(vapply(out, inherits, logical(1), what = "litfit"))) {
    stop("subscript selects fits that are not in the litfit_list")
  }
  structure(out, class = "litfit_list")
}
