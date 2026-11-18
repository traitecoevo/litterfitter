# Input validation and data-range handling in fit_litter()

test_that("model argument: default, partial matching, and invalid values", {
  set.seed(1)
  expect_equal(fit_litter(pineneedles$Year, pineneedles$Mass.remaining, iters = 10, quiet = TRUE)$model, "neg.exp")
  expect_equal(fit_litter(pineneedles$Year, pineneedles$Mass.remaining, "weib",
    iters = 10, quiet = TRUE
  )$model, "weibull")
  expect_error(fit_litter(0:3, c(1, .5, .3, .2), "mean", iters = 10), "should be one of")
  expect_error(fit_litter(0:3, c(1, .5, .3, .2), c("neg.exp", "weibull"), iters = 10), "length 1")
})

test_that("length, type and finiteness are checked", {
  expect_error(fit_litter(0:4, ex_mass, "weibull", iters = 10), "same length")
  expect_error(fit_litter(as.character(0:3), c(1, .5, .3, .2), iters = 10), "numeric")
  expect_error(fit_litter(0:3, c("1", ".5", ".3", ".2"), iters = 10), "numeric")
  expect_error(fit_litter(c(0:2, Inf), c(1, .5, .3, .2), iters = 10), "infinite")
  expect_error(fit_litter(0:3, c(1, .5, .3, -Inf), iters = 10), "infinite")
})

test_that("time values are checked", {
  expect_error(fit_litter(c(0, -1, 2, 3, 4, 5, 6), ex_mass, "weibull", iters = 10), "negative")
  expect_error(fit_litter(rep(1, 5), c(1, .8, .6, .5, .4), "weibull", iters = 10), "distinct time")
})

test_that("mass values are checked", {
  # percentages instead of proportions
  expect_error(fit_litter(0:3, c(100, 80, 50, 20), iters = 10), "proportional mass")
  expect_error(fit_litter(ex_time, c(1000, ex_mass[-1]), "weibull", iters = 10), "proportional mass")
  # clearly negative
  expect_error(fit_litter(0:3, c(1, 0.5, -0.2, -0.5), iters = 10), "proportional mass")
  # exactly 2 is the largest accepted value
  expect_warning(
    fit_litter(0:4, c(2, 1, .5, .2, .1), iters = 10, quiet = TRUE),
    class = "litterfitter_mass_above_one"
  )
})

test_that("values above 1 give an informative, classed warning (issue #21)", {
  set.seed(1)
  # some mass gain early on, but clear decay overall
  expect_warning(
    fit_litter(0:5, c(1, 1.02, 0.8, 0.5, 0.3, 0.1), iters = 20, quiet = TRUE),
    "1 of 6 mass remaining values are above 1 \\(max 1.02\\)",
    class = "litterfitter_mass_above_one"
  )
  w <- testthat::capture_warnings(
    fit_litter(0:5, c(1, 1.02, 0.8, 0.5, 0.3, 0.1), iters = 20, quiet = TRUE)
  )
  expect_false(any(grepl("little mass loss", w)))

  # slow decomposition: values above 1 and little overall loss
  slow_t <- c(0, 0.25, 0.5, 1, 1.5, 2)
  slow_m <- c(1, 1.03, 1.01, 0.99, 1.02, 0.97)
  w <- testthat::capture_warnings(fit_litter(slow_t, slow_m, "neg.exp", iters = 20, quiet = TRUE))
  expect_match(w, "3 of 6 mass remaining values are above 1", all = FALSE)
  expect_match(w, "little mass loss \\(minimum 0.97\\)", all = FALSE)

  # the warning can be silenced by class once checked, leaving the fit intact
  fit <- suppressWarnings(
    fit_litter(slow_t, slow_m, "neg.exp", iters = 20, quiet = TRUE),
    classes = "litterfitter_mass_above_one"
  )
  expect_s3_class(fit, "litfit")
  expect_gt(unname(coef(fit)), 0)
})

test_that("values above 1 are kept, not dropped or truncated", {
  set.seed(1)
  m <- c(1, 1.05, 0.8, 0.5, 0.3, 0.1)
  fit <- quiet_fit(0:5, m, "neg.exp", iters = 20)
  expect_equal(fit$mass, m)
  expect_equal(nobs(fit), 6)
})

test_that("slightly negative values give a classed warning", {
  expect_warning(
    fit_litter(0:4, c(1, 0.5, 0.2, 0.05, -0.01), "neg.exp", iters = 10, quiet = TRUE),
    "1 of 5 mass remaining values are below 0",
    class = "litterfitter_mass_below_zero"
  )
})

test_that("values exactly 0 and 1 do not trigger range warnings", {
  set.seed(1)
  expect_no_warning(
    fit_litter(0:5, c(1, 0.7, 0.4, 0.2, 0.05, 0), "neg.exp", iters = 20, quiet = TRUE),
    class = "litterfitter_mass_above_one"
  )
  expect_no_warning(
    fit_litter(0:5, c(1, 0.7, 0.4, 0.2, 0.05, 0), "neg.exp", iters = 20, quiet = TRUE),
    class = "litterfitter_mass_below_zero"
  )
})

test_that("missing values are removed, and too few remaining is an error", {
  set.seed(1)
  expect_warning(
    fit <- fit_litter(c(pineneedles$Year, NA, 3), c(pineneedles$Mass.remaining, 0.5, NA),
      "neg.exp",
      iters = 20, quiet = TRUE
    ),
    "2 observation"
  )
  expect_equal(nobs(fit), length(pineneedles$Year))
  expect_error(
    suppressWarnings(fit_litter(rep(NA_real_, 3), c(1, 0.5, 0.2), iters = 10)),
    "Too few observations"
  )
  expect_error(
    suppressWarnings(fit_litter(c(0, 1, NA, NA), c(1, 0.5, 0.2, 0.1), "weibull", iters = 10)),
    "Too few observations"
  )
})

test_that("number of observations must exceed number of parameters", {
  expect_error(fit_litter(0:2, c(1, 0.5, 0.3), "discrete.parallel", iters = 10), "Too few")
  expect_error(fit_litter(0:1, c(1, 0.5), "weibull", iters = 10), "Too few")
  expect_error(fit_litter(0, 1, "neg.exp", iters = 10), "Too few")
  set.seed(1)
  # exactly one more observation than parameters is allowed
  expect_s3_class(quiet_fit(0:1, c(1, 0.5), "neg.exp", iters = 10), "litfit")
})

test_that("curves pinned at the origin can be fitted from a single harvest", {
  reset <- function() {
    if (exists("origin_only", envir = litterfitter:::warned_once)) {
      rm("origin_only", envir = litterfitter:::warned_once)
    }
  }
  reset()
  on.exit(reset())
  set.seed(1)
  # one observation: neg.exp passes exactly through it
  expect_warning(
    fit <- fit_litter(0.5, 0.7, "neg.exp", iters = 10, quiet = TRUE),
    class = "litterfitter_origin_only"
  )
  expect_equal(unname(coef(fit)), -log(0.7) / 0.5, tolerance = 1e-6)
  expect_true(is.na(fit$fitAICc))
  # the warning is shown once per session
  expect_no_warning(
    rep_fit <- fit_litter(c(1, 1, 1), c(0.5, 0.6, 0.55), "neg.exp", iters = 10, quiet = TRUE)
  )
  expect_true(is.finite(rep_fit$fitAICc))
  expect_equal(nobs(rep_fit), 3)
  # Weibull needs two distinct harvest times
  expect_s3_class(suppressWarnings(fit_litter(c(0.5, 1), c(0.7, 0.5), "weibull", iters = 10,
                                              quiet = TRUE)), "litfit")
  expect_error(fit_litter(c(1, 1, 1), c(0.5, 0.4, 0.45), "weibull", iters = 10),
               "distinct time points")
  # neg.exp.limit is not pinned at the origin, so the old rules apply
  expect_error(fit_litter(rep(1, 4), c(0.5, 0.4, 0.5, 0.4), "neg.exp.limit", iters = 10),
               "distinct time points")
  # data only at time 0 cannot identify anything
  expect_error(fit_litter(c(0, 0), c(1, 1), "neg.exp", iters = 10), "distinct time points")
})

test_that("fit_litter_all reports the single-harvest warning once", {
  if (exists("origin_only", envir = litterfitter:::warned_once)) {
    rm("origin_only", envir = litterfitter:::warned_once)
  }
  set.seed(1)
  w <- testthat::capture_warnings(
    fits <- fit_litter_all(c(1, 1), c(0.5, 0.6), models = c("neg.exp", "cont.quality"), iters = 10)
  )
  expect_equal(sum(grepl("relying on the curve starting at mass 1", w)), 1)
  expect_s3_class(fits, "litfit_list")
})

test_that("iters is validated", {
  expect_error(fit_litter(0:3, c(1, .5, .3, .2), iters = 0), "iters")
  expect_error(fit_litter(0:3, c(1, .5, .3, .2), iters = -5), "iters")
  expect_error(fit_litter(0:3, c(1, .5, .3, .2), iters = NA), "iters")
  expect_error(fit_litter(0:3, c(1, .5, .3, .2), iters = "10"), "iters")
  expect_error(fit_litter(0:3, c(1, .5, .3, .2), iters = c(5, 10)), "iters")
  set.seed(1)
  # a single start and fewer than 5 starts both work
  expect_s3_class(quiet_fit(pineneedles$Year, pineneedles$Mass.remaining, "neg.exp", iters = 1), "litfit")
  expect_s3_class(quiet_fit(pineneedles$Year, pineneedles$Mass.remaining, "neg.exp", iters = 3), "litfit")
})

test_that("user-supplied bounds are checked and respected", {
  expect_error(fit_litter(0:4, c(1, .9, 1, .4, .6), "weibull", iters = 10, upper = 2), "upper bounds")
  expect_error(fit_litter(0:4, c(1, .9, 1, .4, .6), "weibull", iters = 10, lower = 2), "lower bounds")
  expect_error(
    fit_litter(0:4, c(1, .8, .6, .4, .3), "weibull", iters = 10, lower = c(5, 5), upper = c(1, 1)),
    "smaller than"
  )
  set.seed(1)
  fit <- quiet_fit(pineneedles$Year, pineneedles$Mass.remaining, "weibull",
    iters = 20, lower = c(1, 1), upper = c(3, 1.5)
  )
  expect_true(all(coef(fit) >= c(1, 1) & coef(fit) <= c(3, 1.5)))
})

test_that("parameters on a bound produce a warning naming them", {
  set.seed(456)
  w <- testthat::capture_warnings(
    fit_litter(pineneedles$Year, pineneedles$Mass.remaining, "discrete.series", iters = 200, quiet = TRUE)
  )
  expect_match(w, "boundary \\(R", all = FALSE)
  expect_false(any(grepl("measured in years", w)))
})

test_that("boundary warnings hint at time units when time looks like days", {
  set.seed(1)
  # Weibull scale in days (~1300) is far above the default upper bound of 10
  w <- testthat::capture_warnings(
    fit_litter(pineneedles$Year * 365, pineneedles$Mass.remaining, "weibull", iters = 50, quiet = TRUE)
  )
  expect_match(w, "measured in years", all = FALSE)
  # no hint when the user supplied their own bounds
  w <- testthat::capture_warnings(
    fit_litter(pineneedles$Year * 365, pineneedles$Mass.remaining, "weibull",
      iters = 50, quiet = TRUE, upper = c(10, 10), lower = c(1e-4, 1e-4)
    )
  )
  expect_false(any(grepl("measured in years", w)))
})

test_that("user-supplied optim control is respected", {
  set.seed(1)
  fit <- quiet_fit(pineneedles$Year, pineneedles$Mass.remaining, "neg.exp",
    iters = 5, control = list(ndeps = 1e-3, maxit = 50)
  )
  expect_s3_class(fit, "litfit")
})

test_that("quiet controls the progress message", {
  set.seed(1)
  expect_message(
    fit_litter(pineneedles$Year, pineneedles$Mass.remaining, "neg.exp", iters = 5),
    "successful fits"
  )
  expect_no_message(
    fit_litter(pineneedles$Year, pineneedles$Mass.remaining, "neg.exp", iters = 5, quiet = TRUE)
  )
})
