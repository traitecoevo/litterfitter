# fit_litter_all(), as_litfit_list() and litfit_list methods

pine_all <- function(models = all_models, iters = 50, seed = 1) {
  set.seed(seed)
  suppressWarnings(fit_litter_all(pineneedles$Year, pineneedles$Mass.remaining,
    models = models, iters = iters
  ))
}

# ---- fit_litter_all ----

test_that("fit_litter_all() fits all six models by default, named by model", {
  fits <- pine_all()
  expect_s3_class(fits, "litfit_list")
  expect_named(fits, all_models)
  expect_true(all(vapply(fits, inherits, logical(1), what = "litfit")))
  expect_equal(unname(vapply(fits, function(f) f$model, character(1))), all_models)
})

test_that("fit_litter_all() accepts a subset, partial names, and drops duplicates", {
  fits <- pine_all(c("weib", "neg.exp", "neg.exp"))
  expect_named(fits, c("weibull", "neg.exp"))
  expect_error(fit_litter_all(0:3, c(1, .5, .3, .2), models = "nope"), "should be one of")
})

test_that("fit_litter_all() is quiet and gives each data warning once", {
  set.seed(1)
  expect_no_message(suppressWarnings(
    fit_litter_all(pineneedles$Year, pineneedles$Mass.remaining, c("neg.exp", "weibull"), iters = 10)
  ))
  w <- testthat::capture_warnings(
    fit_litter_all(ex_time, ex_mass, c("neg.exp", "weibull", "cont.quality"), iters = 30)
  )
  expect_equal(sum(grepl("above 1", w)), 1)
  # data warnings keep their class
  expect_warning(
    fit_litter_all(ex_time, ex_mass, "neg.exp", iters = 10),
    class = "litterfitter_mass_above_one"
  )
  w <- testthat::capture_warnings(
    fit_litter_all(c(pineneedles$Year, NA), c(pineneedles$Mass.remaining, 0.5),
      c("neg.exp", "weibull"),
      iters = 10
    )
  )
  expect_equal(sum(grepl("missing", w)), 1)
})

test_that("model-specific warnings are prefixed with the model name", {
  set.seed(456)
  w <- testthat::capture_warnings(
    fit_litter_all(pineneedles$Year, pineneedles$Mass.remaining, c("neg.exp", "discrete.series"), iters = 200)
  )
  expect_match(w, "^discrete.series: one or more parameters fit on the boundary", all = FALSE)
})

test_that("models that cannot be fitted are dropped with a warning", {
  set.seed(1)
  # three observations: the three-parameter models cannot be fitted
  w <- testthat::capture_warnings(fits <- fit_litter_all(0:2, c(1, 0.6, 0.3), iters = 20))
  for (m in c("discrete.parallel", "discrete.series", "neg.exp.limit")) {
    expect_match(w, paste(m, "could not be fitted and was dropped: Too few"), all = FALSE)
    expect_false(m %in% names(fits))
  }
  expect_true(all(c("neg.exp", "weibull") %in% names(fits)))
})

test_that("fit_litter_all() errors if no model can be fitted", {
  expect_error(fit_litter_all(c(0, -1, 2, 3), c(1, .8, .5, .3), iters = 5), "negative values")
  expect_error(fit_litter_all(0:1, c(1, 0.5), models = c("weibull", "discrete.series"), iters = 5), "Too few")
})

test_that("fit_litter_all() passes arguments on to fit_litter()", {
  set.seed(1)
  fits <- suppressWarnings(fit_litter_all(pineneedles$Year, pineneedles$Mass.remaining,
    models = "weibull", iters = 20, lower = c(1, 1), upper = c(3, 1.5)
  ))
  expect_true(all(coef(fits$weibull) >= c(1, 1) & coef(fits$weibull) <= c(3, 1.5)))
  # bounds of the wrong length for some models drop those models
  expect_warning(
    fits <- fit_litter_all(pineneedles$Year, pineneedles$Mass.remaining,
      models = c("neg.exp", "weibull"), iters = 10, upper = c(10, 10), lower = c(1e-4, 1e-4)
    ),
    "neg.exp could not be fitted"
  )
  expect_named(fits, "weibull")
})

# ---- as_litfit_list ----

test_that("as_litfit_list() combines fits given separately or as a list", {
  fits <- pine_all(c("neg.exp", "weibull"))
  l1 <- as_litfit_list(fits$neg.exp, fits$weibull)
  l2 <- as_litfit_list(list(fits$neg.exp, fits$weibull))
  expect_s3_class(l1, "litfit_list")
  expect_named(l1, c("neg.exp", "weibull"))
  expect_equal(l1, l2)
  # explicit names are kept; repeated models get unique names
  l3 <- as_litfit_list(a = fits$neg.exp, fits$neg.exp, fits$neg.exp)
  expect_named(l3, c("a", "neg.exp", "neg.exp.1"))
  expect_named(as_litfit_list(fits$weibull, fits$weibull), c("weibull", "weibull.1"))
})

test_that("as_litfit_list() rejects bad input", {
  fits <- pine_all("neg.exp")
  expect_error(as_litfit_list(), "at least one")
  expect_error(as_litfit_list(list()), "at least one")
  expect_error(as_litfit_list(fits$neg.exp, list(a = 1)), "litfit")
  expect_error(as_litfit_list(1, 2), "litfit")
})

test_that("as_litfit_list() warns when fits are to different data", {
  set.seed(1)
  f1 <- quiet_fit(pineneedles$Year, pineneedles$Mass.remaining, "neg.exp", iters = 10)
  f2 <- quiet_fit(ex_time, ex_mass, "neg.exp", iters = 10)
  expect_warning(as_litfit_list(f1, f2), "different data")
  expect_no_warning(as_litfit_list(f1, f1))
})

# ---- methods ----

test_that("glance() on a litfit_list is a comparison table sorted by AICc", {
  fits <- pine_all()
  g <- glance(fits)
  expect_equal(nrow(g), 6)
  expect_equal(names(g)[1], "fit")
  expect_true("delta_AICc" %in% names(g))
  expect_equal(g$AICc, sort(g$AICc))
  expect_equal(min(g$delta_AICc), 0)
  expect_equal(g$delta_AICc, g$AICc - min(g$AICc))
  expect_setequal(g$fit, all_models)
  expect_equal(rownames(g), as.character(1:6))
})

test_that("tidy() on a litfit_list stacks parameters with fit and model columns", {
  fits <- pine_all()
  td <- tidy(fits)
  expect_named(td, c("fit", "model", "term", "estimate"))
  expect_equal(nrow(td), sum(lengths(expected_param_names)))
  expect_equal(td$term[td$fit == "weibull"], c("beta", "alpha"))
})

test_that("augment() on a litfit_list stacks predictions", {
  fits <- pine_all(c("neg.exp", "weibull"))
  a <- augment(fits)
  expect_named(a, c("fit", "model", "time", "mass", ".fitted", ".resid"))
  expect_equal(nrow(a), 2 * length(pineneedles$Year))
  a <- augment(fits, newdata = seq(0, 5, 1))
  expect_named(a, c("fit", "model", "time", ".fitted"))
  expect_equal(nrow(a), 12)
})

test_that("augment() on a litfit_list accepts a named list of bootstraps", {
  fits <- pine_all(c("neg.exp", "weibull"))
  set.seed(2)
  boots <- lapply(fits, function(f) suppressWarnings(bootstrap_parameters(f, nboot = 20)))
  a <- augment(fits, newdata = c(1, 2), boot = boots)
  expect_true(all(c(".lower", ".upper") %in% names(a)))
  expect_equal(nrow(a), 4)
  # extra bootstraps are ignored; order does not matter
  expect_equal(augment(fits, newdata = c(1, 2), boot = rev(boots)), a)
  expect_error(augment(fits, boot = boots[[1]]), "named list")
  expect_error(augment(fits, boot = unname(boots)), "named list")
  expect_error(augment(fits, boot = boots["weibull"]), "named list")
  # bootstraps paired with the wrong fits are rejected
  swapped <- setNames(boots, rev(names(boots)))
  expect_error(augment(fits, boot = swapped), "generated from")
})

test_that("coef(), [ and print() work on a litfit_list", {
  fits <- pine_all(c("neg.exp", "weibull", "cont.quality"))
  cf <- coef(fits)
  expect_type(cf, "list")
  expect_named(cf, names(fits))
  expect_named(cf$weibull, c("beta", "alpha"))

  sub <- fits[c("weibull", "neg.exp")]
  expect_s3_class(sub, "litfit_list")
  expect_named(sub, c("weibull", "neg.exp"))
  expect_s3_class(fits[2], "litfit_list")
  expect_s3_class(fits[[1]], "litfit")
  expect_error(fits["nope"], "not in the litfit_list")
  expect_error(fits[10], "not in the litfit_list")

  expect_output(print(fits), "litfit_list: 3 models fit to 6 observations")
  expect_output(print(fits[1]), "1 model fit")
  out <- capture.output(r <- withVisible(print(fits)))
  expect_false(r$visible)
  # the fit column is shown only when names differ from models
  expect_false(any(grepl("^ *fit ", out)))
  named <- as_litfit_list(best = fits$neg.exp, other = fits$weibull)
  expect_true(any(grepl("fit", capture.output(print(named))[3])))
})
