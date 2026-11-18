# tidy(), glance() and augment() for litfit and litfit_bootstrap objects

fit_and_boot <- function(model = "weibull", nboot = 60, seed = 1) {
  set.seed(seed)
  fit <- quiet_fit(pineneedles$Year, pineneedles$Mass.remaining, model, iters = 100)
  boot <- suppressWarnings(bootstrap_parameters(fit, nboot = nboot))
  list(fit = fit, boot = boot)
}

test_that("generics are re-exported", {
  expect_identical(tidy, generics::tidy)
  expect_identical(glance, generics::glance)
  expect_identical(augment, generics::augment)
})

# ---- tidy.litfit ----

test_that("tidy() returns one row per parameter for every model", {
  set.seed(1)
  for (m in all_models) {
    fit <- quiet_fit(pineneedles$Year, pineneedles$Mass.remaining, m, iters = 50)
    td <- tidy(fit)
    expect_s3_class(td, "data.frame")
    expect_named(td, c("term", "estimate"))
    expect_type(td$term, "character")
    expect_equal(td$term, expected_param_names[[m]], info = m)
    expect_equal(td$estimate, unname(coef(fit)))
    expect_equal(rownames(td), as.character(seq_len(nrow(td))))
  }
})

test_that("tidy() with boot adds standard errors and intervals", {
  fb <- fit_and_boot()
  td <- tidy(fb$fit, boot = fb$boot)
  expect_named(td, c("term", "estimate", "std.error", "conf.low", "conf.high"))
  expect_true(all(td$std.error > 0))
  expect_true(all(td$conf.low <= td$conf.high))
  # a wider confidence level gives wider intervals
  td50 <- tidy(fb$fit, boot = fb$boot, conf.level = 0.5)
  expect_true(all(td$conf.low <= td50$conf.low & td$conf.high >= td50$conf.high))
  # intervals match quantiles of the bootstrap replicates
  expect_equal(td$conf.low[1], unname(quantile(fb$boot[, 1], 0.025, na.rm = TRUE)))
})

test_that("tidy() rejects bad conf.level and mismatched boot", {
  fb <- fit_and_boot()
  for (bad in list(0, 1, -0.5, 1.5, NA_real_, "0.9", c(0.9, 0.95))) {
    expect_error(tidy(fb$fit, boot = fb$boot, conf.level = bad), "conf.level")
  }
  other <- fit_and_boot("neg.exp")
  expect_error(tidy(fb$fit, boot = other$boot), "generated from a 'neg.exp' fit")
  expect_error(tidy(fb$fit, boot = unclass(fb$boot)), "litfit_bootstrap")
  expect_error(tidy(fb$fit, boot = list(a = 1)), "litfit_bootstrap")
})

test_that("boot without a model attribute is checked by its number of columns", {
  fb <- fit_and_boot()
  b <- fb$boot
  attr(b, "model") <- NULL
  expect_named(tidy(fb$fit, boot = b), c("term", "estimate", "std.error", "conf.low", "conf.high"))
  other <- fit_and_boot("neg.exp")
  b2 <- other$boot
  attr(b2, "model") <- NULL
  expect_error(tidy(fb$fit, boot = b2), "right parameters")
})

test_that("boot with too few successful replicates is an error", {
  set.seed(1)
  fit <- quiet_fit(pineneedles$Year, pineneedles$Mass.remaining, "neg.exp", iters = 20)
  failed <- suppressWarnings(bootstrap_parameters(fit, nboot = 3, method = "BFGS"))
  expect_error(tidy(fit, boot = failed), "fewer than 2")
  expect_error(augment(fit, boot = failed), "fewer than 2")
})

# ---- glance.litfit ----

test_that("glance() gives a one-row summary consistent with other methods", {
  set.seed(1)
  for (m in all_models) {
    fit <- quiet_fit(pineneedles$Year, pineneedles$Mass.remaining, m, iters = 50)
    g <- glance(fit)
    expect_s3_class(g, "data.frame")
    expect_equal(nrow(g), 1)
    expect_named(g, c(
      "model", "nobs", "nparams", "sigma", "logLik", "AIC", "AICc", "BIC",
      "converged", "time_to_50", "steady_state"
    ))
    expect_equal(g$model, m)
    expect_equal(g$nobs, nobs(fit))
    expect_equal(g$nparams, length(expected_param_names[[m]]))
    expect_equal(g$logLik, as.numeric(logLik(fit)))
    expect_equal(g$AIC, AIC(fit))
    expect_equal(g$BIC, BIC(fit))
    expect_equal(g$AICc, fit$fitAICc)
    expect_equal(g$sigma^2 * g$nobs, sum(residuals(fit)^2), tolerance = 1e-8)
    expect_true(g$converged)
    expect_equal(g$steady_state, unname(steady_state(fit)))
  }
})

test_that("glance() time_to_50 matches time_to_prop_mass_remaining()", {
  f <- fake_fit("neg.exp", 0.4)
  expect_equal(glance(f)$time_to_50, log(2) / 0.4, tolerance = 1e-6)
})

test_that("glance() handles unreachable thresholds, infinite steady state and missing convergence", {
  f <- fake_fit("neg.exp.limit", c(1, 0.3, 0.7))
  expect_silent(g <- glance(f))
  expect_true(is.na(g$time_to_50))
  expect_equal(g$steady_state, Inf)
  # fake fits have no convergence code
  expect_false(g$converged)
})

# ---- augment.litfit ----

test_that("augment() without newdata returns data, fitted values and residuals", {
  set.seed(1)
  for (m in all_models) {
    fit <- quiet_fit(pineneedles$Year, pineneedles$Mass.remaining, m, iters = 50)
    a <- augment(fit)
    expect_named(a, c("time", "mass", ".fitted", ".resid"))
    expect_equal(nrow(a), nobs(fit))
    expect_equal(a$time, fit$time)
    expect_equal(a$mass, fit$mass)
    expect_equal(a$.fitted, unname(fitted(fit)))
    expect_equal(a$.resid, unname(residuals(fit)))
  }
})

test_that("augment() predicts at new times given as a vector or data frame", {
  f <- fake_fit("neg.exp", 0.4)
  a <- augment(f, newdata = c(0, 1, 10))
  expect_named(a, c("time", ".fitted"))
  expect_equal(a$.fitted, exp(-0.4 * c(0, 1, 10)))

  nd <- data.frame(time = c(2, 3), site = c("a", "b"))
  a <- augment(f, newdata = nd)
  expect_named(a, c("time", "site", ".fitted"))
  expect_equal(a$site, c("a", "b"))
  expect_equal(a$.fitted, exp(-0.4 * c(2, 3)))

  # single time point, missing time, and no time points
  expect_equal(nrow(augment(f, newdata = 5)), 1)
  expect_true(is.na(augment(f, newdata = c(1, NA))$.fitted[2]))
  expect_equal(nrow(augment(f, newdata = numeric(0))), 0)
})

test_that("augment() rejects bad newdata", {
  f <- fake_fit("neg.exp", 0.4)
  expect_error(augment(f, newdata = data.frame(t = 1:3)), "'time' column")
  expect_error(augment(f, newdata = data.frame(time = c("a", "b"))), "must be numeric")
  expect_error(augment(f, newdata = "1"), "numeric vector")
  expect_error(augment(f, newdata = list(time = 1)), "numeric vector")
})

test_that("augment() with boot adds a band that contains the fitted curve", {
  fb <- fit_and_boot()
  grid <- seq(0, 5, by = 0.25)
  a <- augment(fb$fit, newdata = grid, boot = fb$boot)
  expect_named(a, c("time", ".fitted", ".lower", ".upper"))
  expect_true(all(a$.lower <= a$.upper))
  expect_gte(mean(a$.lower <= a$.fitted & a$.fitted <= a$.upper), 0.9)
  # every curve passes through 1 at time 0, so the band collapses there
  expect_equal(a$.lower[1], 1)
  expect_equal(a$.upper[1], 1)
  # a higher confidence level gives a wider band
  a50 <- augment(fb$fit, newdata = grid, boot = fb$boot, conf.level = 0.5)
  expect_true(all(a$.upper - a$.lower >= a50$.upper - a50$.lower - 1e-12))
  # band also works for the original data and a single time point
  expect_named(augment(fb$fit, boot = fb$boot), c("time", "mass", ".fitted", ".resid", ".lower", ".upper"))
  expect_equal(nrow(augment(fb$fit, newdata = 2, boot = fb$boot)), 1)
  expect_error(augment(fb$fit, boot = fb$boot, conf.level = 2), "conf.level")
})

test_that("augment() bands work for every model", {
  for (m in all_models) {
    fb <- fit_and_boot(m, nboot = 20)
    a <- augment(fb$fit, newdata = c(0.5, 2, 4), boot = fb$boot)
    expect_true(all(is.finite(a$.lower) & is.finite(a$.upper)), info = m)
  }
})

# ---- tidy.litfit_bootstrap ----

test_that("tidy() on a bootstrap gives replicates in long format", {
  fb <- fit_and_boot(nboot = 30)
  td <- tidy(fb$boot)
  expect_named(td, c("replicate", "term", "value"))
  ok <- sum(!is.na(fb$boot[, 1]))
  expect_equal(nrow(td), ok * 3)
  expect_equal(unique(td$term), c("beta", "alpha", "steady_state"))
  expect_equal(td$value[td$term == "beta"], unname(fb$boot[!is.na(fb$boot[, 1]), 1]))
  expect_type(td$replicate, "integer")
})

test_that("tidy() on a bootstrap drops failed replicates", {
  set.seed(1)
  fit <- quiet_fit(pineneedles$Year, pineneedles$Mass.remaining, "neg.exp", iters = 20)
  failed <- suppressWarnings(bootstrap_parameters(fit, nboot = 3, method = "BFGS"))
  td <- tidy(failed)
  expect_equal(nrow(td), 0)
  expect_named(td, c("replicate", "term", "value"))
})
