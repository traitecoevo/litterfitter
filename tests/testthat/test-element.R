# fit_element(), import.model and elementfit methods

# ---- the model ----

test_that("import.model passes through 1 at full mass and 0 at no mass", {
  for (a in c(0.1, 0.5, 3)) for (b in c(0.05, 0.4, 1)) {
    expect_equal(litterfitter:::import.model(1, a, b), 1)
    expect_equal(litterfitter:::import.model(0, a, b), 0)
  }
})

test_that("import.model is the original percent formulation rescaled to proportions", {
  f_pct <- function(x, a, b) (x / b) / sqrt((2 * a * x / b)^2 + (1 - (x / b)^2)^2)
  original <- function(x, a, b) f_pct(x, a, b) / f_pct(100, a, b)
  x <- seq(0.05, 1, by = 0.05)
  for (a in c(0.2, 1, 4)) for (b in c(0.1, 0.5, 0.9)) {
    expect_equal(litterfitter:::import.model(x, a, b), original(100 * x, a, 100 * b))
  }
})

test_that("element_peak finds the maximum of the curve", {
  for (p in list(c(0.3, 0.5), c(0.5, 0.65), c(1, 0.3))) {
    pk <- litterfitter:::element_peak("import.model", p)
    grid <- litterfitter:::import.model(seq(0, 1, by = 1e-4), p[1], p[2])
    expect_equal(pk$peak_element, max(grid), tolerance = 1e-6)
    expect_gte(pk$peak_element, 1)
  }
  # a wide peak centred at full mass: no net immobilisation, peak at mass 1
  pk <- litterfitter:::element_peak("import.model", c(5, 1))
  expect_equal(pk, list(peak_element = 1, mass_at_peak = 1))
})

# ---- fitting ----

test_that("fit_element returns a sane elementfit for the example data", {
  set.seed(1)
  fit <- quiet_element(pineneedles$Mass.remaining, ex_element, iters = 200)
  expect_s3_class(fit, "elementfit")
  expect_equal(fit$model, "import.model")
  expect_named(coef(fit), c("a", "b"))
  expect_true(all(coef(fit) >= c(0.1, 0.01) & coef(fit) <= c(10, 1)))
  expect_equal(fit$optimFit$convergence, 0)
  expect_equal(fit$fitAIC, -2 * fit$logLik + 2 * fit$nparams)
  expect_equal(unname(coef(fit)), c(0.492, 0.646), tolerance = 0.01)
})

test_that("exact synthetic data recovers the true parameters", {
  set.seed(1)
  d <- synthetic_element(0.6, 0.5)
  fit <- quiet_element(d$mass, d$element, iters = 200)
  expect_equal(unname(coef(fit)), c(0.6, 0.5), tolerance = 1e-3)
})

test_that("order of observations does not affect the fit", {
  set.seed(1)
  f1 <- quiet_element(pineneedles$Mass.remaining, ex_element)
  o <- rev(seq_along(ex_element))
  set.seed(1)
  f2 <- quiet_element(pineneedles$Mass.remaining[o], ex_element[o])
  expect_equal(coef(f1), coef(f2), tolerance = 1e-4)
})

test_that("model argument defaults to import.model and rejects others", {
  set.seed(1)
  expect_equal(quiet_element(pineneedles$Mass.remaining, ex_element, iters = 10)$model, "import.model")
  expect_error(fit_element(pineneedles$Mass.remaining, ex_element, model = "stoch.model"), "should be")
})

# ---- input validation ----

test_that("lengths, types, finiteness and iters are checked", {
  m <- pineneedles$Mass.remaining
  expect_error(fit_element(m, ex_element[-1]), "same length")
  expect_error(fit_element(as.character(m), ex_element), "numeric")
  expect_error(fit_element(m, as.character(ex_element)), "numeric")
  expect_error(fit_element(c(m[-1], Inf), ex_element), "infinite")
  expect_error(fit_element(m, c(ex_element[-1], Inf)), "infinite")
  for (bad in list(0, -1, NA, "10", c(5, 10))) {
    expect_error(fit_element(m, ex_element, iters = bad), "iters")
  }
})

test_that("percent input is detected for mass and element", {
  expect_error(
    fit_element(pineneedles$Mass.remaining * 100, ex_element, iters = 5),
    "divide by 100"
  )
  expect_error(
    fit_element(pineneedles$Mass.remaining, ex_element * 100, iters = 5),
    "divide by 100"
  )
  expect_error(fit_element(c(1, .8, -0.3, .4), c(1, 1.1, 1, .8), iters = 5), "proportional mass")
  expect_error(fit_element(c(1, .8, .6, .4), c(1, 1.1, -0.5, .8), iters = 5), "proportional element")
})

test_that("missing values are removed and too few observations is an error", {
  set.seed(1)
  expect_warning(
    fit <- fit_element(c(pineneedles$Mass.remaining, NA), c(ex_element, 0.5),
      iters = 20, quiet = TRUE
    ),
    class = "litterfitter_missing_removed"
  )
  expect_equal(nobs(fit), 6)
  expect_error(fit_element(c(1, 0.5), c(1, 1.2)), "Too few")
  expect_error(suppressWarnings(fit_element(c(1, 0.5, NA), c(1, 1.2, 1))), "Too few")
  expect_error(fit_element(rep(0.5, 4), c(1, 1.1, 1.2, 1)), "distinct values")
})

test_that("bounds are checked and respected", {
  m <- pineneedles$Mass.remaining
  expect_error(fit_element(m, ex_element, upper = 1), "upper bounds")
  expect_error(fit_element(m, ex_element, lower = 1), "lower bounds")
  expect_error(fit_element(m, ex_element, lower = c(5, 0.5), upper = c(1, 1)), "smaller than")
  set.seed(1)
  fit <- quiet_element(m, ex_element, iters = 50, lower = c(1, 0.2), upper = c(3, 0.5))
  expect_true(all(coef(fit) >= c(1, 0.2) & coef(fit) <= c(3, 0.5)))
})

test_that("warnings: mass above 1, element below 0, boundary", {
  set.seed(1)
  expect_warning(
    fit_element(c(1.02, pineneedles$Mass.remaining[-1]), ex_element, iters = 20, quiet = TRUE),
    class = "litterfitter_mass_above_one"
  )
  expect_warning(
    fit_element(c(pineneedles$Mass.remaining, 0.1), c(ex_element, -0.02), iters = 20, quiet = TRUE),
    class = "litterfitter_element_below_zero"
  )
  w <- testthat::capture_warnings(
    fit_element(pineneedles$Mass.remaining, ex_element, iters = 20, quiet = TRUE, upper = c(0.3, 1))
  )
  expect_match(w, "boundary \\(a\\)", all = FALSE)
})

test_that("element values above 1 are normal and do not warn", {
  set.seed(1)
  d <- synthetic_element(0.4, 0.6)
  expect_true(max(d$element) > 1)
  expect_no_warning(fit_element(d$mass, d$element, iters = 50, quiet = TRUE))
})

test_that("quiet controls the progress message", {
  set.seed(1)
  expect_message(fit_element(pineneedles$Mass.remaining, ex_element, iters = 5), "successful fits")
  expect_no_message(suppressWarnings(
    fit_element(pineneedles$Mass.remaining, ex_element, iters = 5, quiet = TRUE)
  ))
})

# ---- methods ----

test_that("extractor methods are consistent", {
  set.seed(1)
  fit <- quiet_element(pineneedles$Mass.remaining, ex_element)
  expect_equal(fitted(fit) + residuals(fit), ex_element)
  expect_equal(predict(fit), fitted(fit))
  expect_equal(predict(fit, newdata = c(1, 0)), c(1, 0))
  expect_length(predict(fit, newdata = seq(0, 1, 0.1)), 11)
  expect_equal(nobs(fit), 6)
  ll <- logLik(fit)
  expect_s3_class(ll, "logLik")
  expect_equal(attr(ll, "df"), 2)
  expect_equal(AIC(fit), fit$fitAIC)
  expect_equal(BIC(fit), fit$fitBIC)
})

test_that("print and plot work", {
  set.seed(1)
  fit <- quiet_element(pineneedles$Mass.remaining, ex_element)
  expect_output(print(fit), "elementfit object: model import.model fit to 6 observations")
  expect_output(print(fit), "Peak element remaining")
  capture.output(expect_invisible(print(fit)))
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_invisible(plot(fit))
  fit2 <- quiet_element(c(1.05, pineneedles$Mass.remaining[-1]), ex_element)
  expect_silent(plot(fit2))
})

test_that("tidy and glance work for element fits", {
  set.seed(1)
  fit <- quiet_element(pineneedles$Mass.remaining, ex_element)
  td <- tidy(fit)
  expect_named(td, c("term", "estimate"))
  expect_equal(td$estimate, unname(coef(fit)))
  g <- glance(fit)
  expect_named(g, c(
    "model", "nobs", "nparams", "sigma", "logLik", "AIC", "AICc", "BIC",
    "converged", "peak_element", "mass_at_peak"
  ))
  expect_equal(g$AIC, AIC(fit))
  expect_equal(g$sigma^2 * g$nobs, sum(residuals(fit)^2), tolerance = 1e-8)
  pk <- litterfitter:::element_peak("import.model", coef(fit))
  expect_equal(g$peak_element, pk$peak_element)
  expect_equal(g$mass_at_peak, pk$mass_at_peak)
  expect_gt(g$peak_element, 1)
})

test_that("augment works for element fits", {
  set.seed(1)
  fit <- quiet_element(pineneedles$Mass.remaining, ex_element)
  a <- augment(fit)
  expect_named(a, c("mass", "element", ".fitted", ".resid"))
  expect_equal(a$.resid, unname(residuals(fit)))
  a <- augment(fit, newdata = c(1, 0.5))
  expect_named(a, c("mass", ".fitted"))
  a <- augment(fit, newdata = data.frame(mass = c(1, 0.5), plot = c("x", "y")))
  expect_named(a, c("mass", "plot", ".fitted"))
  expect_error(augment(fit, newdata = data.frame(time = 1)), "'mass' column")
})

# ---- bootstrap ----

test_that("bootstrap works for element fits with derived quantities", {
  set.seed(2)
  fit <- quiet_element(pineneedles$Mass.remaining, ex_element, iters = 100)
  boot <- suppressWarnings(bootstrap_parameters(fit, nboot = 40))
  expect_s3_class(boot, "litfit_bootstrap")
  expect_equal(attr(boot, "model"), "import.model")
  expect_equal(colnames(boot), c("a", "b", "peak_element", "mass_at_peak"))
  ok <- !is.na(boot[, 1])
  expect_true(all(boot[ok, "peak_element"] >= 1))
  expect_true(all(boot[ok, "mass_at_peak"] > 0 & boot[ok, "mass_at_peak"] <= 1))

  s <- summary(boot)
  expect_equal(rownames(s), c("a", "b", "peak_element", "mass_at_peak"))
  expect_equal(unique(tidy(boot)$term), c("a", "b", "peak_element", "mass_at_peak"))

  td <- tidy(fit, boot = boot)
  expect_named(td, c("term", "estimate", "std.error", "conf.low", "conf.high"))

  a <- augment(fit, newdata = seq(1, 0, by = -0.1), boot = boot)
  expect_true(all(a$.lower <= a$.upper))
  expect_equal(c(a$.lower[1], a$.upper[1]), c(1, 1))
  expect_equal(c(a$.lower[11], a$.upper[11]), c(0, 0))

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_invisible(plot(boot, coef.index = 3))
})

test_that("bootstraps cannot be mixed between mass and element fits", {
  set.seed(1)
  efit <- quiet_element(pineneedles$Mass.remaining, ex_element, iters = 50)
  lfit <- quiet_fit(pineneedles$Year, pineneedles$Mass.remaining, "weibull", iters = 50)
  lboot <- suppressWarnings(bootstrap_parameters(lfit, nboot = 10))
  eboot <- suppressWarnings(bootstrap_parameters(efit, nboot = 10))
  expect_error(tidy(efit, boot = lboot), "generated from a 'weibull' fit")
  expect_error(augment(lfit, boot = eboot), "generated from a 'import.model' fit")
  expect_error(bootstrap_parameters(list()), "'litfit' or 'elementfit'")
})
