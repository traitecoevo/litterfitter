# test-fit_litter.R
# Testthat edition 3 is assumed (DESCRIPTION: Config/testthat/edition: 3)

test_that("fit is sane", {
  set.seed(123) # reproducible fit
  fit <- fit_litter(
    time = c(0, 1, 2, 3, 4, 5, 6),
    mass.remaining = c(1, 0.9, 1.01, 0.4, 0.6, 0.2, 0.01),
    model = "weibull",
    iters = 1000
  )

  expect_s3_class(fit, "litfit")
  expect_gt(fit$nparams, 0)
  expect_equal(fit$optimFit$convergence, 0)
  expect_true(all(predict(fit) > 0))
  expect_gt(steady_state(fit), 0)

  expect_true(simulate_and_check("weibull"))
  expect_true(simulate_and_check("neg.exp"))
  expect_true(simulate_and_check("discrete.parallel"))
  expect_true(simulate_and_check("discrete.series"))
  expect_true(simulate_and_check("cont.quality"))
  expect_true(simulate_and_check("neg.exp.limit"))
})

test_that("plots and summaries do not throw errors", {
  skip_on_cran() # plotting can be flaky on headless CRAN machines
  set.seed(123)

  data("pineneedles", package = "litterfitter")

  fit <- fit_litter(
    time = c(0, 1, 2, 3, 4, 5, 6),
    mass.remaining = c(1, 0.9, 1.01, 0.4, 0.6, 0.2, 0.01),
    model = "weibull",
    iters = 2000
  )

  expect_silent(plot(fit))

  s <- summary(fit)
  expect_s3_class(s, "summary.litfit")
  expect_output(print(s), "Summary of litFit object")

  expect_invisible(
    plot_multiple_fits(
      time = pineneedles$Year,
      mass.remaining = pineneedles$Mass.remaining,
      model = c("neg.exp", "weibull"),
      bty = "n",
      iters = 2000
    )
  )
})

test_that("invalid inputs throw informative errors", {
  # unchanged, deterministic
  expect_error(
    fit_litter(
      time = c(0, 1, 2, 3, 4, 5, 6),
      mass.remaining = c(1000, 0.9, 1.01, 0.4, 0.6, 0.2, 0.01),
      model = "weibull",
      iters = 10
    ),
    class = "error"
  )
  # negative time
  expect_error(fit_litter(
    time = c(0, -1, 2, 3, 4, 5, 6),
    mass.remaining = c(1, 0.9, 1.01, 0.4, 0.6, 0.2, 0.01),
    model = "weibull", iters = 10
  ))
  # mismatched lengths
  expect_error(fit_litter(
    time = c(0, 1, 2, 3, 4),
    mass.remaining = c(1, 0.9, 1.01, 0.4, 0.6, 0.2, 0.01),
    model = "weibull", iters = 10
  ))
  # wrong number of bounds
  expect_error(fit_litter(
    time = c(0, 1, 2, 3, 4), mass.remaining = c(1, 0.9, 1.01, 0.4, 0.6),
    model = "weibull", iters = 10, upper = 2
  ), "upper bounds")
  expect_error(fit_litter(
    time = c(0, 1, 2, 3, 4), mass.remaining = c(1, 0.9, 1.01, 0.4, 0.6),
    model = "weibull", iters = 10, lower = 2
  ), "lower bounds")
  # clearly negative mass is an error; slightly negative is a warning (issue #21)
  expect_error(fit_litter(
    time = 0:3, mass.remaining = c(1, 0.5, -0.2, -0.5),
    model = "neg.exp", iters = 10
  ), "proportional mass")
  expect_warning(
    fit_litter(
      time = 0:4, mass.remaining = c(1, 0.5, 0.2, 0.05, -0.01),
      model = "neg.exp", iters = 10, quiet = TRUE
    ),
    "measurement error"
  )
  # unknown model name
  expect_error(fit_litter(
    time = 0:3, mass.remaining = c(1, 0.5, 0.3, 0.2),
    model = "mean", iters = 10
  ), "should be one of")
  # too few observations for the number of parameters
  expect_error(fit_litter(
    time = 0:2, mass.remaining = c(1, 0.5, 0.3),
    model = "discrete.parallel", iters = 10
  ), "Too few observations")

  expect_error(steady_state(pars = c(3, 3), model = 3))
  expect_error(steady_state(3))
  expect_error(steady_state(pars = c(3, 3), model = "netflix_and_chill"))
  expect_error(time_to_prop_mass_remaining(3))
})

test_that("regression: previously broken inputs now work", {
  set.seed(42)
  # model argument defaults to neg.exp
  fit <- fit_litter(pineneedles$Year, pineneedles$Mass.remaining, iters = 20, quiet = TRUE)
  expect_equal(fit$model, "neg.exp")

  # fewer than 5 starts no longer crashes the convergence check
  expect_s3_class(
    fit_litter(pineneedles$Year, pineneedles$Mass.remaining, "neg.exp", iters = 3, quiet = TRUE),
    "litfit"
  )

  # missing values are dropped with a warning
  expect_warning(
    fit_na <- fit_litter(
      c(pineneedles$Year, NA), c(pineneedles$Mass.remaining, 0.5),
      "neg.exp", iters = 20, quiet = TRUE
    ),
    "missing"
  )
  expect_equal(nobs(fit_na), length(pineneedles$Year))
})

test_that("neg.exp.limit works with summary and bootstrap", {
  set.seed(42)
  fit <- suppressWarnings(fit_litter(
    pineneedles$Year, pineneedles$Mass.remaining, "neg.exp.limit",
    iters = 100, quiet = TRUE
  ))
  expect_type(steady_state(fit), "double")
  expect_output(print(summary(fit)), "steady state")
  boot <- suppressWarnings(bootstrap_parameters(fit, nboot = 20))
  expect_type(unclass(boot), "double")
  expect_equal(colnames(boot), c("k", "A", "B", "steady_state"))
})

test_that("time_to_prop_mass_remaining is accurate and scale independent", {
  set.seed(42)
  fit <- fit_litter(pineneedles$Year, pineneedles$Mass.remaining, "neg.exp", iters = 50, quiet = TRUE)
  k <- unname(coef(fit))
  expect_equal(time_to_prop_mass_remaining(fit, 0.5), log(2) / k, tolerance = 1e-6)

  # data on a day scale that never reaches 50% within the observed range
  days <- c(0, 30, 60, 120, 240, 365)
  mass <- exp(-0.0005 * days)
  fit_days <- fit_litter(days, mass, "neg.exp", iters = 200, quiet = TRUE)
  expect_warning(t50 <- time_to_prop_mass_remaining(fit_days, 0.5), "within the time range")
  expect_equal(t50, log(2) / unname(coef(fit_days)), tolerance = 1e-4)

  # asymptote above the threshold: never reached
  fake <- fit_days
  fake$model <- "neg.exp.limit"
  fake$optimFit$par <- c(k = 1, A = 0.3, B = 0.7)
  w <- testthat::capture_warnings(res <- time_to_prop_mass_remaining(fake, 0.5))
  expect_true(is.na(res))
  expect_match(w, "never", all = FALSE)
})

test_that("standard S3 methods work", {
  set.seed(42)
  f1 <- fit_litter(pineneedles$Year, pineneedles$Mass.remaining, "neg.exp", iters = 50, quiet = TRUE)
  f2 <- fit_litter(pineneedles$Year, pineneedles$Mass.remaining, "weibull", iters = 50, quiet = TRUE)
  expect_named(coef(f2), c("beta", "alpha"))
  expect_equal(AIC(f1), f1$fitAIC)
  expect_equal(BIC(f2), f2$fitBIC)
  expect_equal(nrow(AIC(f1, f2)), 2)
  expect_equal(residuals(f1), f1$mass - fitted(f1))
  expect_output(print(f1), "litfit object")
})

test_that("known parameter fits warn on boundary conditions", {
  data("pineneedles", package = "litterfitter")
  set.seed(456)

  expect_warning(
    fit_litter(
      time = pineneedles$Year,
      mass.remaining = pineneedles$Mass.remaining,
      model = "discrete.series",
      iters = 200
    )
  )
})

test_that("utility functions behave", {
  expect_equal(rnd.to.text(1.11119, digits = 4), "1.1112")
  expect_equal(rnd.to.text(1.11119, digits = 2), "1.11")
})

test_that("bootstraps behave", {
  set.seed(789)

  fit <- fit_litter(
    time = c(0, 1, 2, 3, 4, 5, 6),
    mass.remaining = c(1, 0.9, 1.01, 0.4, 0.6, 0.2, 0.01),
    model = "neg.exp",
    iters = 2000
  )

  out <- bootstrap_parameters(fit)

  x <- predict_from_fit("neg.exp", params = 0.2, target_time = 2)
  expect_type(x, "double")

  expect_true(unname(are.within.ten.percent.of(median(out[, 1]), coef(fit))))
  expect_silent(plot(out))
  s <- summary(out)
  expect_equal(rownames(s), c("k", "steady_state"))
  expect_equal(colnames(s), c("mean", "2.5%", "50%", "97.5%"))
})
