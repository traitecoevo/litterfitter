# Model fitting behaviour across all models

test_that("every model fits the pineneedles data and returns a sane litfit", {
  set.seed(1)
  for (m in all_models) {
    fit <- quiet_fit(pineneedles$Year, pineneedles$Mass.remaining, m, iters = 200)
    expect_s3_class(fit, "litfit")
    expect_equal(fit$model, m)
    expect_equal(fit$nparams, length(expected_param_names[[m]]))
    expect_equal(fit$optimFit$convergence, 0)
    expect_true(all(is.finite(fit$predicted)))
    expect_true(is.finite(fit$logLik))
    expect_equal(fit$fitAIC, -2 * fit$logLik + 2 * fit$nparams)
    expect_lte(fit$fitAIC, fit$fitAICc)
  }
})

test_that("fits are reproducible under refitting with small noise", {
  set.seed(123)
  for (m in all_models) {
    expect_true(simulate_and_check(m), info = m)
  }
})

test_that("exact negative exponential data recovers the true rate", {
  set.seed(1)
  t <- 0:6
  fit <- quiet_fit(t, exp(-0.3 * t), "neg.exp", iters = 20)
  expect_equal(unname(coef(fit)), 0.3, tolerance = 1e-4)
})

test_that("exact Weibull data recovers the true parameters", {
  set.seed(1)
  t <- seq(0, 6, by = 0.5)
  fit <- quiet_fit(t, exp(-(t / 3)^1.8), "weibull", iters = 200)
  expect_equal(unname(coef(fit)), c(3, 1.8), tolerance = 1e-3)
})

test_that("order of observations does not affect the fit", {
  set.seed(1)
  f1 <- quiet_fit(pineneedles$Year, pineneedles$Mass.remaining, "weibull", iters = 100)
  o <- rev(seq_along(pineneedles$Year))
  set.seed(1)
  f2 <- quiet_fit(pineneedles$Year[o], pineneedles$Mass.remaining[o], "weibull", iters = 100)
  expect_equal(coef(f1), coef(f2), tolerance = 1e-4)
})

test_that("replicate bags at the same time point are handled", {
  set.seed(1)
  t <- rep(0:4, each = 3)
  m <- exp(-0.4 * t) + rep(c(-0.02, 0, 0.02), 5)
  fit <- quiet_fit(t, m, "neg.exp", iters = 20)
  expect_equal(unname(coef(fit)), 0.4, tolerance = 0.05)
  expect_equal(nobs(fit), 15)
})

test_that("time units only rescale the rate", {
  set.seed(1)
  f_years <- quiet_fit(pineneedles$Year, pineneedles$Mass.remaining, "neg.exp", iters = 50)
  f_days <- quiet_fit(pineneedles$Year * 365, pineneedles$Mass.remaining, "neg.exp", iters = 200)
  # within 1%: L-BFGS-B's relative stopping rule limits precision for tiny rates
  expect_equal(unname(coef(f_days)) * 365, unname(coef(f_years)), tolerance = 0.01)
})

test_that("model curves start at 1 (except neg.exp.limit, which starts at A + B)", {
  pars <- list(
    neg.exp = 0.5, weibull = c(2, 1.5), discrete.parallel = c(0.3, 1, 0.1),
    discrete.series = c(0.5, 1, 0.2), cont.quality = c(2, 1.5), neg.exp.limit = c(1, 0.6, 0.3)
  )
  for (m in all_models) {
    p0 <- predict(fake_fit(m, pars[[m]]), newdata = 0)
    expect_equal(p0, if (m == "neg.exp.limit") 0.9 else 1, info = m)
  }
})

test_that("discrete.parallel with equal rates reduces to neg.exp", {
  t <- seq(0, 5, 0.5)
  expect_equal(
    litterfitter:::discrete.parallel(t, 0.3, 0.7, 0.7),
    litterfitter:::neg.exp(t, 0.7)
  )
})

test_that("objective returns a finite penalty where the model is undefined", {
  # discrete.series divides by K1 - K2, so equal rates give NaN predictions
  obj <- litterfitter:::make_objective("discrete.series", 0:3, c(1, .8, .6, .4))
  expect_equal(obj(c(0.5, 1, 1)), litterfitter:::obj_penalty)
  expect_true(is.finite(obj(c(0.5, 1, 0.5))))
  # obj_func wrapper gives the same answer
  expect_equal(
    litterfitter:::obj_func(c(0.5, 1, 0.5), 0:3, c(1, .8, .6, .4), "discrete.series"),
    obj(c(0.5, 1, 0.5))
  )
})

test_that("model functions are looked up in the package, not the caller", {
  set.seed(1)
  neg.exp <- function(...) stop("user object should not be used")
  expect_s3_class(quiet_fit(pineneedles$Year, pineneedles$Mass.remaining, "neg.exp", iters = 5), "litfit")
})
