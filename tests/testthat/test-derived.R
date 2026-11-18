# steady_state() and time_to_prop_mass_remaining()

test_that("steady state equals the integral of the mass-remaining curve for every model", {
  pars <- list(
    neg.exp = 0.5, weibull = c(2, 1.5), discrete.parallel = c(0.3, 1, 0.1),
    discrete.series = c(0.5, 1, 0.2), cont.quality = c(2, 1.5), neg.exp.limit = c(1, 0.6, 0)
  )
  for (m in all_models) {
    f <- fake_fit(m, pars[[m]])
    integral <- stats::integrate(function(t) predict(f, newdata = t), 0, Inf)$value
    expect_equal(unname(steady_state(f)), integral, tolerance = 1e-4, info = m)
  }
})

test_that("steady state can be given parameters directly", {
  f <- fake_fit("weibull", c(6, 2))
  expect_equal(steady_state(pars = c(6, 2), model = "weibull"), steady_state(f))
  expect_named(steady_state(pars = 0.5, model = "neg.exp"), "neg.exp")
  expect_type(steady_state(pars = 0.5, model = "neg.exp"), "double")
})

test_that("neg.exp.limit steady state is Inf only for a meaningful asymptote", {
  expect_equal(unname(steady_state(pars = c(1, 0.6, 0.3), model = "neg.exp.limit")), Inf)
  # B at its lower bound is effectively zero
  expect_equal(unname(steady_state(pars = c(0.5, 1, 1e-6), model = "neg.exp.limit")), 2)
  expect_equal(unname(steady_state(pars = c(0.5, 1, 0), model = "neg.exp.limit")), 2)
})

test_that("steady state rejects bad input", {
  expect_error(steady_state(pars = c(3, 3), model = 3))
  expect_error(steady_state(3))
  expect_error(steady_state())
  expect_error(steady_state(pars = c(3, 3), model = "netflix_and_chill"), "implemented")
})

test_that("time to threshold matches closed-form solutions", {
  p <- 0.3
  f <- fake_fit("neg.exp", 0.4)
  expect_equal(time_to_prop_mass_remaining(f, p), -log(p) / 0.4, tolerance = 1e-6)

  f <- fake_fit("weibull", c(3, 1.8))
  expect_equal(time_to_prop_mass_remaining(f, p), 3 * (-log(p))^(1 / 1.8), tolerance = 1e-6)

  f <- fake_fit("cont.quality", c(2, 1.5))
  expect_equal(time_to_prop_mass_remaining(f, p), (p^(-1 / 1.5) - 1) / 2, tolerance = 1e-6)

  f <- fake_fit("neg.exp.limit", c(1, 0.8, 0.1))
  expect_equal(time_to_prop_mass_remaining(f, p), -log((p - 0.1) / 0.8), tolerance = 1e-6)
})

test_that("time to threshold decreases as the threshold rises", {
  f <- fake_fit("discrete.parallel", c(0.4, 1, 0.1), time = 0:30)
  t <- sapply(c(0.75, 0.5, 0.25), function(p) time_to_prop_mass_remaining(f, p))
  expect_true(all(diff(t) > 0))
})

test_that("extrapolation beyond the data warns but stays accurate", {
  f <- fake_fit("neg.exp", 0.0005, time = c(0, 30, 60, 120, 240, 365))
  expect_warning(t50 <- time_to_prop_mass_remaining(f, 0.5), "within the time range")
  expect_equal(t50, log(2) / 0.0005, tolerance = 1e-6)
})

test_that("a threshold that is never reached returns NA with a warning", {
  f <- fake_fit("neg.exp.limit", c(1, 0.3, 0.7))
  w <- testthat::capture_warnings(res <- time_to_prop_mass_remaining(f, 0.5))
  expect_true(is.na(res))
  expect_match(w, "never", all = FALSE)
})

test_that("a threshold already passed at time 0 returns 0", {
  f <- fake_fit("neg.exp.limit", c(1, 0.2, 0.1))
  expect_equal(time_to_prop_mass_remaining(f, 0.5), 0)
})

test_that("time to threshold rejects bad input", {
  f <- fake_fit("neg.exp", 0.4)
  expect_error(time_to_prop_mass_remaining(3), "litfit")
  for (bad in list(0, 1, -0.1, 1.5, c(0.5, 0.6), "0.5", NA_real_)) {
    expect_error(time_to_prop_mass_remaining(f, bad), "threshold.mass")
  }
})
