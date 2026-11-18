central_difference <- function(fn, p) {
  do.call(cbind, lapply(seq_along(p), function(j) {
    step <- numeric(length(p))
    step[j] <- 1e-5 * max(1, abs(p[j]))
    (fn(p + step) - fn(p - step)) / (2 * step[j])
  }))
}

test_that("curve and SSE derivatives match independent finite differences", {
  t <- c(0, 0.1, 1, 2, 7)
  obs <- c(1.05, 0.91, 0.65, 0.42, 0.1)
  pars <- list(neg.exp = 0.3, weibull = c(2, 1.2),
               discrete.parallel = c(0.4, 1, 0.2), discrete.series = c(0.4, 1, 0.2),
               cont.quality = c(0.7, 1.2), neg.exp.limit = c(0.3, 0.8, 0.1))
  for (m in names(pars)) {
    curve <- getFromNamespace(m, "litterfitter")
    pred <- function(p) do.call(curve, c(list(t), as.list(p)))
    for (p in list(pars[[m]], pars[[m]] * 1.3)) {
      J <- model_jacobian(m)(t, p)
      expect_identical(colnames(J), expected_param_names[[m]])
      expect_equal(unname(J), central_difference(pred, p), tolerance = 1e-7)
      grad <- make_gradient(m, t, obs)(p)
      ref <- central_difference(make_objective(m, t, obs), p)
      expect_equal(unname(grad), as.numeric(ref), tolerance = 1e-7)
    }
  }
})

test_that("series curve and its partials are continuous across equal rates", {
  t <- c(0, 0.5, 2, 4)
  R <- 0.4
  k <- 0.7
  decay <- exp(-k * t)
  limit <- decay * (1 + (1 - R) * k * t)
  half <- -(1 - R) * k * t^2 * decay / 2
  partials <- cbind(R = -k * t * decay, K1 = -R * t * decay + half, K2 = half)
  for (delta in c(-1e-10, 0, 1e-10)) {
    p <- c(R, k, k + delta)
    expect_equal(discrete.series(t, R, k, k + delta), limit, tolerance = 1e-9)
    expect_equal(model_jacobian("discrete.series")(t, p), partials, tolerance = 1e-9)
  }
  for (rates in list(c(1000, 1e-4), c(1e-4, 1000))) {
    p <- c(R, rates)
    curve <- function(p) discrete.series(t, p[1], p[2], p[3])
    e1 <- exp(-p[2] * t)
    e2 <- exp(-p[3] * t)
    expected <- e1 + (1 - R) * p[2] * (e2 - e1) / (p[2] - p[3])
    expect_equal(curve(p), expected)
    expect_equal(unname(model_jacobian("discrete.series")(t, p)),
                 central_difference(curve, p), tolerance = 1e-7)
  }
})

test_that("Weibull tail and origin have finite derivatives", {
  expect_equal(unname(model_jacobian("weibull")(c(0, 36500), c(1e-4, 50))),
               matrix(0, 2, 2))
})

test_that("Weibull fits and bootstraps reject a nonpositive beta bound before optimization", {
  t <- 0:5
  y <- exp(-(t / 2)^1.5)
  fit <- fake_fit("weibull", c(2, 1.5), time = t)
  for (beta_min in c(0, -0.1)) {
    lower <- c(beta_min, 0.1)
    expect_error(quiet_fit(t, y, "weibull", iters = 2, lower = lower), "beta.*positive")
    expect_error(bootstrap_parameters(fit, nboot = 1, lower = lower), "beta.*positive")
  }
})

test_that("fixed-start litter fits agree across time units and gradient methods", {
  t <- pineneedles$Year
  y <- pineneedles$Mass.remaining
  starts <- list(neg.exp = 0.3, weibull = c(2, 1.2), discrete.parallel = c(0.4, 1, 0.2),
                 discrete.series = c(0.4, 1, 0.2), cont.quality = c(0.1, 3),
                 neg.exp.limit = c(0.3, 0.8, 0.1))
  for (m in names(starts)) {
    p <- starts[[m]]
    units <- expected_param_names[[m]]
    day_scale <- ifelse(units == "beta", 365,
                        ifelse(units %in% c("k", "K1", "K2", "b"), 1/365, 1))
    fit <- function(time, scale, analytic, factr) {
      run_optim(p * scale, make_objective(m, time, y),
                model_bounds(m, "lower") * scale, model_bounds(m, "upper") * scale,
                gr = if (analytic) make_gradient(m, time, y),
                control = list(parscale = scale, maxit = 500, factr = factr,
                               ndeps = rep(1e-6, length(p))))
    }
    # Years vs days compares predictions, so stop tightly: cont.quality has a
    # shallow minimum, and the default factr leaves platform-dependent slack
    # along it (~1e-5 on Windows).
    years <- fit(t, rep(1, length(p)), TRUE, 1e3)
    days <- fit(t * 365, day_scale, TRUE, 1e3)
    # Finite differences are too noisy for that precision, so compare the
    # gradient methods at optim's default stopping rule.
    an <- fit(t * 365, day_scale, TRUE, 1e7)
    fd <- fit(t * 365, day_scale, FALSE, 1e7)
    expect_equal(c(years = years$convergence, days = days$convergence,
                   an = an$convergence, fd = fd$convergence),
                 c(years = 0, days = 0, an = 0, fd = 0), info = m)
    expect_equal(years$value, days$value, tolerance = 1e-7, info = m)
    expect_equal(an$value, fd$value, tolerance = 1e-7, info = m)
    expect_equal(predict_from_fit(m, years$par, t), predict_from_fit(m, days$par, t * 365),
                 tolerance = 1e-6, info = m)
  }
})

test_that("caller gradients are honoured by fits and bootstrap", {
  used <- 0L
  t <- 0:5
  y <- exp(-0.4 * t)
  grad <- function(p) {
    used <<- used + 1L
    make_gradient("neg.exp", t, y)(p)
  }
  set.seed(27)
  fit <- quiet_fit(t, y, "neg.exp", iters = 2, gr = grad)
  expect_gt(used, 0L)
  used <- 0L
  bootstrap_parameters(fit, nboot = 1, gr = grad)
  expect_gt(used, 0L)
  expect_null(make_gradient("import.model", t, y))
})
