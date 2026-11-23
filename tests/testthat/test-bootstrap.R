# bootstrap_parameters() and its methods

test_that("bootstrap recovers the fitted parameter", {
  set.seed(789)
  fit <- quiet_fit(ex_time, ex_mass, "neg.exp", iters = 200)
  # A few replicates on this small data set can fail on some platforms (noLD,
  # arm64); they are recorded as NA with a warning.
  out <- suppressWarnings(bootstrap_parameters(fit, nboot = 300))
  expect_s3_class(out, "litfit_bootstrap")
  expect_equal(dim(out), c(300, 2))
  expect_lt(mean(is.na(out[, 1])), 0.05)
  expect_true(unname(are.within.ten.percent.of(median(out[, 1], na.rm = TRUE), coef(fit))))
})

test_that("bootstrap output has named columns for every model", {
  set.seed(1)
  for (m in all_models) {
    fit <- quiet_fit(pineneedles$Year, pineneedles$Mass.remaining, m, iters = 50)
    out <- suppressWarnings(bootstrap_parameters(fit, nboot = 5))
    expect_equal(colnames(out), c(expected_param_names[[m]], "steady_state"), info = m)
    expect_type(unclass(out), "double")
  }
})

test_that("failed bootstrap replicates are recorded as NA with a warning", {
  set.seed(1)
  fit <- quiet_fit(pineneedles$Year, pineneedles$Mass.remaining, "neg.exp", iters = 20)
  # a duplicated 'method' argument makes every optim call fail
  expect_warning(out <- bootstrap_parameters(fit, nboot = 4, method = "BFGS"), "4 of 4")
  expect_true(all(is.na(unclass(out))))
  expect_error(plot(out), "Not enough finite")
  expect_no_error(summary(out))
})

test_that("bootstrap discards finite estimates when optim hits its iteration limit", {
  set.seed(19)
  fit <- quiet_fit(pineneedles$Year, pineneedles$Mass.remaining, "weibull", iters = 10)
  set.seed(27)
  expect_warning(out <- bootstrap_parameters(fit, nboot = 1, control = list(maxit = 1)), "1 of 1")
  expect_true(all(is.na(out[1, ])))
  set.seed(27)
  expect_true(all(is.finite(bootstrap_parameters(fit, nboot = 1)[1, ])))
})

test_that("bootstrap rejects non-litfit input", {
  expect_error(bootstrap_parameters(list(a = 1)), "litfit")
})

test_that("bootstrap summary reports mean and requested quantiles", {
  set.seed(2)
  fit <- quiet_fit(ex_time, ex_mass, "weibull", iters = 100)
  out <- suppressWarnings(bootstrap_parameters(fit, nboot = 50))
  s <- summary(out)
  expect_equal(rownames(s), c("beta", "alpha", "steady_state"))
  expect_equal(colnames(s), c("mean", "2.5%", "50%", "97.5%"))
  expect_true(all(s[, "2.5%"] <= s[, "50%"] & s[, "50%"] <= s[, "97.5%"]))
  s2 <- summary(out, probs = c(0.1, 0.9))
  expect_equal(colnames(s2), c("mean", "10%", "90%"))
})

test_that("bootstrap plot works and returns invisibly", {
  set.seed(3)
  fit <- quiet_fit(ex_time, ex_mass, "weibull", iters = 100)
  out <- suppressWarnings(bootstrap_parameters(fit, nboot = 50))
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_invisible(plot(out))
  expect_invisible(plot(out, coef.index = 2))
  expect_invisible(plot(out, coef.index = 3))
})
