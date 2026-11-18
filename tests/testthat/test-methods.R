# S3 methods for litfit objects

test_that("coef, fitted, residuals, nobs and logLik are consistent for every model", {
  set.seed(1)
  for (m in all_models) {
    fit <- quiet_fit(pineneedles$Year, pineneedles$Mass.remaining, m, iters = 50)
    expect_named(coef(fit), expected_param_names[[m]])
    expect_equal(fitted(fit) + residuals(fit), fit$mass)
    expect_equal(predict(fit), fitted(fit))
    expect_equal(nobs(fit), length(pineneedles$Year))
    ll <- logLik(fit)
    expect_s3_class(ll, "logLik")
    expect_equal(attr(ll, "df"), fit$nparams)
    expect_equal(AIC(fit), fit$fitAIC)
    expect_equal(BIC(fit), fit$fitBIC)
  }
})

test_that("AIC compares several fits", {
  set.seed(1)
  f1 <- quiet_fit(pineneedles$Year, pineneedles$Mass.remaining, "neg.exp", iters = 50)
  f2 <- quiet_fit(pineneedles$Year, pineneedles$Mass.remaining, "weibull", iters = 50)
  a <- AIC(f1, f2)
  expect_equal(a$df, c(1, 2))
  expect_equal(a$AIC, c(f1$fitAIC, f2$fitAIC))
})

test_that("predict handles new time points", {
  f <- fake_fit("neg.exp", 0.4)
  expect_length(predict(f, newdata = 2), 1)
  expect_length(predict(f, newdata = seq(0, 20, 0.5)), 41)
  expect_equal(predict(f, newdata = c(0, 1)), exp(-0.4 * c(0, 1)))
  expect_equal(predict(f, newdata = numeric(0)), numeric(0))
})

test_that("predict_from_fit works", {
  x <- predict_from_fit("neg.exp", params = 0.2, target_time = 2)
  expect_equal(x, exp(-0.4))
})

test_that("print and summary work for every model", {
  set.seed(1)
  for (m in all_models) {
    fit <- quiet_fit(pineneedles$Year, pineneedles$Mass.remaining, m, iters = 50)
    expect_output(print(fit), "litfit object: model")
    capture.output(expect_invisible(print(fit)))
    s <- summary(fit)
    expect_s3_class(s, "summary.litfit")
    expect_equal(s$num.obs, length(pineneedles$Year))
    out <- capture.output(print(s))
    expect_true(any(grepl("Summary of litFit object", out)))
    expect_true(any(grepl(paste0("Parameter fit \\(", expected_param_names[[m]][1], "\\)"), out)))
    expect_true(any(grepl("steady state", out)))
  }
})

test_that("summary copes with a threshold that is never reached", {
  f <- fake_fit("neg.exp.limit", c(1, 0.3, 0.7))
  s <- suppressWarnings(summary(f))
  expect_true(is.na(s$time_to_50))
  expect_equal(s$steady_state, Inf)
  expect_output(print(s), "Time to 50% mass loss: NA")
  expect_output(print(s), "Inf")
})

test_that("utility functions behave", {
  expect_equal(rnd.to.text(1.11119, digits = 4), "1.1112")
  expect_equal(rnd.to.text(1.11119, digits = 2), "1.11")
  expect_true(are.within.ten.percent.of(1, 1.05))
  expect_false(are.within.ten.percent.of(1, 1.2))
})
