# test-fit_litter.R
# Testthat edition 3 is assumed (DESCRIPTION: Config/testthat/edition: 3)

test_that("fit is sane", {
  set.seed(123)   # reproducible fit
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
  # ... (other expect_error calls unchanged)
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
  skip_on_cran()  # heavier / stochastic
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
  
  expect_true(are.within.ten.percent.of(median(out[, 1]), coef(fit)))
  expect_silent(plot(out))
})
