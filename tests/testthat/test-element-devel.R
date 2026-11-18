# Experimental element models (import-devel branch only; see #30)

test_that("stoch.model passes through 1 at full mass and 0 at no mass", {
  for (l in c(20, 40, 80)) for (e in c(0.2, 0.5, 0.8)) {
    expect_equal(litterfitter:::stoch.model(1, l, e), 1)
    expect_equal(litterfitter:::stoch.model(0, l, e), 0)
  }
})

test_that("stoch.model can be fitted with fit_element", {
  set.seed(1)
  m <- seq(1, 0.1, by = -0.1)
  el <- litterfitter:::stoch.model(m, 40, 0.4)
  fit <- suppressWarnings(fit_element(m, el, model = "stoch.model", iters = 200, quiet = TRUE))
  expect_s3_class(fit, "elementfit")
  expect_named(coef(fit), c("l", "e"))
  expect_true(coef(fit)[["e"]] > 0 && coef(fit)[["e"]] < 1)
  expect_equal(unname(coef(fit)), c(40, 0.4), tolerance = 1e-2)
  expect_s3_class(glance(fit), "data.frame")
})

test_that("exp.model is not available in fit_element yet", {
  expect_equal(litterfitter:::exp.model(1, 2, 0.5), 2.5)
  expect_error(fit_element(c(1, .8, .6, .4), c(1, 1.1, 1, .8), model = "exp.model"), "should be one of")
})
