context("fit_litter")


test_that("fit sane", {
  fit <- fit_litter(time=c(0,1,2,3,4,5,6),mass.remaining=c(1,0.9,1.01,0.4,0.6,0.2,0.01),model="weibull",iters=1000)
  expect_that(fit, is_a("litfit"))
  expect_true(fit$nparams>0)
})