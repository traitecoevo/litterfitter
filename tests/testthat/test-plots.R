# Plotting functions

test_that("plot.litfit works for every model and returns invisibly", {
  set.seed(1)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  for (m in all_models) {
    fit <- quiet_fit(pineneedles$Year, pineneedles$Mass.remaining, m, iters = 50)
    expect_invisible(plot(fit))
    expect_silent(plot(fit, formulae.cex = 0.8))
  }
})

test_that("plot.litfit works for data above 1 and day-scale time", {
  set.seed(1)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  fit <- quiet_fit(c(0, 30, 90, 180, 365), c(1, 1.08, 0.9, 0.7, 0.5), "neg.exp", iters = 100)
  expect_silent(plot(fit))
})

test_that("plot_multiple_fits returns the fits invisibly", {
  set.seed(1)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_invisible(res <- plot_multiple_fits(
    time = pineneedles$Year, mass.remaining = pineneedles$Mass.remaining,
    model = c("neg.exp", "weibull"), bty = "n", iters = 50
  ))
  expect_length(res, 2)
  expect_s3_class(res[[1]], "litfit")

  res <- suppressWarnings(plot_multiple_fits(
    time = ex_time, mass.remaining = ex_mass,
    model = c("neg.exp", "weibull", "cont.quality"), color = c("red", "blue", "green"), iters = 50
  ))
  expect_length(res, 3)
  expect_error(
    plot_multiple_fits(pineneedles$Year, pineneedles$Mass.remaining, model = "nope", iters = 5),
    "should be one of"
  )
})
