context("fit_litter")
data("pineneedles")

test_that("fit sane", {
  fit <-
    fit_litter(
      time = c(0, 1, 2, 3, 4, 5, 6),
      mass.remaining = c(1, 0.9, 1.01, 0.4, 0.6, 0.2, 0.01),
      model = "weibull",
      iters = 1000
    )
  expect_that(fit, is_a("litfit"))
  expect_true(fit$nparams > 0)
  expect_true(fit$optimFit$convergence == 0)
  expect_true(all(predict(fit) > 0))
  expect_true(steady_state(fit) > 0)
  expect_true(simulate.and.check("weibull"))
  expect_true(simulate.and.check("neg.exp"))
  expect_true(simulate.and.check("discrete.parallel"))
  expect_true(simulate.and.check("discrete.series"))
  expect_true(simulate.and.check("cont.quality"))
  expect_true(simulate.and.check("neg.exp.limit"))
})


test_that("plots and summaries dont throw errors", {
  expect_true(exists("plot", where = "package:litterfitter", mode = "function"))
  fit <-
    fit_litter(
      time = c(0, 1, 2, 3, 4, 5, 6),
      mass.remaining = c(1, 0.9, 1.01, 0.4, 0.6, 0.2, 0.01),
      model = "weibull",
      iters = 2000
    )
  expect_silent(plot(fit))
  expect_that(summary(fit), is_a("summary.litfit"))
  out <- summary(fit)
  expect_output(print(out), "Summary of litFit object")
  expect_output(
    plot_multiple_fits(
      time = pineneedles$Year,
      mass.remaining = pineneedles$Mass.remaining,
      model = c("neg.exp", "weibull"),
      bty = "n",
      iters = 2000
    ),
    "Number of successful fits"
  )
  
})

test_that("crazy input throws errors", {
  expect_error(fit_litter(
    time = c(0, 1, 2, 3, 4, 5, 6),
    mass.remaining = c(1000, 0.9, 1.01, 0.4, 0.6, 0.2, 0.01),
    model = "weibull",
    iters = 10
  ))
  expect_error(fit_litter(
    time = c(0,-1, 2, 3, 4, 5, 6),
    mass.remaining = c(1, 0.9, 1.01, 0.4, 0.6, 0.2, 0.01),
    model = "weibull",
    iters = 10
  ))
  expect_error(fit_litter(
    time = c(0, 1, 2, 3, 4),
    mass.remaining = c(1, 0.9, 1.01, 0.4, 0.6, 0.2, 0.01),
    model = "weibull",
    iters = 10
  ))
  expect_error(fit_litter(
    time = c(0, 1, 2, 3, 4),
    mass.remaining = c(1, 0.9, 1.01, 0.4, 0.6),
    model = "weibull",
    iters = 10,
    upper = 2
  ))
  expect_error(fit_litter(
    time = c(0, 1, 2, 3, 4),
    mass.remaining = c(1, 0.9, 1.01, 0.4, 0.6),
    model = "weibull",
    iters = 10,
    lower = 2
  ))
  expect_error(steady_state(pars = c(3, 3), model = 3))
  expect_error(steady_state(3))
  expect_error(steady_state(pars = c(3, 3), model = "netflix_and_chill"))
})

test_that("known parameter fits on boundary", {
  expect_warning(
    fit_litter(
      time = pineneedles$Year,
      mass.remaining = pineneedles$Mass.remaining,
      model = "discrete.series",
      iters = 200
    )
  )
  #  expect_warning(fit_litter(time=pineneedles$Year,mass.remaining = pineneedles$Mass.remaining,
  #                            model = "discrete.parallel",iters = 500))
  
  
})

test_that("utility functions work", {
  expect_that(rnd.to.text(1.11119, digits = 4), equals("1.1112"))
  expect_that(rnd.to.text(1.11119, digits = 2), equals("1.11"))
})

test_that("bootstraps are behaving", {
  fit <-
    fit_litter(
      time = c(0, 1, 2, 3, 4, 5, 6),
      mass.remaining = c(1, 0.9, 1.01, 0.4, 0.6, 0.2, 0.01),
      model = "neg.exp",
      iters = 2000
    )
  out <- bootstrap_parameters(fit)
  expect_true(are.within.ten.percent.of(median(out[, 1]), coef(fit)))
  expect_silent(plot(out))
})
