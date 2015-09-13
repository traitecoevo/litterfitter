context("fit_litter")


test_that("fit sane", {
    fit <- fit_litter(time = c(0, 1, 2, 3, 4, 5, 6), mass.remaining = c(1, 0.9, 1.01, 0.4, 0.6, 0.2, 0.01),
        model = "weibull", iters = 1000)
    expect_that(fit, is_a("litfit"))
    expect_true(fit$nparams > 0)
    expect_true(fit$optimFit$convergence == 0)
    expect_true(all(predict(fit) > 0))
    expect_true(steady_state(fit)>0)
    expect_true(simulate.and.check("weibull"))
    expect_true(simulate.and.check("neg.exp"))
})


test_that("plots don't throw errors", {
  expect_true(exists('plot', where='package:litterfitter', mode='function'))
  fit <- fit_litter(time = c(0, 1, 2, 3, 4, 5, 6), mass.remaining = c(1, 0.9, 1.01, 0.4, 0.6, 0.2, 0.01),
                    model = "weibull", iters = 1000)
  plot(fit)
  plot_multiple_fits(time = pineneedles$Year, mass.remaining = pineneedles$Mass.remaining,model=c("neg.exp", "weibull"),
                     bty = 'n', iters = 1500)

})

test_that("crazy input throws errors",{
  throws_error(fit_litter(time = c(0, 1, 2, 3, 4, 5, 6), mass.remaining = c(1000, 0.9, 1.01, 0.4, 0.6, 0.2, 0.01),
             model = "weibull", iters = 1000))
  throws_error(fit_litter(time = c(0, -1, 2, 3, 4, 5, 6), mass.remaining = c(1, 0.9, 1.01, 0.4, 0.6, 0.2, 0.01),
                          model = "weibull", iters = 1000))
})



