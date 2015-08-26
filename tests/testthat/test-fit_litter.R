context("fit_litter")


test_that("fit sane", {
    fit <- fit_litter(time = c(0, 1, 2, 3, 4, 5, 6), mass.remaining = c(1, 0.9, 1.01, 0.4, 0.6, 0.2, 0.01), 
        model = "weibull", iters = 1000)
    simulated.data <- simulate.decomposition.with.error(fit, sigma = 0.01)
    simulated.fit <- fit_litter(time = simulated.data$time, mass.remaining = simulated.data$mass.remaining, 
        model = "weibull", iters = 1000)
    expect_true(are.within.ten.percent.of(coef(fit)[1], coef(simulated.fit)[1]))
    expect_true(are.within.ten.percent.of(coef(fit)[2], coef(simulated.fit)[2]))
    expect_that(fit, is_a("litfit"))
    expect_true(fit$nparams > 0)
    expect_true(fit$optimFit$convergence == 0)
    expect_true(all(predict(fit) > 0))
}) 
