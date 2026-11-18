# Base graphics plotting

with_null_device <- function(code) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  force(code)
}

pine_fit <- function(model = "weibull", seed = 1, iters = 50) {
  set.seed(seed)
  quiet_fit(pineneedles$Year, pineneedles$Mass.remaining, model, iters = iters)
}

# ---- model_equation ----

test_that("equal-rate series labels show the limiting curve in both plot formats", {
  fit <- fake_fit("discrete.series", c(0.25, 0.8, 0.8))
  expect_equal(model_equation(fit, "text"), "y = exp(-0.8 t) (1 + 0.6 t)")
  rhs <- model_equation(fit, "plotmath")[[3]]
  t <- c(0, 0.5, 2)
  expect_equal(eval(rhs, list(t = t, e = exp(1))), predict(fit, newdata = t))
})

test_that("model_equation gives text and plotmath for every model", {
  set.seed(1)
  for (m in all_models) {
    fit <- pine_fit(m)
    txt <- litterfitter:::model_equation(fit, "text")
    expect_type(txt, "character")
    expect_length(txt, 1)
    expect_match(txt, "^y = ")
    pm <- litterfitter:::model_equation(fit, "plotmath")
    expect_true(is.call(pm) || is.expression(pm))
  }
  efit <- quiet_element(pineneedles$Mass.remaining, ex_element)
  expect_match(litterfitter:::model_equation(efit, "text"), "a = .*b = ")
  # models without a specific equation fall back to listing parameters
  fake <- efit
  fake$model <- "some.new.model"
  fake$optimFit$par <- c(p = 1, q = 0.25)
  expect_equal(litterfitter:::model_equation(fake, "text"), "p = 1, q = 0.25")
  expect_equal(litterfitter:::model_equation(fake, "plotmath"), "p = 1, q = 0.25")
})

# ---- plot.litfit ----

test_that("plot.litfit works for every model and returns invisibly", {
  with_null_device({
    for (m in all_models) {
      fit <- pine_fit(m)
      expect_invisible(plot(fit))
      expect_silent(plot(fit, formulae.cex = 0.8))
    }
  })
})

test_that("plot.litfit accepts a bootstrap band and user overrides", {
  fit <- pine_fit()
  set.seed(2)
  boot <- suppressWarnings(bootstrap_parameters(fit, nboot = 30))
  with_null_device({
    expect_invisible(plot(fit, boot = boot))
    expect_silent(plot(fit, boot = boot, conf.level = 0.5))
    expect_silent(plot(fit, xlab = "Years", ylab = "M/M0", main = "Custom", pch = 1, col = "red"))
    expect_silent(plot(fit, formulae.cex = NA))
    expect_error(plot(fit, boot = boot, conf.level = 2), "conf.level")
    other <- pine_fit("neg.exp")
    expect_error(plot(other, boot = boot), "generated from")
  })
})

test_that("plot.litfit works for data above 1 and day-scale time", {
  set.seed(1)
  fit <- quiet_fit(c(0, 30, 90, 180, 365), c(1, 1.08, 0.9, 0.7, 0.5), "neg.exp", iters = 100)
  with_null_device(expect_silent(plot(fit)))
})

# ---- plot.elementfit ----

test_that("plot.elementfit works with a band and overrides", {
  set.seed(1)
  efit <- quiet_element(pineneedles$Mass.remaining, ex_element)
  eboot <- suppressWarnings(bootstrap_parameters(efit, nboot = 20))
  with_null_device({
    expect_invisible(plot(efit))
    expect_silent(plot(efit, boot = eboot))
    expect_silent(plot(efit, formulae.cex = NA, main = "N", xlab = "M/M0"))
  })
})

# ---- plot.litfit_list and plot_multiple_fits ----

test_that("plot.litfit_list draws all fits and restores graphical parameters", {
  set.seed(1)
  fits <- suppressWarnings(fit_litter_all(pineneedles$Year, pineneedles$Mass.remaining, iters = 30))
  with_null_device({
    fam <- graphics::par("family")
    expect_invisible(plot(fits))
    expect_identical(graphics::par("family"), fam)
    expect_silent(plot(fits, col = c("red", "blue"), legend.bty = "n", legend.cex = 0.6))
    expect_silent(plot(fits, legend.pos = NA, main = "No legend"))
    expect_silent(plot(fits, legend.pos = "bottomleft"))
    expect_silent(plot(fits[1]))
  })
})

test_that("plot_multiple_fits fits and plots, returning a litfit_list", {
  set.seed(1)
  with_null_device({
    expect_invisible(res <- plot_multiple_fits(
      time = pineneedles$Year, mass.remaining = pineneedles$Mass.remaining,
      model = c("neg.exp", "weibull"), bty = "n", iters = 50
    ))
    expect_s3_class(res, "litfit_list")
    expect_length(res, 2)
    expect_s3_class(res[[1]], "litfit")

    res <- suppressWarnings(plot_multiple_fits(
      time = ex_time, mass.remaining = ex_mass,
      model = c("neg.exp", "weibull", "cont.quality"), color = c("red", "blue", "green"),
      iters = 50, xlab = "Time (y)"
    ))
    expect_length(res, 3)
    expect_error(
      plot_multiple_fits(pineneedles$Year, pineneedles$Mass.remaining, model = "nope", iters = 5),
      "should be one of"
    )
  })
})

test_that("plot_multiple_fits plots existing fits without refitting", {
  set.seed(1)
  fits <- suppressWarnings(fit_litter_all(pineneedles$Year, pineneedles$Mass.remaining,
    models = c("neg.exp", "weibull"), iters = 30
  ))
  with_null_device({
    expect_identical(plot_multiple_fits(fits), fits)
    expect_identical(plot_multiple_fits(unclass(fits)), fits)
    single <- plot_multiple_fits(fits$weibull)
    expect_s3_class(single, "litfit_list")
    expect_named(single, "weibull")
  })
})

# ---- plot.litfit_bootstrap ----

test_that("bootstrap plots select columns by number, name or 'all'", {
  fit <- pine_fit()
  set.seed(2)
  boot <- suppressWarnings(bootstrap_parameters(fit, nboot = 40))
  with_null_device({
    mfrow <- graphics::par("mfrow")
    expect_invisible(plot(boot))
    expect_silent(plot(boot, coef.index = 2))
    expect_silent(plot(boot, coef.index = "steady_state"))
    expect_silent(plot(boot, coef.index = c("beta", "alpha")))
    expect_silent(plot(boot, coef.index = "all"))
    expect_silent(plot(boot, coef.index = 1:3, conf.level = 0.8, main = "Custom title"))
    expect_identical(graphics::par("mfrow"), mfrow)
    expect_error(plot(boot, coef.index = 0), "between 1 and 3")
    expect_error(plot(boot, coef.index = 4), "between 1 and 3")
    expect_error(plot(boot, coef.index = NA_real_), "between 1 and 3")
    expect_error(plot(boot, coef.index = "nope"), "unknown coefficient\\(s\\): nope")
    expect_error(plot(boot, conf.level = 1), "conf.level")
  })
})

test_that("bootstrap plots skip non-finite values and columns", {
  # e.g. neg.exp.limit with a clear asymptote, whose steady state is always Inf
  fit <- pine_fit("neg.exp.limit", iters = 100)
  set.seed(2)
  boot <- suppressWarnings(bootstrap_parameters(fit, nboot = 30))
  boot[, "steady_state"] <- Inf
  boot[1:3, "k"] <- c(NA, Inf, -Inf)
  with_null_device({
    expect_silent(plot(boot, coef.index = "k"))
    expect_error(plot(boot, coef.index = "steady_state"), "Not enough finite")
    expect_warning(plot(boot, coef.index = "all"), "skipping column\\(s\\).*steady_state")
  })
})

# ---- visual snapshots ----

test_that("base plots match snapshots", {
  skip_on_cran()
  skip_on_covr()
  skip_if_not_installed("vdiffr")
  fit <- pine_fit("weibull", iters = 100)
  set.seed(3)
  boot <- suppressWarnings(bootstrap_parameters(fit, nboot = 100))
  set.seed(1)
  fits <- suppressWarnings(fit_litter_all(pineneedles$Year, pineneedles$Mass.remaining,
    models = c("neg.exp", "weibull"), iters = 100
  ))
  set.seed(1)
  efit <- quiet_element(pineneedles$Mass.remaining, ex_element, iters = 200)

  vdiffr::expect_doppelganger("plot-litfit", function() plot(fit))
  vdiffr::expect_doppelganger("plot-litfit-band", function() plot(fit, boot = boot))
  vdiffr::expect_doppelganger("plot-litfit-list", function() plot(fits))
  vdiffr::expect_doppelganger("plot-elementfit", function() plot(efit))
  vdiffr::expect_doppelganger("plot-bootstrap-all", function() plot(boot, coef.index = "all"))
})
