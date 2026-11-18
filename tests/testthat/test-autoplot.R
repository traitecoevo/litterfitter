# ggplot2 autoplot() methods

skip_if_not_installed("ggplot2")

builds <- function(p) {
  expect_s3_class(p, "ggplot")
  expect_no_error(suppressWarnings(ggplot2::ggplot_build(p)))
}

test_that("autoplot works for every model, with and without a band", {
  for (m in all_models) {
    set.seed(1)
    fit <- quiet_fit(pineneedles$Year, pineneedles$Mass.remaining, m, iters = 50)
    builds(ggplot2::autoplot(fit))
    boot <- suppressWarnings(bootstrap_parameters(fit, nboot = 15))
    p <- ggplot2::autoplot(fit, boot = boot)
    builds(p)
    expect_true(any(vapply(p$layers, function(l) inherits(l$geom, "GeomRibbon"), logical(1))))
  }
})

test_that("autoplot.litfit options and validation", {
  set.seed(1)
  fit <- quiet_fit(pineneedles$Year, pineneedles$Mass.remaining, "neg.exp", iters = 20)
  p <- ggplot2::autoplot(fit)
  expect_equal(p$labels$title, "neg.exp")
  expect_match(p$labels$subtitle, "^y = exp\\(-")
  expect_null(ggplot2::autoplot(fit, equation = FALSE)$labels$subtitle)
  expect_equal(nrow(ggplot2::autoplot(fit, n = 50)$data), 50)
  for (bad in list(1, 0, NA, "a", c(10, 20))) {
    expect_error(ggplot2::autoplot(fit, n = bad), "n must be")
  }
  expect_error(ggplot2::autoplot(fit, boot = list()), "litfit_bootstrap")
  # the result can be modified like any ggplot
  builds(p + ggplot2::theme_bw() + ggplot2::labs(x = "Years"))
})

test_that("autoplot works for element fits", {
  set.seed(1)
  efit <- quiet_element(pineneedles$Mass.remaining, ex_element)
  p <- ggplot2::autoplot(efit)
  builds(p)
  expect_match(p$labels$subtitle, "peak element remaining")
  eboot <- suppressWarnings(bootstrap_parameters(efit, nboot = 15))
  builds(ggplot2::autoplot(efit, boot = eboot))
  builds(ggplot2::autoplot(eboot))
})

test_that("autoplot.litfit_list orders and labels models by AICc", {
  set.seed(1)
  fits <- suppressWarnings(fit_litter_all(pineneedles$Year, pineneedles$Mass.remaining, iters = 30))
  p <- ggplot2::autoplot(fits)
  builds(p)
  lev <- levels(p$data$fit)
  g <- glance(fits)
  expect_equal(sub(" .*", "", lev), g$fit)
  expect_match(lev[1], "delta AICc 0.0")
  # colours follow list order (as in the base plot), not AICc rank
  expected <- litterfitter:::okabe_ito(length(fits))[match(g$fit, names(fits))]
  built <- ggplot2::layer_data(p, 1)
  first_col <- unique(built$colour[built$group == 1])
  expect_equal(first_col, expected[1])

  set.seed(2)
  boots <- lapply(fits[c("neg.exp", "weibull")], function(f) {
    suppressWarnings(bootstrap_parameters(f, nboot = 10))
  })
  builds(ggplot2::autoplot(fits[c("neg.exp", "weibull")], boot = boots))
  expect_error(ggplot2::autoplot(fits, boot = boots), "named list")
  # more models than palette colours
  many <- as_litfit_list(rep(unclass(fits), 2))
  builds(ggplot2::autoplot(many))
})

test_that("autoplot.litfit_bootstrap shows selected terms and skips non-finite values", {
  set.seed(1)
  fit <- quiet_fit(pineneedles$Year, pineneedles$Mass.remaining, "weibull", iters = 50)
  boot <- suppressWarnings(bootstrap_parameters(fit, nboot = 40))
  p <- ggplot2::autoplot(boot)
  builds(p)
  expect_equal(levels(p$data$term), c("beta", "alpha", "steady_state"))
  p <- ggplot2::autoplot(boot, terms = "alpha")
  expect_equal(levels(p$data$term), "alpha")
  expect_error(ggplot2::autoplot(boot, terms = c("alpha", "nope")), "unknown term\\(s\\): nope")
  expect_error(ggplot2::autoplot(boot, conf.level = 0), "conf.level")
  expect_match(ggplot2::autoplot(boot, conf.level = 0.9)$labels$subtitle, "central 90%")

  # a term whose values are all infinite is dropped
  b2 <- boot
  b2[, "steady_state"] <- Inf
  expect_equal(levels(ggplot2::autoplot(b2)$data$term), c("beta", "alpha"))
  b3 <- b2
  b3[, ] <- NA
  expect_error(ggplot2::autoplot(b3), "Not enough finite")
})

test_that("autoplot methods are registered for ggplot2's generic", {
  expect_true(all(c("autoplot.litfit", "autoplot.elementfit", "autoplot.litfit_list",
    "autoplot.litfit_bootstrap") %in% as.character(utils::methods(ggplot2::autoplot))))
})

test_that("autoplot output matches snapshots", {
  skip_on_cran()
  skip_on_covr()
  skip_if_not_installed("vdiffr")
  set.seed(1)
  fit <- quiet_fit(pineneedles$Year, pineneedles$Mass.remaining, "weibull", iters = 100)
  set.seed(3)
  boot <- suppressWarnings(bootstrap_parameters(fit, nboot = 100))
  set.seed(1)
  fits <- suppressWarnings(fit_litter_all(pineneedles$Year, pineneedles$Mass.remaining,
    models = c("neg.exp", "weibull"), iters = 100
  ))
  set.seed(1)
  efit <- quiet_element(pineneedles$Mass.remaining, ex_element, iters = 200)

  vdiffr::expect_doppelganger("autoplot-litfit", ggplot2::autoplot(fit))
  vdiffr::expect_doppelganger("autoplot-litfit-band", ggplot2::autoplot(fit, boot = boot))
  vdiffr::expect_doppelganger("autoplot-litfit-list", ggplot2::autoplot(fits))
  vdiffr::expect_doppelganger("autoplot-elementfit", ggplot2::autoplot(efit))
  vdiffr::expect_doppelganger("autoplot-bootstrap", ggplot2::autoplot(boot))
})
