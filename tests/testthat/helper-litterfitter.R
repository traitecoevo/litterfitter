# Shared data and helpers for tests

all_models <- c(
  "neg.exp", "weibull", "discrete.parallel",
  "discrete.series", "cont.quality", "neg.exp.limit"
)

expected_param_names <- list(
  neg.exp = "k",
  weibull = c("beta", "alpha"),
  discrete.parallel = c("A", "K1", "K2"),
  discrete.series = c("R", "K1", "K2"),
  cont.quality = c("b", "a"),
  neg.exp.limit = c("k", "A", "B")
)

# example data used in the README (includes a value above 1)
ex_time <- c(0, 1, 2, 3, 4, 5, 6)
ex_mass <- c(1, 0.9, 1.01, 0.4, 0.6, 0.2, 0.01)

# fit quietly, ignoring the expected "above 1" and boundary warnings
quiet_fit <- function(time, mass, model, iters = 100, ...) {
  suppressWarnings(fit_litter(time, mass, model, iters = iters, quiet = TRUE, ...))
}

# build a litfit object with chosen parameters (for analytic checks)
fake_fit <- function(model, pars, time = 0:5) {
  names(pars) <- expected_param_names[[model]]
  pred <- do.call(model, c(list(time), as.list(unname(pars))), envir = asNamespace("litterfitter"))
  structure(list(
    optimFit = list(par = pars, value = 0.01), logLik = 1, fitAIC = 0, fitAICc = 0,
    fitBIC = 0, time = time, mass = pred, predicted = pred, model = model,
    nparams = length(pars)
  ), class = "litfit")
}

# example element data from the import branch (paired with pineneedles mass)
ex_element <- c(1.1, 1.2, 1.3, 1, 0.8, 0.6)

quiet_element <- function(mass, element, iters = 100, ...) {
  suppressWarnings(fit_element(mass, element, iters = iters, quiet = TRUE, ...))
}

# synthetic element data from known import.model parameters
synthetic_element <- function(a, b, mass = seq(1, 0.1, by = -0.1)) {
  list(mass = mass, element = litterfitter:::import.model(mass, a, b))
}
