# Each Jacobian column is a curve derivative in the model's parameter order.
model_jacobian <- function(model) {
  switch(model,
    neg.exp = function(t, p) cbind(k = -t * exp(-p[1] * t)),
    weibull = function(t, p) {
      beta <- p[1]
      alpha <- p[2]
      u <- (t / beta)^alpha
      weight <- numeric(length(t))
      keep <- is.finite(u) & u > 0
      weight[keep] <- u[keep] * exp(-u[keep])
      d_alpha <- numeric(length(t))
      d_alpha[keep] <- -weight[keep] * (log(t[keep]) - log(beta))
      cbind(beta = weight * alpha / beta, alpha = d_alpha)
    },
    discrete.parallel = function(t, p) {
      first <- exp(-p[2] * t)
      second <- exp(-p[3] * t)
      cbind(A = first - second, K1 = -p[1] * (t * first), K2 = -(1 - p[1]) * (t * second))
    },
    discrete.series = function(t, p) series_eval(t, p[1], p[2], p[3], jacobian = TRUE),
    cont.quality = function(t, p) {
      log_base <- log1p(p[1] * t)
      pred <- exp(-p[2] * log_base)
      d_a <- numeric(length(t))
      keep <- pred != 0
      d_a[keep] <- -log_base[keep] * pred[keep]
      cbind(b = -p[2] * (t / (1 + p[1] * t)) * pred, a = d_a)
    },
    neg.exp.limit = function(t, p) {
      decay <- exp(-p[1] * t)
      cbind(k = -p[2] * (t * decay), A = decay, B = 1)
    },
    NULL
  )
}

make_gradient <- function(model, ind, dep) {
  jac <- model_jacobian(model)
  if (is.null(jac)) return(NULL)
  f <- get(model, envir = topenv(), mode = "function")
  predict <- switch(length(model_param_names(f)),
    function(p) f(ind, p[1]),
    function(p) f(ind, p[1], p[2]),
    function(p) f(ind, p[1], p[2], p[3])
  )
  function(p) drop(2 * crossprod(jac(ind, p), predict(p) - dep))
}
