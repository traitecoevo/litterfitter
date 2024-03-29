## internal functions see base functions for models in Manzoni 2012 Table A1


discrete.series <- function(x, R, K1, K2, upper = c((1 - 1e-04), 1000, 1000), lower = c(1e-04, 
    1e-04, 1e-04)) {
    (((1 - R) * K1 * exp(-K2 * x)) - ((K2 - K1 * R) * exp(-K1 * x)))/(K1 - K2)
}

discrete.parallel <- function(t, A, K1, K2, upper = c((1 - 1e-04), 100, 100), lower = c(1e-04, 
    1e-04, 1e-04)) {
    A * exp(-K1 * t) + (1 - A) * exp(-K2 * t)
}


weibull <- function(x, beta, alpha, upper = c(10, 10), lower = c(1e-04, 1e-04)) {
    exp(-(x/beta)^alpha)
}

neg.exp <- function(x, k, upper = c(5), lower = c(1e-04)) {
    exp(-k * x)
}

# function from greg freschet for testing purposes
neg.exp.limit <- function(x, k, A, B, upper = c(5, 100, 1), lower = c(1e-06, 1e-06, 
    1e-06)) {
    A * exp(-k * x) + B
}


cont.quality <- function(x, b, a, upper = c(10^10, 100), lower = c(1e-04, 1.0001)) {
    1/((1 + b * x)^a)
}

#' Predict Using a Specific Model Function
#'
#' This function computes predictions by applying the specified model function (`model_fn`)
#' with provided parameters (`params`) at a target time (`target_time`). It is an internal
#' utility function and is NOT a generic or S3 method.
#'
#' @keywords internal
#' @param model_fn A function representing the model used for prediction.
#' @param params A list of parameters to be passed to the model function.
#' @param target_time A numeric value representing the target time for the prediction.
#' @return Numeric. The predicted value computed by the model function.
predict_from_fit <- function(model_fn, params, target_time) {
    prediction <- do.call(model_fn, c(list(target_time), as.list(params)))
    return(prediction)
}

# Functions for steady state calculation (according to Reviewer 2)
weibull.steadystate <- function(beta, alpha) {
    beta/alpha * gamma(1/alpha)
}

negexp.steadystate <- function(k) {
    1/k
}

discrete.series.steadystate <- function(R, K1, K2) {
    (1/K1) + (1 - R)/K2
}

discrete.parallel.steadystate <- function(A, K1, K2) {
    (A/K1) + (1 - A)/K2
}

cont.quality.steadystate <- function(b, a) {
    1/b * 1/(a - 1)
}


obj_func <- function(x, ind, dep, curve) {
    try(predicted <- do.call(curve, (c(list(ind), as.list(x)))))
    return(sum((predicted - dep)^2))
}


calculateLL <- function(predicted, observed, sigma) {
    return(sum(stats::dnorm(observed, mean = predicted, sd = sigma, log = T)))
}

calculateAIC <- function(LL, nparams) {
    return(-2 * LL + 2 * nparams)
}

calculateAICc <- function(LL, nparams, samplesize) {
    AIC <- calculateAIC(LL, nparams)
    AICc <- AIC + (2 * nparams * (nparams + 1))/(samplesize - nparams - 1)
    return(AICc)
}

calculateBIC <- function(LL, nparams, samplesize) {
    return(-2 * LL + nparams * log(samplesize))
}

multioptimFit <- function(time_data, mass_data, model, iters = 200, upper = NULL, 
    lower = NULL, ...) {
    nArgs <- length(formals(model)) - 3
    # need this to allow variation in number of parameters (changed to 3 because
    # 'upper' and 'lower' are now formal arguments)
    fit <- list()
    ifelse(!is.null(upper), upper_bounds <- upper, upper_bounds <- eval(as.list(formals(model))$upper))
    
    ifelse(!is.null(lower), lower_bounds <- lower, lower_bounds <- eval(as.list(formals(model))$lower))
    
    for (i in 1:iters) {
        starter <- runif(nArgs, min = lower_bounds, max = lower_bounds + 0.9998)
        # always start near lower bound--empirically works better
        fit[[i]] <- tryCatch(stats::optim(starter, obj_func, ind = time_data, dep = mass_data, 
            curve = model, method = "L-BFGS-B", lower = lower_bounds, upper = upper_bounds, 
            ...), error = function(e) NULL)
    }
    successes <- unlist(sapply(fit, function(x) {
        ifelse(is.null(x), return(FALSE), return(x$convergence == 0))
    }))
    message(paste("Number of successful fits: ", sum(successes), " out of", iters, "\n"))
    if (sum(successes) == 0) {
        stop("All attempts failed to converge")
    }
    s.fit <- subset(fit, successes)
    likes <- unlist(sapply(s.fit, function(x) x$value))
    likes[likes == 0] <- NA
    if (sd(sort(likes, decreasing = TRUE)[1:5]) > 0.01) {
        warning("May not have found global best fit; increase iterations")
    }
    best.one <- which.min(likes[likes > 0])
    return(s.fit[[best.one]])
}

are.within.ten.percent.of <- function(x, y) {
    return(y < 1.1 * x & y > 0.9 * x)
}

rnd.to.text <- function(x, digits = 4) {
    format(round(x, digits), scientific = F)
}


#' Simulate and Check Model Fitting with Litter Data
#'
#' This internal function first fits a model to the `pineneedles` data from the `litterfitter` package
#' using the `fit_litter` function. Next, it introduces random noise to the mass remaining data and refits 
#' the model to this perturbed data. The function returns whether the time to proportion mass remaining
#' values from the two fits are within ten percent of each other.
#'
#' @keywords internal
#' @param model A character string representing the model type to be fitted using `fit_litter`.
#' @return Logical. TRUE if the time to proportion mass remaining values from the original and simulated
#' fits are within ten percent of each other; FALSE otherwise.
#' @seealso \code{\link[litterfitter]{fit_litter}}, \code{\link[litterfitter]{pineneedles}}
#' 
simulate_and_check <- function(model) {
    suppressWarnings(fit <- fit_litter(time = litterfitter::pineneedles$Year, mass.remaining = litterfitter::pineneedles$Mass.remaining, 
        model = model, iters = 1000))
    mass.with.error <- litterfitter::pineneedles$Mass.remaining + rnorm(length(predict(fit)), 0, 
        1e-03)
    suppressWarnings(simulated.fit <- fit_litter(time = litterfitter::pineneedles$Year, mass.remaining = mass.with.error, 
        model = model, iters = 1000))
    return(are.within.ten.percent.of(time_to_prop_mass_remaining(fit), time_to_prop_mass_remaining(simulated.fit)))
}

