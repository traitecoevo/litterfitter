## internal functions see base functions for models in Manzoni 2012 Table A1

discrete.series<- function(x, R, K1, K2, upper = c((1 - 1e-04), 1000, 1000), lower = c(1e-04, 1e-04, 
    1e-04)) {
    (((1 - R) * K1 * exp(-K2 * x)) - ((K2 - K1 * R) * exp(-K1 * x)))/(K1 - K2)
}

discrete.parallel <- function(t, A, K1, K2, upper = c((1 - 1e-04), 100, 100), lower = c(1e-04, 1e-04, 1e-04)) {
    A * exp(-K1 * t) + (1 - A) * exp(-K2 * t)
}

weibull <- function(x, beta, alpha, upper = c(10, 10), lower = c(1e-04, 1e-04)) {
    exp(-(x/beta)^alpha)
}

neg.exp <- function(x, k, upper = c(5), lower = c(1e-04)) {
    exp(-k * x)
}

# function from greg freschet for testing purposes
neg.exp.limit <- function(x, k, A, B, upper = c(5, 100, 1), lower = c(1e-06, 1e-06, 1e-06)) {
    A * exp(-k * x) + B
}


cont.quality.1 <- function(x, b, a, upper = c(10^10, 100), lower = c(1e-04, 1e-04)) {
    (b^a)/(b + x)^a
}

cont.quality.2 <- function(x, b, a, upper = c(10^10, 100), lower = c(1e-04, 1.0001)) {
    1/((1 + b * x)^a)
}

predict.from.fit <- function(model_fn, params, target_time) {
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

cont.quality.2.steadystate <- function(b, a) {
    1/b * 1/(a - 1)
}


obj_func <- function(x, ind, dep, curve) {
    # print(x)
    try(predicted <- do.call(curve, (c(list(ind), as.list(x)))))
    return(sum((predicted - dep)^2))
}


calculateLL <- function(predicted, observed, sigma) {
    return(sum(dnorm(observed, mean = predicted, sd = sigma, log = T)))
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

multioptimFit <- function(time_data, mass_data, model, iters = 200, ...) {
    nArgs <- length(formals(model)) - 3
    # need this to allow variation in number of parameters (changed to 3 because 'upper' and 'lower' are
    # now formal arguments)
    fit <- list()
    upper_bounds <- eval(as.list(formals(model))$upper)
    lower_bounds <- eval(as.list(formals(model))$lower)
    for (i in 1:iters) {
        starter <- runif(nArgs, min = lower_bounds, max = lower_bounds + 0.9998)
        # always start near lower bound--empirically works better
        fit[[i]] <- tryCatch(optim(starter, obj_func, ind = time_data, dep = mass_data, curve = model, 
            method = "L-BFGS-B", lower = lower_bounds, upper = upper_bounds, ...), error = function(e) NULL)
    }
    successes <- unlist(sapply(fit, function(x) {
        ifelse(is.null(x), return(FALSE), return(x$convergence == 0))
    }))
    cat(paste("Number of successful fits: ", sum(successes), " out of", iters, "\n"))
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

simulate.decomposition.with.error <- function(fit, sigma) {
    mass.remaining <- predict(fit) + rnorm(length(predict(fit)), 0, sigma)
    time <- fit$time
    return(data.frame(mass.remaining = mass.remaining, time = time))
}

are.within.ten.percent.of <- function(x, y) {
    return(y < 1.1 * x & y > 0.9 * x)
} 
