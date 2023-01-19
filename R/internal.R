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

# functions to model release in nutrients - Parton et al. (2007)
b_func<- function(x,d,e){
  d*(1-exp(e*x))
}

b_func_100<- function(d,e){
  d*(1-exp(e*100))
}

f_func<-function(x,a,b){
  (x/b)/(sqrt((2*a*x/b)^2+(1-(x/b)^2)^2))
}

f_func_100<-function(a,b){
  (100/b)/(sqrt((2*a*100/b)^2+(1-(100/b)^2)^2))
}

import.model <- function(x,a,b,lower = c(0.25, 1),upper = c(10, 100)){
  f_func(x,a,b)/f_func_100(a,b)
}


exp.model <- function(x,a,b, lower = c(1,0.1), upper = c(1000000, 400)){
  a^x + b
}

# Function to model release in nutrients in relation to stoichiometry and consumer carbon efficiency - Manzoni et al. (2008)

stoch.model <- function(x, l, e, lower = c(0.1, 0.01), upper = c(200,200)){
  
  x*(10/l)+(1-(10/l))*x^(1/(1-e))
  
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

cont.quality.steadystate <- function(b, a) {
    1/b * 1/(a - 1)
}


obj_func <- function(x, ind, dep, curve) {
    # print(x)
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

are.within.ten.percent.of <- function(x, y) {
    return(y < 1.1 * x & y > 0.9 * x)
}

rnd.to.text <- function(x, digits = 4) {
    format(round(x, digits), scientific = F)
}

simulate.and.check <- function(model) {
    suppressWarnings(fit <- fit_litter(time = pineneedles$Year, mass.remaining = pineneedles$Mass.remaining, 
        model = model, iters = 1000))
    mass.with.error <- pineneedles$Mass.remaining + rnorm(length(predict(fit)), 0, 
        1e-03)
    suppressWarnings(simulated.fit <- fit_litter(time = pineneedles$Year, mass.remaining = mass.with.error, 
        model = model, iters = 1000))
    return(are.within.ten.percent.of(time_to_prop_mass_remaining(fit), time_to_prop_mass_remaining(simulated.fit)))
}

weibull.df <- function(x, repetition, obs.time){
  
  # Run bootstrap and extract parameters for each repeated bootstrap
  bootmat<-bootstrap_parameters(x, nboot=repetition)
  alpha <- bootmat[,2]
  beta <- bootmat[,1]
  
  bootfin <- as.data.frame(cbind(alpha, beta)) %>% 
    mutate(grouped_num = 1:repetition) %>% 
    dplyr::filter(alpha < as.numeric(quantile(bootmat[,2], 0.95)) & alpha > as.numeric(quantile(bootmat[,2], 0.05))) %>% 
    dplyr::filter(beta < as.numeric(quantile(bootmat[,1], 0.95)) & beta > as.numeric(quantile(bootmat[,1], 0.05))) %>% 
    add_row(alpha = median(bootmat[,2]), beta = median(bootmat[,1]), grouped_num = repetition+1)
  
  time.vec <- rep(seq(0, max(obs.time), 0.1), repetition+1)
  grouped_num <- rep(seq(1,repetition+1, 1), each = ((max(obs.time)/0.1)+1))
  
  final_df <- as.data.frame(cbind(time.vec, grouped_num)) %>% 
    left_join(bootfin, by = "grouped_num") %>% 
    rowwise() %>% 
    mutate(pred.val = exp(-(time.vec/beta)^alpha))
  
  return(final_df)
  
}

neg.exp.df <- function(x, repetition, obs.time){
  
  # Run bootstrap and extract parameters for each repeated bootstrap
  bootmat<-bootstrap_parameters(x, nboot=repetition)
  k <- bootmat[,1]
  
  bootfin <- as.data.frame(k) %>% 
    mutate(grouped_num = 1:repetition) %>% 
    dplyr::filter(k < as.numeric(quantile(bootmat[,1], 0.95)) & k > as.numeric(quantile(bootmat[,1], 0.05))) %>% 
    add_row(k = median(bootmat[,1]), grouped_num = repetition+1)
  
  time.vec <- rep(seq(0, max(obs.time), 0.1), repetition+1)
  grouped_num <- rep(seq(1,repetition+1, 1), each = ((max(obs.time)/0.1)+1))
  
  final_df <- as.data.frame(cbind(time.vec, grouped_num)) %>% 
    left_join(bootfin, by = "grouped_num") %>% 
    rowwise() %>% 
    mutate(pred.val = exp(-k * time.vec))
  
  return(final_df)
  
}

discrete.series.df <- function(x, repetition, obs.time){
  
  # Run bootstrap and extract parameters for each repeated bootstrap
  bootmat<-bootstrap_parameters(x, nboot=repetition)
  R <- bootmat[,1]
  K1 <- bootmat[,2]
  K2 <- bootmat[,3]
  
  
  bootfin <- as.data.frame(cbind(R, K1, K2))  %>% 
    mutate(grouped_num = 1:repetition) %>% 
    dplyr::filter(R < as.numeric(quantile(bootmat[,1], 0.95)) & R > as.numeric(quantile(bootmat[,1], 0.05))) %>% 
    dplyr::filter(K1 < as.numeric(quantile(bootmat[,2], 0.95)) & K1 > as.numeric(quantile(bootmat[,2], 0.05))) %>% 
    dplyr::filter(K2 < as.numeric(quantile(bootmat[,3], 0.95)) & K2 > as.numeric(quantile(bootmat[,3], 0.05))) %>% 
  add_row(R = median(bootmat[,1]), K1 = median(bootmat[,2]), K2 = median(bootmat[,3]),  grouped_num = repetition+1)
  
  time.vec <- rep(seq(0, max(obs.time), 0.1), repetition+1)
  grouped_num <- rep(seq(1,repetition+1, 1), each = ((max(obs.time)/0.1)+1))
  
  final_df <- as.data.frame(cbind(time.vec, grouped_num)) %>% 
    left_join(bootfin, by = "grouped_num") %>% 
    rowwise() %>% 
    mutate(pred.val = (((1 - R) * K1 * exp(-K2 * time.vec)) - ((K2 - K1 * R) * exp(-K1 * time.vec)))/(K1 - K2))
  
  return(final_df)
  
}

discrete.parallel.df <- function(x, repetition, obs.time){
  
  # Run bootstrap and extract parameters for each repeated bootstrap
  bootmat<-bootstrap_parameters(x, nboot=repetition)
  A <- bootmat[,1]
  K1 <- bootmat[,2]
  K2 <- bootmat[,3]
  
  
  bootfin <- as.data.frame(cbind(A, K1, K2))  %>% 
    mutate(grouped_num = 1:repetition) %>% 
    dplyr::filter(A < as.numeric(quantile(bootmat[,1], 0.95)) & A > as.numeric(quantile(bootmat[,1], 0.05))) %>% 
    dplyr::filter(K1 < as.numeric(quantile(bootmat[,2], 0.95)) & K1 > as.numeric(quantile(bootmat[,2], 0.05))) %>% 
    dplyr::filter(K2 < as.numeric(quantile(bootmat[,3], 0.95)) & K2 > as.numeric(quantile(bootmat[,3], 0.05))) %>% 
  add_row(A = median(bootmat[,1]), K1 = median(bootmat[,2]), K2 = median(bootmat[,3]),  grouped_num = repetition+1)
  
  time.vec <- rep(seq(0, max(obs.time), 0.1), repetition+1)
  grouped_num <- rep(seq(1,repetition+1, 1), each = ((max(obs.time)/0.1)+1))
  
  final_df <- as.data.frame(cbind(time.vec, grouped_num)) %>% 
    left_join(bootfin, by = "grouped_num") %>% 
    rowwise() %>% 
    mutate(pred.val = A * exp(-K1 * time.vec) + (1 - A) * exp(-K2 * time.vec))
  
  return(final_df)
}

cont.quality.df <- function(x, repetition, obs.time){
  
  # Run bootstrap and extract parameters for each repeated bootstrap
  bootmat<-bootstrap_parameters(x, nboot=repetition)
  a <- bootmat[,2]
  b <- bootmat[,1]
  
  bootfin <- as.data.frame(cbind(a, b)) %>% 
    mutate(grouped_num = 1:repetition) %>% 
    dplyr::filter(a < as.numeric(quantile(bootmat[,2], 0.95)) & a > as.numeric(quantile(bootmat[,2], 0.05))) %>% 
    dplyr::filter(b < as.numeric(quantile(bootmat[,1], 0.95)) & b > as.numeric(quantile(bootmat[,1], 0.05))) %>% 
    add_row(a = median(bootmat[,2]), b = median(bootmat[,1]), grouped_num = repetition+1)
  time.vec <- rep(seq(0, max(obs.time), 0.1), repetition+1)
  grouped_num <- rep(seq(1,repetition+1, 1), each = ((max(obs.time)/0.1)+1))
  
  final_df <- as.data.frame(cbind(time.vec, grouped_num)) %>% 
    left_join(bootfin, by = "grouped_num") %>% 
    rowwise() %>% 
    mutate(pred.val = 1/((1 + b * time.vec)^a))
  
  return(final_df)
}

import.model.df <- function(x, repetition, obs.time){
  
  # Run bootstrap and extract parameters for each repeated bootstrap
  bootmat<-bootstrap_parameters_element(x, nboot=repetition)
  a <- bootmat[,1]
  b <- bootmat[,2]
  
  bootfin <- as.data.frame(cbind(a, b)) %>% 
    mutate(grouped_num = 1:repetition) %>% 
    dplyr::filter(a < as.numeric(quantile(bootmat[,2], 0.95)) & a > as.numeric(quantile(bootmat[,2], 0.05))) %>% 
    dplyr::filter(b < as.numeric(quantile(bootmat[,1], 0.95)) & b > as.numeric(quantile(bootmat[,1], 0.05))) %>% 
    add_row(a = median(bootmat[,2]), b = median(bootmat[,1]), grouped_num = repetition+1)
  time.vec <- rep(seq(0, max(obs.time), 0.1), repetition+1)
  grouped_num <- rep(seq(1,repetition+1, 1), each = ((max(obs.time)/0.1)+1))
  
  final_df <- as.data.frame(cbind(time.vec, grouped_num)) %>% 
    left_join(bootfin, by = "grouped_num") %>% 
    rowwise() %>% 
    mutate(pred.val = 1/((1 + b * time.vec)^a))
  
  return(final_df)
}