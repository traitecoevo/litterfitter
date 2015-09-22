##' Plot a fit of a curve to a litter decomposition trajectory
##' 
##' @title Plot decomposition trajectory and curve fit
##' 
##' @usage \method{plot}{litfit}(x,formulae.cex,...)
##' 
##' @param x litfit object
##' 
##' @param formulae.cex how big do you want your formula?
##' 
##' @param ... additional arguments passed to plot.default
##' 
##' @details The data and the line fit plotted from a litFit object.  Designed to give a quick visual check if model fitting is adequate.
##' 
##' @seealso \code{\link{fit_litter}}
##' 
##' @author Will Cornwell
##' 
##' @examples fit<-fit_litter(time=c(0,1,2,3,4,5,6),mass.remaining=c(1,0.9,1.01,0.4,0.6,0.2,0.01),
##' 'neg.exp',iters=250)
##' plot(fit)
##' 
##' @export

plot.litfit <- function(x, formulae.cex = 1, ...) {
    plot(x$mass ~ x$time, pch = 16, xlab = "Time", ylab = "Proportion mass remaining", 
        xlim = c(0, max(x$time)), main = x$model, ...)
    mod <- get(x$model)
    time.vec <- seq(0, max(x$time), 0.01)
    lines(time.vec, do.call(mod, c(list(time.vec), as.list(x$optimFit$par))))
    pt.pos <- c(grconvertX(0.5, from = "npc"), grconvertY(0.95, from = "npc"))
    
    formula.text <- switch(x$model, neg.exp = substitute(paste(y == e^A), list(A = paste("-", 
        rnd.to.text(x$optimFit$par, 3), "t", sep = ""))), weibull = substitute(paste(y == 
        e^frac(-t, A)^B), list(A = round(x$optimFit$par[1], 3), B = round(x$optimFit$par[2], 
        3))), discrete.parallel = substitute(paste(y == A * e^{
        B * t
    } + C * e^{
        D * t
    }), list(A = rnd.to.text(x$optimFit$par[1], 4), B = rnd.to.text(-1 * x$optimFit$par[2], 
        4), C = rnd.to.text(1 - x$optimFit$par[1], 4), D = rnd.to.text(-1 * x$optimFit$par[3], 
        4))), discrete.series = substitute(paste(y == frac(A * e^{
        C * t
    } * sign * D * e^{
        F * t
    }, G)), list(A = rnd.to.text((1 - x$optimFit$par[1]) * x$optimFit$par[2]), C = rnd.to.text(-1 * 
        x$optimFit$par[3]), D = rnd.to.text(x$optimFit$par[3] - x$optimFit$par[2] * 
        x$optimFit$par[1]), F = rnd.to.text(-1 * x$optimFit$par[2]), G = x$optimFit$par[2] - 
        x$optimFit$par[3], sign = ifelse(x$optimFit$par[3] - x$optimFit$par[2] * 
        x$optimFit$par[1] > 0, "-", ""))), cont.quality.1 = substitute(paste(y == 
        frac((B^A), (B + t)^A)), list(A = rnd.to.text(x$optimFit$par[2]), B = rnd.to.text(x$optimFit$par[1]))), 
        cont.quality.2 = substitute(paste(y == frac(1, (1 + B * t)^A)), list(A = rnd.to.text(x$optimFit$par[2]), 
            B = rnd.to.text(x$optimFit$par[1]))), neg.exp.limit = substitute(paste(y == 
            A * e^{
                -K * t
            } + B), list(K = rnd.to.text(x$optimFit$par[1]), A = rnd.to.text(x$optimFit$par[2]), 
            B = rnd.to.text(x$optimFit$par[3]))))
    
    text(pt.pos[1], pt.pos[2], label = formula.text, cex = formulae.cex)
}

#' @export
coef.litfit <- function(object, ...) {
    # add names of parameters to output
    return(object$optimFit$par)
}

#' @export
fitted.litfit <- function(object, ...) {
    return(object$predicted)
}

#' @export

summary.litfit <- function(object, ...) {
    # prototype, to be expanded and improved
    cat("Summary of litFit object\n")
    cat(paste("Model type:", object$model, "\n"))
    cat(paste("Number of observations: ", length(object$time), "\n"))
    cat(paste("AIC: ", round(object$fitAIC, 4), "\n"))
    cat(paste("AICc: ", round(object$fitAICc, 4), "\n"))
    cat(paste("BIC: ", round(object$fitBIC, 4), "\n"))
}

##' Generated predicted values for (new) time points from a litfit model fit
##' 
##' @title Predict method for litfit objects
##' 
##' @usage \method{predict}{litfit}(object,newdata=NULL,...)
##' 
##' @param object litfit object
##' 
##' @param newdata optional vector of new Time points at which to predict mass remaining. If not specified, Time points from the original fit are used.
##' 
##' @param ... further arguments passed to or from other methods.
##' 
##' @details to do
##' 
##' @seealso \code{\link{fit_litter}}
##' 
##' @author Will Cornwell
##' @author James Weedon
##' 
##' @examples fit<-fit_litter(time=c(0,1,2,3,4,5,6),mass.remaining=c(1,0.9,1.01,0.4,0.6,0.2,0.01),
##' 'neg.exp',iters=250)
##' predict(fit, newdata=1:10)
##' 
##' @export
predict.litfit <- function(object, newdata = NULL, ...) {
    if (is.null(newdata)) {
        X <- object$time
    } else X <- newdata
    
    mod <- get(object$model)
    
    predicted_values <- do.call(mod, c(list(X), as.list(object$optimFit$par)))
    return(predicted_values)
}



##' Estimate the steady state biomass as a proportion of the annual input,
##' based on the particular model fit.
##' 
##' @title Steady-state estimating from a lit fit object
##' 
##' @usage steady_state(x,...)
##' 
##' @param x litfit object
##' 
##' @param ... additional parameters
##' 
##' @details Right now only implemented for a subset of models.  More coming soon...
##' 
##' @seealso \code{\link{fit_litter}}
##' 
##' @author Will Cornwell
##' 
##' @examples fit<-fit_litter(time=c(0,1,2,3,4,5,6),mass.remaining=c(1,0.9,1.01,0.4,0.6,0.2,0.01),
##' 'neg.exp',iters=250)
##' steady_state(fit)
##' 
##' 
##' @export steady_state
steady_state <- function(x=NULL,pars=NULL,model=NULL) {
    if (is.null(x) & (class(model)!="character" | class(pars) != "numeric")) {
        stop("Something went wrong -- litterfitter::steady_state requires either a litfit
             object, or a model name and the appropriate numeric vector of parameter values")
    }
  if(class(x) == "litfit"){
    model <- x$model
    pars <- x$optimFit$par
  }
    
    out <- switch(model,
                  neg.exp = negexp.steadystate(pars),
                  weibull = weibull.steadystate(pars[1],pars[2]),
                  discrete.parallel = discrete.parallel.steadystate(pars[1],pars[2],pars[3]),
                  discrete.series = discrete.series.steadystate(pars[1],pars[2],pars[3]),
                  cont.quality.2 = cont.quality.2.steadystate(pars[1],pars[2]),
                  cont.quality.1 = "not yet implemented",
                  neg.exp.limit = "not yet implemented")
    names(out) <- model
    return(out)
}


##' Plot multiple fits of decomposition trajectories on one graph with model selection results displayed
##' 
##' @title Plot multiple fits on one graph with model selection results displayed
##' 
##' @usage plot_multiple_fits(time,mass.remaining,model,color,iters,bty,...)
##' 
##' @param time vector of time points
##' 
##' @param mass.remaining vector of mass remaining
##' 
##' @param model vector of models to fit and plot (see \code{\link{fit_litter}})
##' 
##' @param color a vector of colors the same length as the number of models
##' 
##' @param iters parameter passed to \code{\link{fit_litter}}
##' 
##' @param bty bty
##' 
##' @param ... additional parameters passed to \code{\link{plot}}
##' 
##' @details this function is designed to compare a variety of curve shapes visually and with AIC and BIC simultaneously
##' 
##' @seealso \code{\link{fit_litter}} \code{\link{plot.litfit}}
##' 
##' @author Liu Guofang
##' 
##' @examples data(pineneedles,package='litterfitter')
##'
##' plot_multiple_fits(time = pineneedles$Year, 
##' mass.remaining = pineneedles$Mass.remaining, 
##' bty = 'n', model = c('neg.exp', 'weibull'), 
##' xlab = 'Time', ylab = 'Proportion mass remaining',iters=1000) 
##'   
##' 
##' @export plot_multiple_fits
plot_multiple_fits <- function(time, mass.remaining, model = c("neg.exp", "weibull", 
    "discrete.parallel", "discrete.series", "cont.quality.1", "cont.quality.2", "neg.exp.limit"), 
    color = NULL, iters = 500, bty = "o", ...) {
    
    fixed_string_width <- function(string) {
        empty.char <- strwidth(" ", units = "inch")
        value <- strwidth(string, units = "inch")
        diff <- max(value) - value
        number <- round(diff/empty.char, 0)
        return(lapply(1:length(string), function(i) paste(string[i], paste(rep(" ", 
            number[i]), collapse = ""), sep = "")))
    }
    
    mod.lst <- lapply(model, function(x) fit_litter(time = time, mass.remaining = mass.remaining, 
        model = x, iters = iters))
    
    N <- length(model)
    if (is.null(color)) 
        color <- palette()[1:N]
    plot(time, mass.remaining, pch = 19, ylim = c(0, 1), ...)
    
    for (i in 1:N) {
        mod <- get(mod.lst[[i]]$model)
        lines(seq(0, max(mod.lst[[i]]$time), 0.01), do.call(mod, c(list(seq(0, max(mod.lst[[i]]$time), 
            0.01)), as.list(mod.lst[[i]]$optimFit$par))), col = color[i])
    }
    
    values <- plyr::ldply(mod.lst, function(x) cbind(AIC = round(x$fitAIC, 2), BIC = round(x$fitBIC, 
        2)))
    values <- data.frame(model, values)
    values <- values[order(values$AIC), ]
    legend("topright", lty = c(NA, rep(1, N)), legend = sprintf("%s %s  %s", fixed_string_width(c("", 
        as.character(values$model))), fixed_string_width(c("AIC", values$AIC)), fixed_string_width(c("BIC", 
        values$BIC))), col = c(NA, color[as.numeric(row.names(values))]), bty = bty)
}



##' Get estimated time to 0.5 (or an alternate threshold) mass loss from a particular fit to a litter decomposition trajectory
##' 
##' 
##' @title Get the predicted time until half mass loss for a litter decomposition trajectory
##' 
##' @usage time_to_prop_mass_loss(x,threshold.mass=0.5)
##' 
##' @param x a litfit object
##' 
##' @param threshold.mass mass loss threshold in proportion, default is 0.5
##' 
##' @details this function finds the time to a specified mass loss percentage
##' 
##' @seealso \code{\link{fit_litter}} \code{\link{plot.litfit}}
##' 
##' @author Will Cornwell 
##' 
##' @examples 
##'
##'  fit<-fit_litter(time=pineneedles$Year,mass.remaining=pineneedles$Mass.remaining,
##'  model='neg.exp',iters=1000)
##'  time_to_prop_mass_loss(fit)
##' 
##' @export 
##' 
time_to_prop_mass_loss <- function(x, threshold.mass = 0.5) {
    if (class(x) != "litfit") {
        stop("Something went wrong -- litterfitter::steady_state takes a 'litfit' object")
    }
    mod <- get(x$model)
    time.vec <- seq(0, max(x$time), 1e-04)
    mass.predict <- do.call(mod, c(list(time.vec), as.list(x$optimFit$par)))
    if (all(mass.predict < threshold.mass)) {
        stop("Not predicted to reach threshold mass within the time range.")
    }
    return(min(time.vec[mass.predict < threshold.mass]))
} 
