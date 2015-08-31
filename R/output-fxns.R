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
    plot(x$mass ~ x$time, pch = 16, xlab = "Time", ylab = "Propotion mass remaining", xlim = c(0, max(x$time)), 
        main = x$model, ...)
    mod <- eval(parse(text = paste("litterfitter:::", x$model, sep = "")))
    lines(seq(0, max(x$time), 0.01), do.call(mod, c(list(seq(0, max(x$time), 0.01)), as.list(x$optimFit$par))))
    pt.pos = c(grconvertX(0.5, from = "npc"), grconvertY(0.95, from = "npc"))
    
    tmp = switch(x$model, neg.exp = sprintf("text(%f,%f, expression(paste(y==e^\"-%.4f t\")),cex=%f)", 
        pt.pos[1], pt.pos[2], x$optimFit$par, formulae.cex), weibull = sprintf("text(%f,%f, expression(paste(y==e^-(t/%.4f)^%.4f)),cex=%f)", 
        pt.pos[1], pt.pos[2], x$optimFit$par[1], x$optimFit$par[2], formulae.cex), discrete.parallel = sprintf("text(%f,%f, expression(paste(y==%s(\"%.4fe\"^-'%.4f t'%s\"%.4fe\"^-'%.4f t')/%.4f)),cex=%f)", 
        pt.p = pt.pos[1], pt.pos[2], ifelse(x$optimFit$par[2] - x$optimFit$par[3] > 0, "", "-"), (1 - 
            x$optimFit$par[1]) * x$optimFit$par[2], x$optimFit$par[3], ifelse(x$optimFit$par[3] - x$optimFit$par[2] < 
            0, "+", "-"), abs(x$optimFit$par[3] - x$optimFit$par[2] * x$optimFit$par[1]), x$optimFit$par[2], 
        abs(x$optimFit$par[2] - x$optimFit$par[3]), formulae.cex), discrete.series = sprintf("text(%f,%f, expression(paste(y==\"%.4fe\"^-'%.4f t'+\"%.4fe\"^-'%.4f t')),cex=%f)", 
        pt.pos[1], pt.pos[2], x$optimFit$par[1], x$optimFit$par[2], 1 - x$optimFit$par[1], x$optimFit$par[3], 
        formulae.cex), cont.quality.1 = sprintf("text(%f,%f, expression(paste(y==%.4f^%.4f*(%.4f+t)^%.4f)),cex=%f)", 
        pt.pos[1], pt.pos[2], x$optimFit$par[1], x$optimFit$par[2], x$optimFit$par[1], x$optimFit$par[2] * 
            (-1), formulae.cex), cont.quality.2 = sprintf("text(%f,%f, expression(paste(y==\"(1%s%.4f t)\"^%.4f)),cex=%f)", 
        pt.pos[1], pt.pos[2], ifelse(x$optimFit$par[1] > 0, "+", "-"), abs(x$optimFit$par[1]), x$optimFit$par[2] * 
            (-1), formulae.cex), neg.exp.limit = sprintf("text(%f,%f, expression(paste(y==\"%.4fe\"^-'%.4f t' %s %.4f)),cex=%f)", 
        pt.pos[1], pt.pos[2], x$optimFit$par[2], x$optimFit$par[1], ifelse(x$optimFit$par[3] > 
            0, "+", "-"), abs(x$optimFit$par[3]), formulae.cex))
    
    writeLines(tmp, "abcdefg.r")
    source("abcdefg.r")
    invisible(file.remove("abcdefg.r"))
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
steady_state <- function(x, ...) {
    if (class(x) != "litfit") {
        message("Something went wrong -- litterfitter::steady_state takes a 'litfit' object")
        return(NULL)
    }
    out <- switch(x$model, neg.exp = negexp.steadystate(x$optimFit$par), weibull = weibull.steadystate(x$optimFit$par[1], 
        x$optimFit$par[2]), discrete.parallel = discrete.parallel.steadystate(x$optimFit$par[1], x$optimFit$par[2], 
        x$optimFit$par[3]), discrete.series = discrete.series.steadystate(x$optimFit$par[1], x$optimFit$par[2], 
        x$optimFit$par[3]), cont.quality.2 = cont.quality.2.steadystate(x$optimFit$par[1], x$optimFit$par[2]), 
        cont.quality.1 = "not yet implemented", neg.exp.limit = "not yet implemented")
    names(out) <- x$model
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
##' plot_multiple_fits(time = pineneedles$Year, mass.remaining = pineneedles$Mass.remaining, 
##'                 bty = 'n')
##'
##' plot_multiple_fits(time = pineneedles$Year, mass.remaining = pineneedles$Mass.remaining, 
##' bty = 'n', model = c('neg.exp', 'weibull'), xlab = 'Time', ylab = 'Proportion mass remaining') 
##'   
##' 
##' @export plot_multiple_fits
plot_multiple_fits <- function(time, mass.remaining, model = c("neg.exp", "weibull", "discrete.parallel", 
    "discrete.series", "cont.quality.1", "cont.quality.2", "neg.exp.limit"), color = NULL, iters = 500, 
    bty = "o", ...) {
    
    fixed_string_width <- function(string) {
        empty.char <- strwidth(" ", units = "inch")
        value <- strwidth(string, units = "inch")
        diff <- max(value) - value
        number <- round(diff/empty.char, 0)
        return(lapply(1:length(string), function(i) paste(string[i], paste(rep(" ", number[i]), collapse = ""), 
            sep = "")))
    }
    
    mod.lst <- lapply(model, function(x) fit_litter(time = time, mass.remaining = mass.remaining, model = x, 
        iters = iters))
    
    N <- length(model)
    if (is.null(color)) 
        color <- palette()[1:N]
    plot(time, mass.remaining, pch = 19, ylim = c(0, 1), ...)
    
    for (i in 1:N) {
        mod <- eval(parse(text = paste("litterfitter:::", mod.lst[[i]]$model, sep = "")))
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
