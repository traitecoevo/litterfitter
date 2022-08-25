
#old system
data(pineneedles)
fit<-fit_litter(time=pineneedles$Year,mass.remaining=pineneedles$Mass.remaining,
                model='weibull',iters=5000)
plot(fit)


#modelling import
fake<-data.frame(percent_mass_remaining=seq(from=100,to=10,by=-10),fraction_N=c(1,1.2,1.3,1.1,1,0.9,0.7,0.6,0.4,0.2))

fit<-fit_litter(time=fake$percent_mass_remaining,mass.remaining=fake$fraction_N,
                model='import.model',iters=10000)
plot(fit)

#a and b fits
fit$optimFit$par
