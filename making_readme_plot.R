png("example_plot.png")
plot_multiple_fits(time=c(0,1,2,3,4,5,6),mass.remaining=c(1,0.9,0.98,0.4,0.6,0.2,0.01),iters=800)
dev.off()