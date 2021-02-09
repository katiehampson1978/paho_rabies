# Create stacked LAC timeseries of annual data since 1981
ycases = read.csv("data/LAC_cases_yr.csv")
y = 1981:2015
length(y)

# Plot png of decline rabies cases throughout LAC
# png("figs/Declines.png", bg = "transparent", units = 'in', res = 300, width=5, height=3.5)
par(mfrow=c(1,1), mgp=c(1.3, 0.4, 0), lwd=.5, cex=0.7, mgp = c(3,2,1))
plot(ycases$yr[-(1:11)], apply(ycases[-(1:11),1:24], 1, sum), col="black",
     ylim=c(0,20000), axes = FALSE, cex=0.5, lwd=1.5, type="l", ylab = "Cases", xlab="")

polygon(c(1981:2015, 2015:1981),
        y = c(apply(ycases[-(1:11),1:24], 1, sum), rep(0, length(y))),
        col="white", border = "black")

for(i in 23:2){
  polygon(c(1981:2015, 2015:1981),
          y = c(apply(ycases[-(1:11),1:i], 1, sum), rep(0, length(y))),
          col = "white", border="black")
}

# WHEN HOVERING OVER EACH COOUNTRY WOULD IT BE POSSIBLE TO HIGHLIGHT THE POLYGON?
# shade black and thicken line width?


axis(2, tck = -0.025, lwd=0.5)
axis(1)
# dev.off()
