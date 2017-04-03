cbbPalette <- c("#000000", "#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00", 
                "#CC79A7", "#F0E442")


pdf(file=paste0("doc/ExampleFigure.pdf"),width=5,height=4)
par(mar=c(5,5,1,1))
label_x     <- expression(tau~"(time measured in mutations)")
label_y     <- expression(theta*"="*4*N[e]*mu)


plot(x=c(0,1.4,1.4,5),
     y=c(4,4,40,40),
     type="l",
     log="y",
     lwd=2,
     xlim=c(0,4),
     ylim=c(10^(-3),10^3),
     xlab=label_x,
     ylab=label_y,
     col=cbbPalette[1])

points(x=c(0,1,2,3,4),
       y=c(4,4,40,40,40),
       col=cbbPalette[1])

lines(x=c(0,5),
      y=c(1.5,1.5),
      lwd=2,
      col=cbbPalette[2])

points(x=c(0,1,2,3,4),
       y=c(1.5,1.5,1.5,1.5,1.5),
       col=cbbPalette[2])

lines(x=c(0,0.4,0.4,2.3,2.3,5),
      lwd=2,
      y=c(0.04,0.04,11,11,178,178),
      col=cbbPalette[3])

points(x=c(0,1,2,3,4),
       y=c(0.04,11,11,178,178),
       col=cbbPalette[3])



abline(v=0,lty=2,lwd=0.5)
abline(v=1,lty=2,lwd=0.5)
abline(v=2,lty=2,lwd=0.5)
abline(v=3,lty=2,lwd=0.5)
abline(v=4,lty=2,lwd=0.5)

legend(x="bottomright",
       legend=c("Simulation 1","Simulation 2","Simulation 3"),
       lty=1,lwd=2,
       col=cbbPalette[1:3])


dev.off()
