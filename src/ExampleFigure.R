cbbPalette <- c("#000000", "#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00", 
                "#CC79A7", "#F0E442")


pdf(file=paste0("doc/ExampleFigure.pdf"),width=5,height=4)
par(mar=c(5,5,1,1))


plot(x=c(0,1.4,1.4,5),
     y=c(log10(4),log10(4),log10(40),log10(40)),
     type="l",
     lwd=2,
     xlim=c(0,4),
     ylim=c(-3,3),
     xlab="time (mutations)",
     ylab=expression(log[10](theta)),
     col=cbbPalette[1])

lines(x=c(0,5),
      y=c(log10(1.5),log10(1.5)),
      lwd=2,
      col=cbbPalette[2])

lines(x=c(0,0.4,0.4,2.3,2.3,5),
      lwd=2,
      y=c(log10(0.04),log10(0.04),log10(11),log10(11),log10(178),log10(178)),
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
