# Load results file
load("/home/miguel/Work/Research/ABC_Skyline_plot/Real_datasets/WhaleShark/Requin/Requin_results.RData")
WhaleSharkSky <- skylineplot
WhaleSharkTest <- test_constant_model
WhaleSharkPGSM <- list(pGSM_hat=pGSM_hat,pGSM_95HPD=pGSM_95HPD)

load("/home/miguel/Work/Research/ABC_Skyline_plot/Real_datasets/Leatherback/Leatherback/Leatherback_results.RData")
LeatherbackSky <- skylineplot
LeatherbackTest <- test_constant_model
LeatherbackPGSM <- list(pGSM_hat=pGSM_hat,pGSM_95HPD=pGSM_95HPD)

load("/home/miguel/Work/Research/ABC_Skyline_plot/Real_datasets/Colobus2/Colobus_BWC_SMM/Colobus_BWC_SMM_results.RData")
BlackNWhiteColobusSky <- skylineplot
BlackNWhiteColobusTest <- test_constant_model
BlackNWhiteColobusPGSM <- list(pGSM_hat=pGSM_hat,pGSM_95HPD=pGSM_95HPD)

load("/home/miguel/Work/Research/ABC_Skyline_plot/Real_datasets/Colobus2/Colobus_RC_SMM/Colobus_RC_SMM_results.RData")
RedColobusSky <- skylineplot
RedColobusTest <- test_constant_model
RedColobusPGSM <- list(pGSM_hat=pGSM_hat,pGSM_95HPD=pGSM_95HPD)


require(gplots)




pdf(file="RealDataSkylinePlot.pdf", width=8, height=8)
par(cex.axis=1,cex.lab=1.2,mar=c(3,3,2,2), oma=c(3.5,3.5,0,0),mfcol=c(2,2))

limits_on_y <- c(-2.5,3.5)
limits_on_x <- c(0,4)
label_x     <- "t (mutations/locus)"
label_y     <- expression("log"[10]*theta)
  
plot(WhaleSharkSky[,1],
     log10(WhaleSharkSky[,2]),
     type="l",
     xlab="",
     ylab="",
     ylim=limits_on_y,
     xlim=limits_on_x,
     lty=1,lwd=4)
lines(WhaleSharkSky[,1],
      log10(WhaleSharkSky[,3]),col="grey",type="l",lty=1,lwd=2)
lines(WhaleSharkSky[,1],
      log10(WhaleSharkSky[,4]),col="grey",type="l",lty=1,lwd=2)

theta0 <- 7.473
thetaA <- 2.471
D      <- 0.00902
tau    <- theta0*D
lines( c(0,tau, 10),
       log10(c(theta0,thetaA,thetaA) ),
       col="blue",type="l",lty=5,lwd=2)
  
legend(x="topright",legend="whale shark",cex=1.2,bty="n")
  
box()
  


plot(LeatherbackSky[,1],
     log10(LeatherbackSky[,2]),
     type="l",
     xlab="",
     ylab="",
     ylim=limits_on_y,
     xlim=limits_on_x,
     lty=1,lwd=4)
lines(LeatherbackSky[,1],
      log10(LeatherbackSky[,3]),col="grey",type="l",lty=1,lwd=2)
lines(LeatherbackSky[,1],
      log10(LeatherbackSky[,4]),col="grey",type="l",lty=1,lwd=2)

theta0 <- 4.903
thetaA <- 0.0248
D      <- 1.073
tau    <- theta0*D
lines( c(0,tau, 10),
       log10(c(theta0,thetaA,thetaA) ),
       col="blue",type="l",lty=5,lwd=2)


legend(x="topright",legend="leatherback turtle",cex=1.2,bty="n")

box()


plot(BlackNWhiteColobusSky[,1],
     log10(BlackNWhiteColobusSky[,2]),
     type="l",
     xlab="",
     ylab="",
     ylim=limits_on_y,
     xlim=limits_on_x,
     lty=1,lwd=4)
lines(BlackNWhiteColobusSky[,1],
      log10(BlackNWhiteColobusSky[,3]),col="grey",type="l",lty=1,lwd=2)
lines(BlackNWhiteColobusSky[,1],
      log10(BlackNWhiteColobusSky[,4]),col="grey",type="l",lty=1,lwd=2)

theta <- 1.399
abline(h=theta,col="blue",lty=5,lwd=2)

legend(x="topright",legend="black-and-white colobus",cex=1.2,bty="n")

box()


plot(RedColobusSky[,1],
     log10(RedColobusSky[,2]),
     type="l",
     xlab="",
     ylab="",
     ylim=limits_on_y,
     xlim=limits_on_x,
     lty=1,lwd=4)
lines(RedColobusSky[,1],
      log10(RedColobusSky[,3]),col="grey",type="l",lty=1,lwd=2)
lines(RedColobusSky[,1],
      log10(RedColobusSky[,4]),col="grey",type="l",lty=1,lwd=2)


theta0 <-  0.831
thetaA <- 6.099
D      <- 0.256
tau    <- theta0*D
lines( c(0,tau, 10),
       log10(c(theta0,thetaA,thetaA) ),
       col="blue",type="l",lty=5,lwd=2)

legend(x="topright",legend="red colobus",cex=1.2,bty="n")

box()


mtext(label_x, side=1, adj=0.5, cex=1.2, outer=TRUE)
mtext(label_y, side=2, adj=0.5, cex=1.2, outer=TRUE)

  
  
  

dev.off ( which=dev.cur() )

