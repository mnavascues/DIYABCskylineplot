cbbPalette <- c("#000000", "#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00", 
                "#CC79A7", "#F0E442")


# Load results file
load("/home/miguel/Work/Research/ABC_Skyline_plot/DIYABCskylineplot/results/Requin/Requin_step3.RData")
WhaleSharkSky <- skylineplot
WhaleSharkTest <- test_constant_model
WhaleSharkPGSM <- list(pGSM_hat=pGSM_hat,pGSM_95HPD=pGSM_95HPD)
WhaleSharkMutPrior <- mut_params
WhaleSharkMutPost  <- abcresult$adj.values


load("/home/miguel/Work/Research/ABC_Skyline_plot/DIYABCskylineplot/results/Leatherback/Leatherback_step3.RData")
LeatherbackSky <- skylineplot
LeatherbackTest <- test_constant_model
LeatherbackPGSM <- list(pGSM_hat=pGSM_hat,pGSM_95HPD=pGSM_95HPD)
LeatherbackMutPrior <- mut_params
LeatherbackMutPost  <- abcresult$adj.values

load("/home/miguel/Work/Research/ABC_Skyline_plot/DIYABCskylineplot/results/BWC_SSMandGSM_results.RData")
BlackNWhiteColobusSky <- skylineplot
BlackNWhiteColobusTest <- BF
load("/home/miguel/Work/Research/ABC_Skyline_plot/DIYABCskylineplot/results/BWC_PGSM_results.RData")
BlackNWhiteColobusPGSM <- list(pGSM_hat=pGSM_hat,pGSM_95HPD=pGSM_95HPD)
BlackNWhiteColobusMutPrior <- prior
BlackNWhiteColobusMutPost  <- posterior

load("/home/miguel/Work/Research/ABC_Skyline_plot/DIYABCskylineplot/results/RC_SSMandGSM_results.RData")
RedColobusSky <- skylineplot
RedColobusTest <- BF
load("/home/miguel/Work/Research/ABC_Skyline_plot/DIYABCskylineplot/results/RC_PGSM_results.RData")
RedColobusPGSM <- list(pGSM_hat=pGSM_hat,pGSM_95HPD=pGSM_95HPD)
RedColobusMutPrior <- prior
RedColobusMutPost  <- posterior


require(gplots)

pdf(file="results/RealDataPGSM.pdf", width=8, height=8)
par(cex.axis=1,cex.lab=1.2,mar=c(3,3,2,2), oma=c(3.5,3.5,0,0),mfcol=c(2,2))

label_x     <- expression(P[GSM])#c(expression(P[GSM]),expression(log[10](P[GSM])))
label_y     <- "probability density"


hist(WhaleSharkMutPrior,
     breaks=40,
     col="grey",
     freq=F,
     ylim=c(0,10),
     main="",xlab="",ylab="")
hist(WhaleSharkMutPost,
     breaks=20,
     col=rgb(1,0,0,0.5),
     freq=F,
     add=T)
legend(x="topright",
       legend=c("whale shark"),
       cex=1.2,bty="n")
legend(x="topleft",
       fill=c("grey",rgb(1,0,0,0.5)),
       legend=c("prior","posterior"),
       cex=1.2,bty="n")
box()


hist(LeatherbackMutPrior,
     breaks=40,
     col="grey",
     freq=F,
     ylim=c(0,10),
     main="",xlab="",ylab="")
hist(LeatherbackMutPost,
     breaks=20,
     col=rgb(1,0,0,0.5),
     freq=F,
     add=T)
legend(x="topright",
       legend=c("leatherback turtle"),
       cex=1.2,bty="n")
box()


hist(BlackNWhiteColobusMutPrior,
     breaks=40,
     col="grey",
     freq=F,
     ylim=c(0,10),
     main="",xlab="",ylab="")
hist(BlackNWhiteColobusMutPost,
     breaks=40,
     col=rgb(1,0,0,0.5),
     freq=F,
     add=T)
legend(x="topright",
       legend=c("black-and-white colobus"),
       cex=1.2,bty="n")
box()

hist(RedColobusMutPrior,
     breaks=40,
     col="grey",
     freq=F,
     ylim=c(0,10),
     main="",xlab="",ylab="")
hist(RedColobusMutPost,
     breaks=40,
     col=rgb(1,0,0,0.5),
     freq=F,
     add=T)
box()
legend(x="topright",
       legend=c("red colobus"),
       cex=1.2,bty="n")

mtext(label_x, side=1, adj=0.5, cex=1.2, outer=TRUE)
mtext(label_y, side=2, adj=0.5, cex=1.2, outer=TRUE)


dev.off ( which=dev.cur() )







pdf(file="results/RealDataSkylinePlot.pdf", width=8, height=8)
par(cex.axis=1,cex.lab=1.2,mar=c(3,3,2,2), oma=c(3.5,3.5,0,0),mfcol=c(2,2))

limits_on_y <- c(-2.5,3.5)
limits_on_x <- c(0,4)
label_x     <- expression(tau~"(mutations/locus)")
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
       col=cbbPalette[2],type="l",lty=5,lwd=2)

BF <- round(WhaleSharkTest$posterior[2]/WhaleSharkTest$posterior[1],digits = 2)
legend(x="topright",
       legend=c("whale shark",
                paste0("BF=",BF)),
       cex=1.2,bty="n")
  
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
       col=cbbPalette[2],type="l",lty=5,lwd=2)

BF <- round(LeatherbackTest$posterior[2]/LeatherbackTest$posterior[1],digits = 2)
legend(x="topright",
       legend=c("leatherback turtle",
                paste0("BF=",BF)),
       cex=1.2,bty="n")

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
abline(h=log10(theta),col=cbbPalette[2],lty=5,lwd=2)

BF <- round(BlackNWhiteColobusTest,digits = 2)
legend(x="topright",
       legend=c("black-and-white colobus",
                paste0("BF=",BF)),
       cex=1.2,bty="n")



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
       col=cbbPalette[2],type="l",lty=5,lwd=2)

BF <- round(RedColobusTest,digits = 2)
legend(x="topright",
       legend=c("red colobus",
                paste0("BF=",BF)),
       cex=1.2,bty="n")


box()


mtext(label_x, side=1, adj=0.5, cex=1.2, outer=TRUE)
mtext(label_y, side=2, adj=0.5, cex=1.2, outer=TRUE)

  
  
  

dev.off ( which=dev.cur() )

