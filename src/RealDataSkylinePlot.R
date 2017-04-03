cbbPalette <- c("#000000", "#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00", 
                "#CC79A7", "#F0E442")


# Load results file
load("/home/miguel/Work/Research/ABC_Skyline_plot/DIYABCskylineplot/results/Requin/Requin_step3.RData")
WhaleSharkSky <- skylineplot
WhaleSharkTest <- test_constant_model
WhaleSharkPGSM <- list(pGSM_hat=pGSM_hat,pGSM_95HPD=pGSM_95HPD)
WhaleSharkMutPrior <- mut_params
WhaleSharkMutPost  <- abcresult$adj.values

WhaleSharkVarEff   <- read.table(file=paste0("results/VarEff/Requin.skylineplot"))
#WhaleSharkBEAST    <- read.csv(file=paste0("results/BEAST/RequinR.csv"))

load("/home/miguel/Work/Research/ABC_Skyline_plot/DIYABCskylineplot/results/Leatherback/Leatherback_step3.RData")
LeatherbackSky <- skylineplot
LeatherbackTest <- test_constant_model
LeatherbackPGSM <- list(pGSM_hat=pGSM_hat,pGSM_95HPD=pGSM_95HPD)
LeatherbackMutPrior <- mut_params
LeatherbackMutPost  <- abcresult$adj.values

LeatherbackVarEff   <- read.table(file=paste0("results/VarEff/Leatherback.skylineplot"))
#LeatherbackBEAST    <- read.csv(file=paste0("results/BEAST/Leatherback.csv"))


load("/home/miguel/Work/Research/ABC_Skyline_plot/DIYABCskylineplot/results/BWC_SSMandGSM_results.RData")
BlackNWhiteColobusSky <- skylineplot
BlackNWhiteColobusTest <- BF
load("/home/miguel/Work/Research/ABC_Skyline_plot/DIYABCskylineplot/results/BWC_PGSM_results.RData")
BlackNWhiteColobusPGSM <- list(pGSM_hat=pGSM_hat,pGSM_95HPD=pGSM_95HPD)
BlackNWhiteColobusMutPrior <- prior
BlackNWhiteColobusMutPost  <- posterior

BlackNWhiteColobusVarEff   <- read.table(file=paste0("results/VarEff/BWC_CNP.skylineplot"))
BlackNWhiteColobusBEAST    <- read.csv(file=paste0("results/BEAST/BWC/BWC_CNP.csv"))


load("/home/miguel/Work/Research/ABC_Skyline_plot/DIYABCskylineplot/results/RC_SSMandGSM_results.RData")
RedColobusSky <- skylineplot
RedColobusTest <- BF
load("/home/miguel/Work/Research/ABC_Skyline_plot/DIYABCskylineplot/results/RC_PGSM_results.RData")
RedColobusPGSM <- list(pGSM_hat=pGSM_hat,pGSM_95HPD=pGSM_95HPD)
RedColobusMutPrior <- prior
RedColobusMutPost  <- posterior

RedColobusVarEff   <- read.table(file=paste0("results/VarEff/RC_CNP.skylineplot"))
RedColobusBEAST    <- read.csv(file=paste0("results/BEAST/RC/RC_CNP.csv"))



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







pdf(file="results/RealDataSkylinePlotSupp.pdf", width=8, height=8)
par(cex.axis=1,cex.lab=1.2,mar=c(3,3,2,2), oma=c(3.5,3.5,0,0),mfcol=c(2,2))

limits_on_y <- c(10^(-2.5),10^3.5)
limits_on_x <- c(0.00025,4)
label_x     <- expression(tau~"(time measured in mutations)")
label_y     <- expression(theta*"="*4*N[e]*mu)

plot(WhaleSharkSky[,1],
     WhaleSharkSky[,2],
     type="l",
     xlab="",
     ylab="",
     log="xy",
     ylim=limits_on_y,
     xlim=limits_on_x,
     cex.axis=0.00001,
     lty=1,lwd=4)
axis(1,
     at     = c(5e-4,5e-3,5e-2,5e-1,5) ,
     labels = c("0.0005","0.005","0.05","0.5","5") , las=1, tick=F)
axis(2,
     at     = c(0.01,1,100) ,
     labels = c("0.01","1","100") , las=1, tick=F)
lines(WhaleSharkSky[,1],
      WhaleSharkSky[,3],col=cbbPalette[1],type="l",lty=2,lwd=2)
lines(WhaleSharkSky[,1],
      WhaleSharkSky[,4],col=cbbPalette[1],type="l",lty=2,lwd=2)

lines(WhaleSharkVarEff[,1],WhaleSharkVarEff[,5],col=cbbPalette[5],lty=1,lwd=2)
lines(WhaleSharkVarEff[,1],WhaleSharkVarEff[,6],col=cbbPalette[5],lty=2)
lines(WhaleSharkVarEff[,1],WhaleSharkVarEff[,7],col=cbbPalette[5],lty=2)
#lines(WhaleSharkBEAST[-1,1],WhaleSharkBEAST[-1,3],col=cbbPalette[7],lty=1,lwd=2)



theta0      <- 7.473
thetaA      <- 2.471
D           <- 0.00902
tau         <- theta0*D
growth_rate <- log(thetaA/theta0)/tau
generations <- c(seq(0.00001,0.00025,0.0001),WhaleSharkSky[,1],5:40)
theta_t     <- array(NA,length(generations))
for (gen in 1:length(generations)){
  if (generations[gen]<=tau){
    theta_t[gen] <- theta0 * exp(growth_rate * generations[gen])
  }else{
    theta_t[gen] <- theta0 * exp(growth_rate * tau)
  }
}
lines( generations,
       theta_t,
       col=cbbPalette[2],type="l",lty=1,lwd=2)

legend(x="topright",
       legend="whale shark",
       cex=1.2,bty="n")

legend(x="bottomleft",
       legend=c("DIYABCskylineplot","Migraine","VarEff","BEAST"),
       col=c(cbbPalette[1],cbbPalette[2],cbbPalette[5],cbbPalette[7]),
       lwd=2,
       lty=1,
       cex=0.8,
       bty="n")

  
box()
  


plot(LeatherbackSky[,1],
     LeatherbackSky[,2],
     type="l",
     xlab="",
     ylab="",
     log="xy",
     ylim=limits_on_y,
     xlim=limits_on_x,
     cex.axis=0.00001,
     lty=1,lwd=4)
axis(1,
     at     = c(5e-4,5e-3,5e-2,5e-1,5) ,
     labels = c("0.0005","0.005","0.05","0.5","5") , las=1, tick=F)
axis(2,
     at     = c(0.01,1,100) ,
     labels = c("0.01","1","100") , las=1, tick=F)
lines(LeatherbackSky[,1],
      LeatherbackSky[,3],col=cbbPalette[1],type="l",lty=2,lwd=2)
lines(LeatherbackSky[,1],
      LeatherbackSky[,4],col=cbbPalette[1],type="l",lty=2,lwd=2)

lines(LeatherbackVarEff[,1],LeatherbackVarEff[,5],col=cbbPalette[5],lty=1,lwd=2)
lines(LeatherbackVarEff[,1],LeatherbackVarEff[,6],col=cbbPalette[5],lty=2)
lines(LeatherbackVarEff[,1],LeatherbackVarEff[,7],col=cbbPalette[5],lty=2)

#lines(LeatherbackBEAST[-1,1],LeatherbackBEAST[-1,3],col=cbbPalette[7],lty=1,lwd=2)


theta0 <- 4.903
thetaA <- 0.0248
D      <- 1.073
tau    <- theta0*D
growth_rate <- log(thetaA/theta0)/tau
generations <- c(seq(0.00001,0.00025,0.0001),LeatherbackSky[,1],5:40)
theta_t     <- array(NA,length(generations))
for (gen in 1:length(generations)){
  if (generations[gen]<=tau){
    theta_t[gen] <- theta0 * exp(growth_rate * generations[gen])
  }else{
    theta_t[gen] <- theta0 * exp(growth_rate * tau)
  }
}
lines( generations,
       theta_t,
       col=cbbPalette[2],type="l",lty=1,lwd=2)

legend(x="topright",
       legend="leatherback turtle",
       cex=1.2,bty="n")

box()


plot(BlackNWhiteColobusSky[,1],
     BlackNWhiteColobusSky[,2],
     type="l",
     xlab="",
     ylab="",
     log="xy",
     ylim=limits_on_y,
     xlim=limits_on_x,
     cex.axis=0.00001,
     lty=1,lwd=4)
axis(1,
     at     = c(5e-4,5e-3,5e-2,5e-1,5) ,
     labels = c("0.0005","0.005","0.05","0.5","5") , las=1, tick=F)
axis(2,
     at     = c(0.01,1,100) ,
     labels = c("0.01","1","100") , las=1, tick=F)
lines(BlackNWhiteColobusSky[,1],
      BlackNWhiteColobusSky[,3],col=cbbPalette[1],type="l",lty=2,lwd=2)
lines(BlackNWhiteColobusSky[,1],
      BlackNWhiteColobusSky[,4],col=cbbPalette[1],type="l",lty=2,lwd=2)

lines(BlackNWhiteColobusVarEff[,1],BlackNWhiteColobusVarEff[,5],col=cbbPalette[5],lty=1,lwd=2)
lines(BlackNWhiteColobusVarEff[,1],BlackNWhiteColobusVarEff[,6],col=cbbPalette[5],lty=2)
lines(BlackNWhiteColobusVarEff[,1],BlackNWhiteColobusVarEff[,7],col=cbbPalette[5],lty=2)

lines(BlackNWhiteColobusBEAST[-1,1],BlackNWhiteColobusBEAST[-1,3],col=cbbPalette[7],lty=1,lwd=2)
lines(BlackNWhiteColobusBEAST[-1,1],BlackNWhiteColobusBEAST[-1,4],col=cbbPalette[7],lty=2)
lines(BlackNWhiteColobusBEAST[-1,1],BlackNWhiteColobusBEAST[-1,5],col=cbbPalette[7],lty=2)

theta <- 1.399
abline(h=theta,col=cbbPalette[2],lty=1,lwd=2)

legend(x="topright",
       legend="black-and-white colobus",
       cex=1.2,bty="n")



box()


plot(RedColobusSky[,1],
     RedColobusSky[,2],
     type="l",
     xlab="",
     ylab="",
     log="xy",
     ylim=limits_on_y,
     xlim=limits_on_x,
     cex.axis=0.00001,
     lty=1,lwd=4)
axis(1,
     at     = c(5e-4,5e-3,5e-2,5e-1,5) ,
     labels = c("0.0005","0.005","0.05","0.5","5") , las=1, tick=F)
axis(2,
     at     = c(0.01,1,100) ,
     labels = c("0.01","1","100") , las=1, tick=F)
lines(RedColobusSky[,1],
      RedColobusSky[,3],col=cbbPalette[1],type="l",lty=2,lwd=2)
lines(RedColobusSky[,1],
      RedColobusSky[,4],col=cbbPalette[1],type="l",lty=2,lwd=2)

lines(RedColobusVarEff[,1],RedColobusVarEff[,5],col=cbbPalette[5],lty=1,lwd=2)
lines(RedColobusVarEff[,1],RedColobusVarEff[,6],col=cbbPalette[5],lty=2)
lines(RedColobusVarEff[,1],RedColobusVarEff[,7],col=cbbPalette[5],lty=2)

lines(RedColobusBEAST[-1,1],RedColobusBEAST[-1,3],col=cbbPalette[7],lty=1,lwd=2)
lines(RedColobusBEAST[-1,1],RedColobusBEAST[-1,4],col=cbbPalette[7],lty=2)
lines(RedColobusBEAST[-1,1],RedColobusBEAST[-1,5],col=cbbPalette[7],lty=2)


theta0 <-  0.831
thetaA <- 6.099
D      <- 0.256
tau    <- theta0*D
growth_rate <- log(thetaA/theta0)/tau
generations <- c(seq(0.00001,0.00025,0.0001),RedColobusSky[,1],5:40)
theta_t     <- array(NA,length(generations))
for (gen in 1:length(generations)){
  if (generations[gen]<=tau){
    theta_t[gen] <- theta0 * exp(growth_rate * generations[gen])
  }else{
    theta_t[gen] <- theta0 * exp(growth_rate * tau)
  }
}
lines( generations,
       theta_t,
       col=cbbPalette[2],type="l",lty=1,lwd=2)

legend(x="topright",
       legend="red colobus",
       cex=1.2,bty="n")


box()


mtext(label_x, side=1, adj=0.5, cex=1.2, outer=TRUE)
mtext(label_y, side=2, adj=0.5, cex=1.2, outer=TRUE)

  
  
  

dev.off ( which=dev.cur() )



















pdf(file="results/RealDataSkylinePlot.pdf", width=8, height=8)
par(cex.axis=1,cex.lab=1.2,mar=c(3,3,2,2), oma=c(3.5,3.5,0,0),mfcol=c(2,2))

limits_on_y <- c(10^(-2.5),10^3.5)
limits_on_x <- c(0.00025,4)
label_x     <- expression(tau~"(time measured in mutations)")
label_y     <- expression(theta*"="*4*N[e]*mu)

plot(WhaleSharkSky[,1],
     WhaleSharkSky[,2],
     type="l",
     xlab="",
     ylab="",
     log="xy",
     ylim=limits_on_y,
     xlim=limits_on_x,
     cex.axis=0.00001,
     lty=1,lwd=4)
axis(1,
     at     = c(5e-4,5e-3,5e-2,5e-1,5) ,
     labels = c("0.0005","0.005","0.05","0.5","5") , las=1, tick=F)
axis(2,
     at     = c(0.01,1,100) ,
     labels = c("0.01","1","100") , las=1, tick=F)
lines(WhaleSharkSky[,1],
      WhaleSharkSky[,3],col=cbbPalette[1],type="l",lty=2,lwd=2)
lines(WhaleSharkSky[,1],
      WhaleSharkSky[,4],col=cbbPalette[1],type="l",lty=2,lwd=2)

theta0      <- 7.473
thetaA      <- 2.471
D           <- 0.00902
tau         <- theta0*D
growth_rate <- log(thetaA/theta0)/tau
generations <- c(seq(0.00001,0.00025,0.0001),WhaleSharkSky[,1],5:40)
theta_t     <- array(NA,length(generations))
for (gen in 1:length(generations)){
  if (generations[gen]<=tau){
    theta_t[gen] <- theta0 * exp(growth_rate * generations[gen])
  }else{
    theta_t[gen] <- theta0 * exp(growth_rate * tau)
  }
}
lines( generations,
       theta_t,
       col=cbbPalette[2],type="l",lty=1,lwd=2)

BF <- round(WhaleSharkTest$posterior[2]/WhaleSharkTest$posterior[1],digits = 2)
legend(x="topright",
       legend=c("whale shark",
                paste0("BF=",BF)),
       cex=1.2,bty="n")

legend(x="bottomleft",
       legend=c("DIYABCskylineplot","Migraine"),
       col=c(cbbPalette[1],cbbPalette[2]),
       lwd=2,
       lty=1,
       cex=0.8,
       bty="n")


box()



plot(LeatherbackSky[,1],
     LeatherbackSky[,2],
     type="l",
     xlab="",
     ylab="",
     log="xy",
     ylim=limits_on_y,
     xlim=limits_on_x,
     cex.axis=0.00001,
     lty=1,lwd=4)
axis(1,
     at     = c(5e-4,5e-3,5e-2,5e-1,5) ,
     labels = c("0.0005","0.005","0.05","0.5","5") , las=1, tick=F)
axis(2,
     at     = c(0.01,1,100) ,
     labels = c("0.01","1","100") , las=1, tick=F)
lines(LeatherbackSky[,1],
      LeatherbackSky[,3],col=cbbPalette[1],type="l",lty=2,lwd=2)
lines(LeatherbackSky[,1],
      LeatherbackSky[,4],col=cbbPalette[1],type="l",lty=2,lwd=2)

theta0 <- 4.903
thetaA <- 0.0248
D      <- 1.073
tau    <- theta0*D
growth_rate <- log(thetaA/theta0)/tau
generations <- c(seq(0.00001,0.00025,0.0001),LeatherbackSky[,1],5:40)
theta_t     <- array(NA,length(generations))
for (gen in 1:length(generations)){
  if (generations[gen]<=tau){
    theta_t[gen] <- theta0 * exp(growth_rate * generations[gen])
  }else{
    theta_t[gen] <- theta0 * exp(growth_rate * tau)
  }
}
lines( generations,
       theta_t,
       col=cbbPalette[2],type="l",lty=1,lwd=2)

BF <- round(LeatherbackTest$posterior[2]/LeatherbackTest$posterior[1],digits = 2)
legend(x="topright",
       legend=c("leatherback turtle",
                paste0("BF=",BF)),
       cex=1.2,bty="n")

box()


plot(BlackNWhiteColobusSky[,1],
     BlackNWhiteColobusSky[,2],
     type="l",
     xlab="",
     ylab="",
     log="xy",
     ylim=limits_on_y,
     xlim=limits_on_x,
     cex.axis=0.00001,
     lty=1,lwd=4)
axis(1,
     at     = c(5e-4,5e-3,5e-2,5e-1,5) ,
     labels = c("0.0005","0.005","0.05","0.5","5") , las=1, tick=F)
axis(2,
     at     = c(0.01,1,100) ,
     labels = c("0.01","1","100") , las=1, tick=F)
lines(BlackNWhiteColobusSky[,1],
      BlackNWhiteColobusSky[,3],col=cbbPalette[1],type="l",lty=2,lwd=2)
lines(BlackNWhiteColobusSky[,1],
      BlackNWhiteColobusSky[,4],col=cbbPalette[1],type="l",lty=2,lwd=2)

theta <- 1.399
abline(h=theta,col=cbbPalette[2],lty=1,lwd=2)

BF <- round(BlackNWhiteColobusTest,digits = 2)
legend(x="topright",
       legend=c("black-and-white colobus",
                paste0("BF=",BF)),
       cex=1.2,bty="n")



box()


plot(RedColobusSky[,1],
     RedColobusSky[,2],
     type="l",
     xlab="",
     ylab="",
     log="xy",
     ylim=limits_on_y,
     xlim=limits_on_x,
     cex.axis=0.00001,
     lty=1,lwd=4)
axis(1,
     at     = c(5e-4,5e-3,5e-2,5e-1,5) ,
     labels = c("0.0005","0.005","0.05","0.5","5") , las=1, tick=F)
axis(2,
     at     = c(0.01,1,100) ,
     labels = c("0.01","1","100") , las=1, tick=F)
lines(RedColobusSky[,1],
      RedColobusSky[,3],col=cbbPalette[1],type="l",lty=2,lwd=2)
lines(RedColobusSky[,1],
      RedColobusSky[,4],col=cbbPalette[1],type="l",lty=2,lwd=2)

theta0 <-  0.831
thetaA <- 6.099
D      <- 0.256
tau    <- theta0*D
growth_rate <- log(thetaA/theta0)/tau
generations <- c(seq(0.00001,0.00025,0.0001),RedColobusSky[,1],5:40)
theta_t     <- array(NA,length(generations))
for (gen in 1:length(generations)){
  if (generations[gen]<=tau){
    theta_t[gen] <- theta0 * exp(growth_rate * generations[gen])
  }else{
    theta_t[gen] <- theta0 * exp(growth_rate * tau)
  }
}
lines( generations,
       theta_t,
       col=cbbPalette[2],type="l",lty=1,lwd=2)

BF <- round(RedColobusTest,digits = 2)
legend(x="topright",
       legend=c("red colobus",
                paste0("BF=",BF)),
       cex=1.2,bty="n")


box()


mtext(label_x, side=1, adj=0.5, cex=1.2, outer=TRUE)
mtext(label_y, side=2, adj=0.5, cex=1.2, outer=TRUE)





dev.off ( which=dev.cur() )

