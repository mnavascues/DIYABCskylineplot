# Script to summarize the results from the test of the ABC Skyline Plot
# approach on simulated data sets:  COMPARISON WITH VarEff & BEAST

# Miguel Navascués

cbbPalette <- c("#000000", "#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00", 
                "#CC79A7", "#F0E442")

scenarios <- c(8,20,26)

pdf(file=paste0("results/MethodComparisonOnSims.pdf"),width=4,height=8)
par(mfrow=c(3,1), mar=c(3,3,1,1), oma=c(3,3,0,0))
for (scen in 1:3 ){
  
  load(paste0("results/Scenario",scenarios[scen],"/Scenario",scenarios[scen],"_step3.RData"))
  source(paste0("src/Scenari/Scenario",scenarios[scen],".R"))
  true_demo <- demography(skylineplot[,"generations"]/1e-3)*1e-3*2

  generations    <- skylineplot[,"generations"]
  SKYtrue        <- true_demo
  SKY_ABC_median <- skylineplot[,"median"]  
  SKY_ABC_lower  <- skylineplot[,"lower_95HPD"] 
  SKY_ABC_upper  <- skylineplot[,"upper_95HPD"] 

  SKY_VarEff     <- read.table(file=paste0("results/VarEff/Scenario",scenarios[scen],".skylineplot"))

  SKY_BEAST      <- read.csv(file=paste0("results/BEAST/Scenario",scenarios[scen],"/Scenario",scenarios[scen],".csv"))

  label_x     <- expression(tau~"(time measured in mutations)")
  label_y     <- expression(theta*"="*4*N[e]*mu)
  
  plot( generations,
        SKYtrue,
        type="l",
        col=cbbPalette[3],
        xlab="",
        ylab="",
        xlim=c(2.5e-4,4),
        ylim=c(1e-3,1e4), #ylim=c(-3,4),
        cex.axis=0.00001,
        log="xy",lwd=3)
  axis(1,
       at     = c(5e-4,5e-3,5e-2,5e-1,5) ,
       labels = c("0.0005","0.005","0.05","0.5","5") , las=1, tick=F)
  axis(2,
       at     = c(0.001,0.1,10,1000) ,
       labels = c("0.001","0.1","10","1000") , las=1, tick=F)
  
  
  lines(generations,SKY_ABC_lower,col=cbbPalette[1],lty=2)
  lines(generations,SKY_ABC_upper,col=cbbPalette[1],lty=2)
  lines(generations,SKY_ABC_median,col=cbbPalette[1],lwd=2)

  lines(SKY_VarEff[,1],SKY_VarEff[,6],col=cbbPalette[5],lty=2)
  lines(SKY_VarEff[,1],SKY_VarEff[,7],col=cbbPalette[5],lty=2)
  lines(SKY_VarEff[,1],SKY_VarEff[,5],col=cbbPalette[5],lwd=2)
  
  lines(SKY_BEAST[-1,1],SKY_BEAST[-1,5],col=cbbPalette[7],lty=2)
  lines(SKY_BEAST[-1,1],SKY_BEAST[-1,4],col=cbbPalette[7],lty=2)
  lines(SKY_BEAST[-1,1],SKY_BEAST[-1,3],col=cbbPalette[7],lwd=2)
  
  if (scen==1){
    theta0      <- 0.13
    thetaA      <- 11.28
    D           <- 0.2
  }
  if (scen==2){
    theta0      <- 36.11
    thetaA      <- 0.34
    D           <- 0.1
  }
  if (scen==3){
    theta0      <- 47.55
    thetaA      <- 47.55
    D           <- 0.1
  }
  tau         <- theta0*D
  growth_rate <- log(thetaA/theta0)/tau
  generations2 <- c(seq(0.00001,0.00025,0.0001),generations,5:40)
  theta_t     <- array(NA,length(generations))
  for (gen in 1:length(generations2)){
    if (generations2[gen]<=tau){
      theta_t[gen] <- theta0 * exp(growth_rate * generations2[gen])
    }else{
      theta_t[gen] <- theta0 * exp(growth_rate * tau)
    }
  }
  lines( generations2,
         theta_t,
         col=cbbPalette[2],type="l",lwd=2)
  
  rm()
  
  
  
  if (scen==3){
    mtext(label_x, side=1, adj=0.5, cex=1, outer=TRUE)
    mtext(label_y, side=2, adj=0.5, cex=1, outer=TRUE)
  }

  
  
  
  if(scen==1)  legend(x="topright",legend="contraction",cex=1.2,bty="n")
  if(scen==2) legend(x="topright",legend="expansion",cex=1.2,bty="n")
  if(scen==3) {
    legend(x="topright",legend="constant size",cex=1.2,bty="n")
    
    legend(x="bottomright",
           legend=c("Simulation","DIYABCskylineplot","Migraine","VarEff","BEAST"),
           col=c(cbbPalette[3],cbbPalette[1],cbbPalette[2],cbbPalette[5],cbbPalette[7]),
           lwd=1.5,
           lty=1,
           cex=1,
           bty="n")
    
  }

  
  
}
dev.off()

