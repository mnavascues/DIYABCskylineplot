# Script to summarize the results from the test of the ABC Skyline Plot
# approach on simulated data sets:  ERROR

# Miguel Navascués

pgsm_values <- 0.22 #c(0.00,0.22,0.74)
options(scipen = 999)
project <- "test"
number_of_replicates <- 30
scenarios_number <- c(8,20,26) #1:27
scenarios <- paste("Scenario", scenarios_number, sep="")


scen_table <- read.table("src/Scenari/scenari.table.txt",header=T,row.names=1)
mkdir_command <- paste0("mkdir results/",project,"/Results/Error")
system( mkdir_command )

for (pgsm in seq_along(pgsm_values)){
  pdf(file=paste0("results/",project,"/Results/Error/Error_",pgsm_values[pgsm],".pdf"),width=4,height=8)
  par(mfrow=c(3,1), mar=c(3,3,1,1), oma=c(3,3,0,0))
  for (scen in seq_along(scenarios_number) ){

    load(paste0("results/",project,"/Results/",scenarios[scen],"_",pgsm_values[pgsm],"_results.RData"))
    label_x     <- "t (mutations/locus)"
    label_y     <- c("relative mean absolute error"," relative bias")
    
    plot( generations,
          sky_error$relative_bias*(-1), #bias was calculated with the incorrect sign !!!!
          lwd=2,
          col="dodgerblue",
          type="l",
          xlab="",
          ylab="",
          xlim=c(0,4),
          ylim=c(-1,10))
    lines(generations,sky_error$relative_mean_absolute_error,lty=2,lwd=2,col="black")
    abline(h=0,lwd=0.5)
    
    
    if (scenarios_number[scen]==26){
      mtext(label_x, side=1, adj=0.5, cex=1, outer=TRUE)
      mtext(label_y, side=2,  adj=c(0.25,0.8), cex=1, outer=TRUE,col=c("black","dodgerblue"))
    }
    
    if(scenarios_number[scen]==8)  legend(x="topright",legend="contraction",cex=1.2,bty="n")
    if(scenarios_number[scen]==20) legend(x="topright",legend="expansion",cex=1.2,bty="n")
    if(scenarios_number[scen]==26) legend(x="topright",legend="constant size",cex=1.2,bty="n")
    
    
    
    
    
  }
  dev.off()
  
}

#######################################



# MAIN TEXT FIGURE
pdf(file=paste0(model,"/Results/Error.pdf"),width=4,height=8)
par(mfrow=c(3,1), mar=c(3,3,1,1), oma=c(3,3,0,0))
for (scen in c(8,20,26) ){

  load(paste0(model,"/Scenario",scen,"/Scenario",scen,"_results.RData"))
  
  label_x     <- "t (mutations/locus)"
  label_y     <- c("relative mean absolute error"," relative bias")
  
  plot( generations,
        sky_error$relative_bias*(-1), #bias was calculated with the incorrect sign !!!!
        lwd=2,
        col="dodgerblue",
        type="l",
        xlab="",
        ylab="",
        xlim=c(0,4),
        ylim=c(-1,10))
  lines(generations,sky_error$relative_mean_absolute_error,lty=2,lwd=2,col="black")
  abline(h=0,lwd=0.5)
  
  
  if (scen==26){
    mtext(label_x, side=1, adj=0.5, cex=1, outer=TRUE)
    mtext(label_y, side=2,  adj=c(0.25,0.8), cex=1, outer=TRUE,col=c("black","dodgerblue"))
  }

  if(scen==8)  legend(x="topright",legend="contraction",cex=1.2,bty="n")
  if(scen==20) legend(x="topright",legend="expansion",cex=1.2,bty="n")
  if(scen==26) legend(x="topright",legend="constant size",cex=1.2,bty="n")
  
  
  
}
dev.off()











# conctractions
pdf(file=paste0(model,"/Results/ErrorContractions.pdf"),width=12,height=12)

par(mfrow=c(4,3), mar=c(3,3,1,1), oma=c(6,6,3,0))
for (scen in 1:12){

  load(paste0(model,"/Scenario",scen,"/Scenario",scen,"_results.RData"))
  
  label_x     <- "t (mutations/locus)"
  label_y     <- c("relative mean absolute error"," relative bias")
  
  plot( generations,
        sky_error$relative_bias*(-1), #bias was calculated with the incorrect sign !!!!
        lwd=2,
        col="dodgerblue",
        type="l",
        xlab="",
        ylab="",
        xlim=c(0,4),
        ylim=c(-1,10))
  lines(generations,sky_error$relative_mean_absolute_error,lty=2,lwd=2,col="black")
  abline(h=0,lwd=0.5)
  
  
  if (scen==12){
    mtext(label_x, side=1, adj=0.5, cex=1.5, outer=TRUE)
    mtext(label_y, side=2,  adj=c(0.25,0.8), cex=1.5, outer=TRUE,col=c("black","dodgerblue"))

    mtext(expression(theta[1]*"=4"), side=1, line=3, cex=1.5, adj=0.15, outer=TRUE)
    mtext(expression(theta[1]*"=40"), side=1, line=3, cex=1.5, adj=0.5, outer=TRUE)
    mtext(expression(theta[1]*"=400"), side=1, line=3, cex=1.5, adj=0.85, outer=TRUE)
    mtext(expression(tau*"=0.01"), side=2, line=3, cex=1.5, adj=0.9, outer=TRUE)
    mtext(expression(tau*"=0.05"), side=2, line=3, cex=1.5, adj=0.65, outer=TRUE)
    mtext(expression(tau*"=0.1"), side=2, line=3, cex=1.5, adj=0.35, outer=TRUE)
    mtext(expression(tau*"=0.5"), side=2, line=3, cex=1.5, adj=0.1, outer=TRUE)
  }
  
  
}
dev.off()






# plotting skylines
# expansions
pdf(file=paste0(model,"/Results/ErrorExpansions.pdf"),width=12,height=12)

par(mfrow=c(4,3), mar=c(3,3,1,1), oma=c(6,6,3,0))
for (scen in 13:24){
  
  load(paste0(model,"/Scenario",scen,"/Scenario",scen,"_results.RData"))
  
  label_x     <- "t (mutations/locus)"
  label_y     <- c("relative mean absolute error"," relative bias")
  
  plot( generations,
        sky_error$relative_bias*(-1),
        lwd=2,
        col="dodgerblue",
        type="l",
        xlab="",
        ylab="",
        xlim=c(0,4),
        ylim=c(-1,10))
  lines(generations,sky_error$relative_mean_absolute_error,lty=2,lwd=2,col="black")
  abline(h=0,lwd=0.5)
  
  
  if (scen==24){
    mtext(label_x, side=1, adj=0.5, cex=1.5, outer=TRUE)
    mtext(label_y, side=2,  adj=c(0.25,0.8), cex=1.5, outer=TRUE,col=c("black","dodgerblue"))

    mtext(expression(theta[0]*"=4"), side=1, line=3, cex=1.5, adj=0.15, outer=TRUE)
    mtext(expression(theta[0]*"=40"), side=1, line=3, cex=1.5, adj=0.5, outer=TRUE)
    mtext(expression(theta[0]*"=400"), side=1, line=3, cex=1.5, adj=0.85, outer=TRUE)
    mtext(expression(tau*"=0.01"), side=2, line=3, cex=1.5, adj=0.9, outer=TRUE)
    mtext(expression(tau*"=0.05"), side=2, line=3, cex=1.5, adj=0.65, outer=TRUE)
    mtext(expression(tau*"=0.1"), side=2, line=3, cex=1.5, adj=0.35, outer=TRUE)
    mtext(expression(tau*"=0.5"), side=2, line=3, cex=1.5, adj=0.1, outer=TRUE)
  }
  
  
}
dev.off()





# plotting skylines
# constant size
pdf(file=paste0(model,"/Results/ErrorConstantSize.pdf"),width=12,height=4)

par(mfrow=c(1,3), mar=c(3,3,1,1), oma=c(6,6,3,0))
for (scen in 25:27){
  
  load(paste0(model,"/Scenario",scen,"/Scenario",scen,"_results.RData"))
  
  label_x     <- "t (mutations/locus)"
  label_y     <- c("relative mean absolute error"," relative bias")
  
  plot( generations,
        sky_error$relative_bias*(-1),
        lwd=2,
        col="dodgerblue",
        type="l",
        xlab="",
        ylab="",
        xlim=c(0,4),
        ylim=c(-1,10))
  lines(generations,sky_error$relative_mean_absolute_error,lty=2,lwd=2,col="black")
  abline(h=0,lwd=0.5)
  
  
  if (scen==27){
    mtext(label_x, side=1, adj=0.5, cex=1.5, outer=TRUE)
    mtext(label_y, side=2, adj=0.5, line=c(1,3), cex=1.3, outer=TRUE,col=c("black","dodgerblue"))
    
    mtext(expression(theta*"=4"), side=1, line=3, cex=1.5, adj=0.15, outer=TRUE)
    mtext(expression(theta*"=40"), side=1, line=3, cex=1.5, adj=0.5, outer=TRUE)
    mtext(expression(theta*"=400"), side=1, line=3, cex=1.5, adj=0.85, outer=TRUE)
  }
  
  
}
dev.off()


