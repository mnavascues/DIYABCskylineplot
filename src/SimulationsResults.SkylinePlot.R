# Script to summarize the results from the test of the ABC Skyline Plot
# approach on simulated data sets:  SKYLINE PLOT

# Miguel Navascués

pgsm_values <- c(0.00,0.22,0.74)
options(scipen = 999)
project <- "Poisson"
number_of_replicates <- 100
scenarios_number <- 1:27
scenarios <- paste("Scenario", scenarios_number, sep="")

scen_table <- read.table("src/Scenari/scenari.table.txt",header=T,row.names=1)
mkdir_command <- paste0("mkdir results/",project,"/Results/SkylinePlot")
system( mkdir_command )

cbbPalette <- c("#000000", "#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00", 
                "#CC79A7", "#F0E442")


#MAIN FIGURE

pdf(file=paste0("results/",project,"/Results/SkylinePlot/SkylinePlot_main.pdf"),width=4,height=8)
par(mfrow=c(3,1), mar=c(3,3,1,1), oma=c(3,3,0,0))
for (scen in c(8,20,26) ){
  
  load(paste0("results/",project,"/Results/",scenarios[scen],"_0.22_results.RData"))

  SKYtrue   <- true_demo
  SKYmedian <- t(skylineplot$median) 
  SKYlower  <- t(skylineplot$lower_95HPD)
  SKYupper  <- t(skylineplot$upper_95HPD)
  
  label_x     <- expression(tau~"(mutations/locus)")
  label_y     <- expression(theta)
  
  plot( generations,
        SKYmedian[,1],
        type="n",
        xlab="",
        ylab="",
        xlim=c(2.5e-4,4),
        ylim=c(1e-3,1e4), #ylim=c(-3,4),
        cex.axis=1.2,
        log="xy")
  
  for (i in 1:number_of_replicates){
    lines(generations,SKYlower[,i],col=rgb(0.9,0.9,0.9,0.5))
    lines(generations,SKYupper[,i],col=rgb(0.6,0.6,0.6,0.5))
  }
  for (i in 1:number_of_replicates){
    lines(generations,SKYmedian[,i],col=rgb(0,0,0,0.5))
  }
  lines(generations,SKYtrue,col=cbbPalette[3])
  
  if (scen==26){
    mtext(label_x, side=1, adj=0.5, cex=1, outer=TRUE)
    mtext(label_y, side=2, adj=0.5, cex=1, outer=TRUE)
  }

  if(scen==8)  legend(x="topright",legend="contraction",cex=1.2,bty="n")
  if(scen==20) legend(x="topright",legend="expansion",cex=1.2,bty="n")
  if(scen==26) legend(x="topright",legend="constant size",cex=1.2,bty="n")

}
dev.off()







for (pgsm in seq_along(pgsm_values)){
  
  pdf(file=paste0("results/",project,"/Results/SkylinePlot/SkylinesContractions_",pgsm_values[pgsm],".pdf"),width=12,height=12)
  par(mfrow=c(4,3), mar=c(3,3,1,1), oma=c(6,6,3,0))
  for (scen in 1:12){
    
    load(paste0("results/",project,"/Results/",scenarios[scen],"_",pgsm_values[pgsm],"_results.RData"))

    SKYtrue   <- true_demo
    SKYmedian <- t(skylineplot$median) 
    SKYlower  <- t(skylineplot$lower_95HPD)
    SKYupper  <- t(skylineplot$upper_95HPD)
    
    label_x     <- expression(tau~"(mutations/locus)")
    label_y     <- expression(theta)
    
    plot( generations,
          SKYmedian[,1],
          type="n",
          xlab="",
          ylab="",
          xlim=c(2.5e-4,4),
          ylim=c(1e-3,1e4), #ylim=c(-3,4),
          cex.axis=1.2,
          log="xy")
    
    for (i in 1:number_of_replicates){
      lines(generations,SKYlower[,i],col=rgb(0.9,0.9,0.9,0.5))
      lines(generations,SKYupper[,i],col=rgb(0.6,0.6,0.6,0.5))
    }
    for (i in 1:number_of_replicates){
      lines(generations,SKYmedian[,i],col=rgb(0,0,0,0.5))
    }
    lines(generations,SKYtrue,col=cbbPalette[3])
    
    if (scen==12){
      mtext(label_x, side=1, adj=0.5, cex=1.5, outer=TRUE)
      mtext(label_y, side=2, adj=0.5, cex=1.5, outer=TRUE)
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


  pdf(file=paste0("results/",project,"/Results/SkylinePlot/SkylinesExpansions_",pgsm_values[pgsm],".pdf"),width=12,height=12)
  par(mfrow=c(4,3), mar=c(3,3,1,1), oma=c(6,6,3,0))
  for (scen in 13:24){
    
    load(paste0("results/",project,"/Results/",scenarios[scen],"_",pgsm_values[pgsm],"_results.RData"))
    
    SKYtrue   <- true_demo
    SKYmedian <- t(skylineplot$median) 
    SKYlower  <- t(skylineplot$lower_95HPD)
    SKYupper  <- t(skylineplot$upper_95HPD)
    
    label_x     <- expression(tau~"(mutations/locus)")
    label_y     <- expression(theta)
    
    plot( generations,
          SKYmedian[,1],
          type="n",
          xlab="",
          ylab="",
          xlim=c(2.5e-4,4),
          ylim=c(1e-3,1e4), #ylim=c(-3,4),
          cex.axis=1.2,
          log="xy")
    
    for (i in 1:number_of_replicates){
      lines(generations,SKYlower[,i],col=rgb(0.9,0.9,0.9,0.5))
      lines(generations,SKYupper[,i],col=rgb(0.6,0.6,0.6,0.5))
    }
    for (i in 1:number_of_replicates){
      lines(generations,SKYmedian[,i],col=rgb(0,0,0,0.5))
    }
    lines(generations,SKYtrue,col=cbbPalette[3])
    
    if (scen==24){
      mtext(label_x, side=1, adj=0.5, cex=1.5, outer=TRUE)
      mtext(label_y, side=2, adj=0.5, cex=1.5, outer=TRUE)
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
  
  
  
  
  
  pdf(file=paste0("results/",project,"/Results/SkylinePlot/SkylinePlotConstantSize_",pgsm_values[pgsm],".pdf"),width=12,height=4)
  par(mfrow=c(1,3), mar=c(3,3,1,1), oma=c(6,6,3,0))
  for (scen in 25:27){
    load(paste0("results/",project,"/Results/",scenarios[scen],"_",pgsm_values[pgsm],"_results.RData"))
    
    SKYtrue   <- true_demo
    SKYmedian <- t(skylineplot$median) 
    SKYlower  <- t(skylineplot$lower_95HPD)
    SKYupper  <- t(skylineplot$upper_95HPD)
    
    label_x     <- expression(tau~"(mutations/locus)")
    label_y     <- expression(theta)
    
    plot( generations,
          SKYmedian[,1],
          type="n",
          xlab="",
          ylab="",
          xlim=c(2.5e-4,4),
          ylim=c(1e-3,1e4), #ylim=c(-3,4),
          cex.axis=1.2,
          log="xy")
    
    for (i in 1:number_of_replicates){
      lines(generations,SKYlower[,i],col=rgb(0.9,0.9,0.9,0.5))
      lines(generations,SKYupper[,i],col=rgb(0.6,0.6,0.6,0.5))
    }
    for (i in 1:number_of_replicates){
      lines(generations,SKYmedian[,i],col=rgb(0,0,0,0.5))
    }
    lines(generations,SKYtrue,col=cbbPalette[3])
    
    
    if (scen==27){
      mtext(label_x, side=1, adj=0.5, cex=1.5, outer=TRUE)
      mtext(label_y, side=2, adj=0.5, cex=1.3, outer=TRUE)
      
      mtext(expression(theta*"=4"), side=1, line=3, cex=1.5, adj=0.15, outer=TRUE)
      mtext(expression(theta*"=40"), side=1, line=3, cex=1.5, adj=0.5, outer=TRUE)
      mtext(expression(theta*"=400"), side=1, line=3, cex=1.5, adj=0.85, outer=TRUE)
    }
  }
  dev.off()
  
    
}



























