# Script to summarize the results from the test of the ABC Skyline Plot
# approach on simulated data sets:  SKYLINE PLOT

# Miguel Navascués

number_of_replicates<-1000

model <- "P00"

options(scipen = 999)
scen_table <- read.table("Scenari/scenari.table.txt",header=T,row.names=1)
mkdir_command <- paste0("mkdir ",model,"/Results")
system( mkdir_command )

# MAIN TEXT FIGURE
pdf(file=paste0(model,"/Results/AllSkylines.pdf"),width=4,height=8)
par(mfrow=c(3,1), mar=c(3,3,1,1), oma=c(3,3,0,0))
for (scen in c(8,20,26) ){
  
  load(paste0(model,"/Scenario",scen,"/Scenario",scen,"_results.RData"))
  
  SKYtrue   <- true_demo
  SKYmedian <- t(skylineplot$median) 
  SKYlower  <- t(skylineplot$lower_95HPD)
  SKYupper  <- t(skylineplot$upper_95HPD)
  
  label_x     <- "t (mutations/locus)"
  label_y     <- expression("log"[10]*theta)
  
  plot( generations,
        log10(SKYmedian[,1]),
        type="n",
        xlab="",
        ylab="",
        xlim=c(0,4),
        ylim=c(-3,4), #ylim=c(-3,4),
        cex.axis=1.2)
  
  for (i in 1:number_of_replicates){
    lines(generations,log10(SKYlower[,i]),col=rgb(0.9,0.9,0.9,0.1))
    lines(generations,log10(SKYupper[,i]),col=rgb(0.6,0.6,0.6,0.1))
  }
  for (i in 1:number_of_replicates){
    lines(generations,log10(SKYmedian[,i]),col=rgb(0,0,0,0.1))
  }
  lines(generations,log10(SKYtrue),col="red")

  if (scen==26){
    mtext(label_x, side=1, adj=0.5, cex=1, outer=TRUE)
    mtext(label_y, side=2, adj=0.5, cex=1, outer=TRUE)
  }
  
  
  if(scen==8)  legend(x="topright",legend="contraction",cex=1.2,bty="n")
  if(scen==20) legend(x="topright",legend="expansion",cex=1.2,bty="n")
  if(scen==26) legend(x="topright",legend="constant size",cex=1.2,bty="n")
  
  
}
dev.off()





# conctractions
png(filename=paste0(model,"/Results/AllSkylinesContractions.png"),width=12,height=12,units="in", res=300)

par(mfrow=c(4,3), mar=c(3,3,1,1), oma=c(6,6,3,0))
for (scen in 1:12){
  
  load(paste0(model,"/Scenario",scen,"/Scenario",scen,"_results.RData"))
  
  SKYtrue   <- true_demo
  SKYmedian <- t(skylineplot$median) 
  SKYlower  <- t(skylineplot$lower_95HPD)
  SKYupper  <- t(skylineplot$upper_95HPD)
  
  label_x     <- "t (mutations/locus)"
  label_y     <- expression("log"[10]*theta)
  
  plot( generations,
        log10(SKYmedian[,1]),
        type="n",
        xlab="",
        ylab="",
        xlim=c(0,4),
        ylim=c(-3,4), #ylim=c(-3,4),
        cex.axis=1.2)
  
  for (i in 1:number_of_replicates){
    lines(generations,log10(SKYlower[,i]),col=rgb(0.9,0.9,0.9,0.1))
    lines(generations,log10(SKYupper[,i]),col=rgb(0.6,0.6,0.6,0.1))
  }
  for (i in 1:number_of_replicates){
    lines(generations,log10(SKYmedian[,i]),col=rgb(0,0,0,0.1))
  }
  lines(generations,log10(SKYtrue),col="red")

  if (scen==12){
    mtext(label_x, side=1, adj=0.5, cex=1.5, outer=TRUE)
    mtext(label_y, side=2, adj=0.5, cex=1.5, outer=TRUE)
    #mtext(main_title, side=3, adj=0.5, cex=1.2, outer=TRUE)
    
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
png(filename=paste0(model,"/Results/AllSkylinesExpansions.png"),width=12,height=12,units="in", res=300)

par(mfrow=c(4,3), mar=c(3,3,1,1), oma=c(6,6,3,0))
for (scen in 13:24){
  
  load(paste0(model,"/Scenario",scen,"/Scenario",scen,"_results.RData"))
  
  SKYtrue   <- true_demo
  SKYmedian <- t(skylineplot$median) 
  SKYlower  <- t(skylineplot$lower_95HPD)
  SKYupper  <- t(skylineplot$upper_95HPD)
  
  label_x     <- "t (mutations/locus)"
  label_y     <- expression("log"[10]*theta)
  
  plot( generations,
        log10(SKYmedian[,1]),
        type="n",
        xlab="",
        ylab="",
        xlim=c(0,4),
        ylim=c(-3,4), #ylim=c(-3,4),
        cex.axis=1.2)
  
  for (i in 1:number_of_replicates){
    lines(generations,log10(SKYlower[,i]),col=rgb(0.9,0.9,0.9,0.1))
    lines(generations,log10(SKYupper[,i]),col=rgb(0.6,0.6,0.6,0.1))
  }
  for (i in 1:number_of_replicates){
    lines(generations,log10(SKYmedian[,i]),col=rgb(0,0,0,0.1))
  }
  lines(generations,log10(SKYtrue),col="red")
  
  if (scen==24){
    mtext(label_x, side=1, adj=0.5, cex=1.5, outer=TRUE)
    mtext(label_y, side=2, adj=0.5, cex=1.5, outer=TRUE)
    #mtext(main_title, side=3, adj=0.5, cex=1.2, outer=TRUE)
    
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
png(filename=paste0(model,"/Results/AllSkylinesConstantSize.png"),width=12,height=4,units="in", res=300)

par(mfrow=c(1,3), mar=c(3,3,1,1), oma=c(6,3,3,0))
for (scen in 25:27){
  
  load(paste0(model,"/Scenario",scen,"/Scenario",scen,"_results.RData"))
  
  SKYtrue   <- true_demo
  SKYmedian <- t(skylineplot$median) 
  SKYlower  <- t(skylineplot$lower_95HPD)
  SKYupper  <- t(skylineplot$upper_95HPD)
  
  label_x     <- "t (mutations/locus)"
  label_y     <- expression("log"[10]*theta)
  
  plot( generations,
        log10(SKYmedian[,1]),
        type="n",
        xlab="",
        ylab="",
        xlim=c(0,4),
        ylim=c(-3,4), #ylim=c(-3,4),
        cex.axis=1.2)
  
  for (i in 1:number_of_replicates){
    lines(generations,log10(SKYlower[,i]),col=rgb(0.9,0.9,0.9,0.1))
    lines(generations,log10(SKYupper[,i]),col=rgb(0.6,0.6,0.6,0.1))
  }
  for (i in 1:number_of_replicates){
    lines(generations,log10(SKYmedian[,i]),col=rgb(0,0,0,0.1))
  }
  lines(generations,log10(SKYtrue),col="red")
  
  if (scen==27){
    mtext(label_x, side=1, adj=0.5, cex=1.5, outer=TRUE)
    mtext(label_y, side=2, adj=0.5, cex=1.5, outer=TRUE)
    #mtext(main_title, side=3, adj=0.5, cex=1.5, outer=TRUE)
    
    mtext(expression(theta*"=4"), side=1, line=3, cex=1.5, adj=0.15, outer=TRUE)
    mtext(expression(theta*"=40"), side=1, line=3, cex=1.5, adj=0.5, outer=TRUE)
    mtext(expression(theta*"=400"), side=1, line=3, cex=1.5, adj=0.85, outer=TRUE)
  }
  
  
}
dev.off()


