# Script to summarize the results from the test of the ABC Skyline Plot
# approach on simulated data sets

# Miguel Navascués

options(scipen = 999)

model <- "SMM"

scen_table <- read.table("Scenari/scenari.table.txt",header=T,row.names=1)
mkdir_command <- paste0("mkdir ",model,"/Results")
system( mkdir_command )


#####################
#####################

# ABS VS. BEAST

#####################
#####################

# comparison with BEAST
# conctractions
pdf(file=paste0(model,"BEASTvsABC_Contractions.pdf"),width=12,height=9)

par(mfrow=c(4,3), mar=c(3,3,1,1), oma=c(6,6,3,0))
for (scen in 1:12 ){
  
  SKYtrue   <- read.table(paste0(model,"/Scenario",scen,"/Scenario",scen,"_ABCsky_true.txt"),header=T)
  SKYmedian <- t(read.table(paste0(model,"/Scenario",scen,"/Scenario",scen,"_ABCsky_median.txt"),header=T))
  SKYlower  <- t(read.table(paste0(model,"/Scenario",scen,"/Scenario",scen,"_ABCsky_lower.txt"),header=T))
  SKYupper  <- t(read.table(paste0(model,"/Scenario",scen,"/Scenario",scen,"_ABCsky_upper.txt"),header=T))

  
  main_title <- expression("Contractions, with "*N[0]*"="*100)
  label_x     <- "t (mutations/locus)"
  label_y     <- expression("log"[10]*theta)
  
  # True demography
  plot( SKYtrue$time,
        log10(SKYtrue$pop_size),
        col="red",
        type="l",
        xlab="",
        ylab="",
        xlim=c(0,4),
        ylim=c(-3,4))
  
  # ABC skyline plot
  lines(SKYtrue$time,log10(SKYmedian[,16]),col="black")
  lines(SKYtrue$time,log10(SKYlower[,16]),col="black",lty=2)
  lines(SKYtrue$time,log10(SKYupper[,16]),col="black",lty=2)

  if (length(which(c(1,2,3,4,10,12)==scen))==1){
    SKYbeast  <- read.csv(paste0("BEAST/Scen",scen,"_sim16.log.csv"))
    # BEAST skyline plot
    lines(SKYbeast$time,log10(SKYbeast$median),col="blue")
    lines(SKYbeast$time,log10(SKYbeast$hpd.lower.95),col="blue",lty=2)
    lines(SKYbeast$time,log10(SKYbeast$hpd.upper.95),col="blue",lty=2)
  }
  
  
  if (scen==12){
    mtext(label_x, side=1, adj=0.5, cex=1, outer=TRUE)
    mtext(label_y, side=2, adj=0.5, cex=1, outer=TRUE)
    mtext(main_title, side=3, adj=0.5, cex=1.2, outer=TRUE)
    
    mtext(expression(N[1]*"="*1000), side=1, line=3, cex=1.2, adj=0.15, outer=TRUE)
    mtext(expression(N[1]*"="*10000), side=1, line=3, cex=1.2, adj=0.5, outer=TRUE)
    mtext(expression(N[1]*"="*100000), side=1, line=3, cex=1.2, adj=0.85, outer=TRUE)
    mtext(expression(T[a]*"="*10), side=2, line=3, cex=1.2, adj=0.9, outer=TRUE)
    mtext(expression(T[a]*"="*50), side=2, line=3, cex=1.2, adj=0.65, outer=TRUE)
    mtext(expression(T[a]*"="*100), side=2, line=3, cex=1.2, adj=0.35, outer=TRUE)
    mtext(expression(T[a]*"="*500), side=2, line=3, cex=1.2, adj=0.1, outer=TRUE)
  }
  
  
}
dev.off()





# comparison with BEAST
# expansions
pdf(file=paste0(model,"BEASTvsABC_Expansions.pdf"),width=12,height=9)

par(mfrow=c(4,3), mar=c(3,3,1,1), oma=c(6,6,3,0))
for (scen in 13:24){
  
  SKYtrue   <- read.table(paste0(model,"/Scenario",scen,"/Scenario",scen,"_ABCsky_true.txt"),header=T)
  SKYmedian <- t(read.table(paste0(model,"/Scenario",scen,"/Scenario",scen,"_ABCsky_median.txt"),header=T))
  SKYlower  <- t(read.table(paste0(model,"/Scenario",scen,"/Scenario",scen,"_ABCsky_lower.txt"),header=T))
  SKYupper  <- t(read.table(paste0(model,"/Scenario",scen,"/Scenario",scen,"_ABCsky_upper.txt"),header=T))
  
  main_title <- expression("Expansions, with "*N[1]*"="*100)
  label_x     <- "t (mutations/locus)"
  label_y     <- expression("log"[10]*theta)
  
  # True demography
  plot( SKYtrue$time,
        log10(SKYtrue$pop_size),
        col="red",
        type="l",
        xlab="",
        ylab="",
        xlim=c(0,4),
        ylim=c(-3,4))
  
  # ABC skyline plot
  lines(SKYtrue$time,log10(SKYmedian[,16]),col="black")
  lines(SKYtrue$time,log10(SKYlower[,16]),col="black",lty=2)
  lines(SKYtrue$time,log10(SKYupper[,16]),col="black",lty=2)
  
  if (length(which(c(13,15,22,24)==scen))==1){
    SKYbeast  <- read.csv(paste0("BEAST/Scen",scen,"_sim16.log.csv"))
    # BEAST skyline plot
    lines(SKYbeast$time,log10(SKYbeast$median),col="blue")
    lines(SKYbeast$time,log10(SKYbeast$hpd.lower.95),col="blue",lty=2)
    lines(SKYbeast$time,log10(SKYbeast$hpd.upper.95),col="blue",lty=2)
  }
  
  if (scen==24){
    mtext(label_x, side=1, adj=0.5, cex=1, outer=TRUE)
    mtext(label_y, side=2, adj=0.5, cex=1, outer=TRUE)
    mtext(main_title, side=3, adj=0.5, cex=1.2, outer=TRUE)
    
    mtext(expression(N[0]*"="*1000), side=1, line=3, cex=1.2, adj=0.15, outer=TRUE)
    mtext(expression(N[0]*"="*10000), side=1, line=3, cex=1.2, adj=0.5, outer=TRUE)
    mtext(expression(N[0]*"="*100000), side=1, line=3, cex=1.2, adj=0.85, outer=TRUE)
    mtext(expression(T[a]*"="*10), side=2, line=3, cex=1.2, adj=0.9, outer=TRUE)
    mtext(expression(T[a]*"="*50), side=2, line=3, cex=1.2, adj=0.65, outer=TRUE)
    mtext(expression(T[a]*"="*100), side=2, line=3, cex=1.2, adj=0.35, outer=TRUE)
    mtext(expression(T[a]*"="*500), side=2, line=3, cex=1.2, adj=0.1, outer=TRUE)
  }
  
  
}
dev.off()



# comparison with BEAST
# constant size
pdf(file=paste0(model,"BEASTvsABC_ConstantSize.pdf"),width=12,height=4)

par(mfrow=c(1,3), mar=c(3,3,1,1), oma=c(6,3,3,0))
for (scen in 25:27){
  
  SKYtrue   <- read.table(paste0(model,"/Scenario",scen,"/Scenario",scen,"_ABCsky_true.txt"),header=T)
  SKYmedian <- t(read.table(paste0(model,"/Scenario",scen,"/Scenario",scen,"_ABCsky_median.txt"),header=T))
  SKYlower  <- t(read.table(paste0(model,"/Scenario",scen,"/Scenario",scen,"_ABCsky_lower.txt"),header=T))
  SKYupper  <- t(read.table(paste0(model,"/Scenario",scen,"/Scenario",scen,"_ABCsky_upper.txt"),header=T))
  
  main_title <- "Constant size"
  label_x     <- "t (mutations/locus)"
  label_y     <- expression("log"[10]*theta)
  
  # True demography
  plot( SKYtrue$time,
        log10(SKYtrue$pop_size),
        col="red",
        type="l",
        xlab="",
        ylab="",
        xlim=c(0,4),
        ylim=c(-3,4))
  
  # ABC skyline plot
  lines(SKYtrue$time,log10(SKYmedian[,16]),col="black")
  lines(SKYtrue$time,log10(SKYlower[,16]),col="black",lty=2)
  lines(SKYtrue$time,log10(SKYupper[,16]),col="black",lty=2)
  
  if (length(which(c(25,27)==scen))==1){
    SKYbeast  <- read.csv(paste0("BEAST/Scen",scen,"_sim16.log.csv"))
    # BEAST skyline plot
    lines(SKYbeast$time,log10(SKYbeast$median),col="blue")
    lines(SKYbeast$time,log10(SKYbeast$hpd.lower.95),col="blue",lty=2)
    lines(SKYbeast$time,log10(SKYbeast$hpd.upper.95),col="blue",lty=2)
  }
  
  if (scen==27){
    mtext(label_x, side=1, adj=0.5, cex=1, outer=TRUE)
    mtext(label_y, side=2, adj=0.5, cex=1, outer=TRUE)
    mtext(main_title, side=3, adj=0.5, cex=1.2, outer=TRUE)
    
    mtext("N=1000", side=1, line=3, cex=1.2, adj=0.15, outer=TRUE)
    mtext("N=10000", side=1, line=3, cex=1.2, adj=0.5, outer=TRUE)
    mtext("N=100000", side=1, line=3, cex=1.2, adj=0.85, outer=TRUE)
  }
  
  
}
dev.off()





















