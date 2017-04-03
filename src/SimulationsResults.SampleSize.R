# Script to summarize the results from the test of the ABC Skyline Plot
# approach on simulated data sets:  SAMPLE SIZE

# Miguel Navascués

pgsm_values <- 0.22
options(scipen = 999)
project <- "Poisson"
number_of_replicates <- 100
scenarios_number <- c(8,20,26)
scenarios <- paste("Scenario", scenarios_number, sep="")

load(paste0("results/",project,"/Results/BayesFactor/BayesFactor_0.22.RData"))

scen8_loci            <- matrix(NA,nrow=100,ncol=4)
scen20_loci           <- matrix(NA,nrow=100,ncol=4)
scen26_loci           <- matrix(NA,nrow=100,ncol=4)
colnames(scen8_loci)  <- c("loci_7","loci_15","loci_30","loci_60")
colnames(scen20_loci) <- c("loci_7","loci_15","loci_30","loci_60")
colnames(scen26_loci) <- c("loci_7","loci_15","loci_30","loci_60")

scen8_ind            <- matrix(NA,nrow=100,ncol=5)
scen20_ind           <- matrix(NA,nrow=100,ncol=5)
scen26_ind           <- matrix(NA,nrow=100,ncol=5)
colnames(scen8_ind)  <- c("ind_6","ind_12","ind_25","ind_50","ind_100")
colnames(scen20_ind) <- c("ind_6","ind_12","ind_25","ind_50","ind_100")
colnames(scen26_ind) <- c("ind_6","ind_12","ind_25","ind_50","ind_100")

scen8_loci[ ,"loci_30"] <- scen8_ind[ ,"ind_50"] <- bayes_factor[,8]
scen20_loci[,"loci_30"] <- scen20_ind[,"ind_50"] <- bayes_factor[,20]
scen26_loci[,"loci_30"] <- scen26_ind[,"ind_50"] <- bayes_factor[,26]

for (project in c("ind_6","ind_12","ind_25","ind_100")){
  bayes_factor <- matrix(NA,nrow=number_of_replicates,ncol=length(scenarios_number))
  for (scen in seq_along(scenarios_number)){
    load(paste0("results/",project,"/Results/",scenarios[scen],"_",pgsm_values[pgsm],"_results.RData"))
    bayes_factor[,scen] <- (test_constant_model$posterior[,2]/test_constant_model$posterior[,1])[1:number_of_replicates]
    bayes_factor[which(is.infinite(bayes_factor[,scen])),scen] <- NA
  }
  scen8_ind[,project] <- bayes_factor[,1]
  scen20_ind[,project] <- bayes_factor[,2]
  scen26_ind[,project] <- bayes_factor[,3]
}

for (project in c("loci_7","loci_15","loci_60")){
  bayes_factor <- matrix(NA,nrow=number_of_replicates,ncol=length(scenarios_number))
  for (scen in seq_along(scenarios_number)){
    load(paste0("results/",project,"/Results/",scenarios[scen],"_",pgsm_values[pgsm],"_results.RData"))
    bayes_factor[,scen] <- (test_constant_model$posterior[,2]/test_constant_model$posterior[,1])[1:number_of_replicates]
    bayes_factor[which(is.infinite(bayes_factor[,scen])),scen] <- NA
  }
  scen8_loci[,project] <- bayes_factor[,1]
  scen20_loci[,project] <- bayes_factor[,2]
  scen26_loci[,project] <- bayes_factor[,3]
}

{
pdf(file=paste0("results/BayesFactor_sample_size.pdf"),width=8,height=6)
par(mfrow=c(2,3), mar=c(4,4,3,1))

plot.new()
plot.window(xlim=c(0,6), xaxs="i",
            ylim=c(0.1,1000), yaxs="i", log="y")
xx <- c(0,0,6,6)
yy <- c(0.3,1,1,0.3)
polygon(xx, yy, col="gray100",border = NA)
yy <- c(1,3,3,1)
polygon(xx, yy, col="gray95",border = NA)
yy <- c(3,10,10,3)
polygon(xx, yy, col="gray90",border = NA)
yy <- c(10,30,30,10)
polygon(xx, yy, col="gray85",border = NA)
yy <- c(30,100,100,30)
polygon(xx, yy, col="gray80",border = NA)
yy <- c(100,10000000,10000000,100)
polygon(xx, yy, col="gray75",border = NA)

boxplot(scen8_ind, add=T, names=NA, main= "contraction", ylab="Bayes factor", col="white",pch=".")
axis(1, at=1:5 , labels= c(6,12,25,50,100), las=1, tick=F)

plot.new()
plot.window(xlim=c(0,6), xaxs="i",
            ylim=c(0.1,1000), yaxs="i", log="y")
xx <- c(0,0,6,6)
yy <- c(0.3,1,1,0.3)
polygon(xx, yy, col="gray100",border = NA)
yy <- c(1,3,3,1)
polygon(xx, yy, col="gray95",border = NA)
yy <- c(3,10,10,3)
polygon(xx, yy, col="gray90",border = NA)
yy <- c(10,30,30,10)
polygon(xx, yy, col="gray85",border = NA)
yy <- c(30,100,100,30)
polygon(xx, yy, col="gray80",border = NA)
yy <- c(100,10000000,10000000,100)
polygon(xx, yy, col="gray75",border = NA)
boxplot(scen20_ind, add=T, names=NA, main= "expansion", xlab="sample size (diploid individuals)", col="white",pch=".")
axis(1, at=1:5 , labels= c(6,12,25,50,100), las=1, tick=F)


plot.new()
plot.window(xlim=c(0,7), xaxs="i",
            ylim=c(0.1,1000), yaxs="i", log="y")
xx <- c(0,0,7,7)
yy <- c(0.3,1,1,0.3)
polygon(xx, yy, col="gray100",border = NA)
yy <- c(1,3,3,1)
polygon(xx, yy, col="gray95",border = NA)
yy <- c(3,10,10,3)
polygon(xx, yy, col="gray90",border = NA)
yy <- c(10,30,30,10)
polygon(xx, yy, col="gray85",border = NA)
yy <- c(30,100,100,30)
polygon(xx, yy, col="gray80",border = NA)
yy <- c(100,10000000,10000000,100)
polygon(xx, yy, col="gray75",border = NA)
text(6,0.5477226,"negative",cex=0.8)
text(6,1.732051,"weak",cex=0.8)
text(6,5.477226,"substantial",cex=0.8)
text(6,17.32051,"strong",cex=0.8)
text(6,54.77226,"very strong",cex=0.8)
text(6,173.2051,"decisive",cex=0.8)
boxplot(scen26_ind, add=T, names=NA, main="constant", col="white",pch=".")
axis(1, at=1:5 , labels= c(6,12,25,50,100), las=1, tick=F)








plot.new()
plot.window(xlim=c(0,5), xaxs="i",
            ylim=c(0.1,1000), yaxs="i", log="y")
xx <- c(0,0,5,5)
yy <- c(0.3,1,1,0.3)
polygon(xx, yy, col="gray100",border = NA)
yy <- c(1,3,3,1)
polygon(xx, yy, col="gray95",border = NA)
yy <- c(3,10,10,3)
polygon(xx, yy, col="gray90",border = NA)
yy <- c(10,30,30,10)
polygon(xx, yy, col="gray85",border = NA)
yy <- c(30,100,100,30)
polygon(xx, yy, col="gray80",border = NA)
yy <- c(100,10000000,10000000,100)
polygon(xx, yy, col="gray75",border = NA)

boxplot(scen8_loci, add=T, names=NA, ylab="Bayes factor", col="white",pch=".")
axis(1, at=1:4 , labels= c(7,15,30,60), las=1, tick=F)

plot.new()
plot.window(xlim=c(0,5), xaxs="i",
            ylim=c(0.1,1000), yaxs="i", log="y")
xx <- c(0,0,5,5)
yy <- c(0.3,1,1,0.3)
polygon(xx, yy, col="gray100",border = NA)
yy <- c(1,3,3,1)
polygon(xx, yy, col="gray95",border = NA)
yy <- c(3,10,10,3)
polygon(xx, yy, col="gray90",border = NA)
yy <- c(10,30,30,10)
polygon(xx, yy, col="gray85",border = NA)
yy <- c(30,100,100,30)
polygon(xx, yy, col="gray80",border = NA)
yy <- c(100,10000000,10000000,100)
polygon(xx, yy, col="gray75",border = NA)
boxplot(scen20_loci, add=T, names=NA, xlab="number of loci", col="white",pch=".")
axis(1, at=1:4 , labels= c(7,15,30,60), las=1, tick=F)


plot.new()
plot.window(xlim=c(0,6), xaxs="i",
            ylim=c(0.1,1000), yaxs="i", log="y")
xx <- c(0,0,6,6)
yy <- c(0.3,1,1,0.3)
polygon(xx, yy, col="gray100",border = NA)
yy <- c(1,3,3,1)
polygon(xx, yy, col="gray95",border = NA)
yy <- c(3,10,10,3)
polygon(xx, yy, col="gray90",border = NA)
yy <- c(10,30,30,10)
polygon(xx, yy, col="gray85",border = NA)
yy <- c(30,100,100,30)
polygon(xx, yy, col="gray80",border = NA)
yy <- c(100,10000000,10000000,100)
polygon(xx, yy, col="gray75",border = NA)
text(5,0.5477226,"negative",cex=0.8)
text(5,1.732051,"weak",cex=0.8)
text(5,5.477226,"substantial",cex=0.8)
text(5,17.32051,"strong",cex=0.8)
text(5,54.77226,"very strong",cex=0.8)
text(5,173.2051,"decisive",cex=0.8)
boxplot(scen26_loci, add=T, names=NA, col="white",pch=".")
axis(1, at=1:4 , labels= c(7,15,30,60), las=1, tick=F)





dev.off()
}



bayes_factor <- matrix(NA,nrow=number_of_replicates,ncol=length(scenarios_number))
for (scen in seq_along(scenarios_number)){
  load(paste0("results/loci_14_ind_22/Results/",scenarios[scen],"_0.43_results.RData"))
  bayes_factor[,scen] <- (test_constant_model$posterior[,2]/test_constant_model$posterior[,1])[1:number_of_replicates]
  bayes_factor[which(is.infinite(bayes_factor[,scen])),scen] <- NA
}

pdf(file=paste0("results/loci_14_ind_22/Results/BayesFactor_colobus.pdf"),width=5,height=4)
plot.new()
par(mar=c(3,4,2,1))
plot.window(xlim=c(0,5), xaxs="i",
            ylim=c(0.1,1000), yaxs="i", log="y")
xx <- c(0,0,5,5)
yy <- c(0.3,1,1,0.3)
polygon(xx, yy, col="gray100",border = NA)
yy <- c(1,3,3,1)
polygon(xx, yy, col="gray95",border = NA)
yy <- c(3,10,10,3)
polygon(xx, yy, col="gray90",border = NA)
yy <- c(10,30,30,10)
polygon(xx, yy, col="gray85",border = NA)
yy <- c(30,100,100,30)
polygon(xx, yy, col="gray80",border = NA)
yy <- c(100,10000000,10000000,100)
polygon(xx, yy, col="gray75",border = NA)
text(4,0.5477226,"negative",cex=0.8)
text(4,1.732051,"weak",cex=0.8)
text(4,5.477226,"substantial",cex=0.8)
text(4,17.32051,"strong",cex=0.8)
text(4,54.77226,"very strong",cex=0.8)
text(4,173.2051,"decisive",cex=0.8)
main_title <- expression()
boxplot(bayes_factor[,], add=T, names=NA, ylab="Bayes factor", col="white",pch=".")
axis(1, at=1:3 , labels= c("contraction","","constant") , las=1, tick=F)
axis(1, at=1:3 , labels= c("","expansion","") , line=1, las=1, tick=F)
dev.off()

cbbPalette <- c("#000000", "#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00", 
                "#CC79A7", "#F0E442")

pdf(file=paste0("results/loci_14_ind_22/Results/SkylinePlot_colobus.pdf"),width=4,height=8)
par(mfrow=c(3,1), mar=c(3,3,1,1), oma=c(3,3,0,0))
for (scen in c(1,2,3) ){
  
  load(paste0("results/loci_14_ind_22/Results/",scenarios[scen],"_0.43_results.RData"))
  
  SKYtrue   <- true_demo
  SKYmedian <- t(skylineplot$median) 
  SKYlower  <- t(skylineplot$lower_95HPD)
  SKYupper  <- t(skylineplot$upper_95HPD)
  
  label_x     <- expression(tau~"(time measured in mutations)")
  label_y     <- expression(theta*"="*4*N[e]*mu)
  
  plot( generations,
        SKYmedian[,1],
        type="n",
        xlab="",
        ylab="",
        xlim=c(2.5e-4,4),
        ylim=c(1e-3,1e4), #ylim=c(-3,4),
        cex.axis=0.00001,
        log="xy")
  axis(1,
       at     = c(5e-4,5e-3,5e-2,5e-1,5) ,
       labels = c("0.0005","0.005","0.05","0.5","5") , las=1, tick=F)
  axis(2,
       at     = c(0.001,0.1,10,1000) ,
       labels = c("0.001","0.1","10","1000") , las=1, tick=F)
  
  
  for (i in 1:number_of_replicates){
    lines(generations,SKYlower[,i],col=rgb(0.6,0.6,0.6,0.5))
    lines(generations,SKYupper[,i],col=rgb(0.6,0.6,0.6,0.5))
  }
  for (i in 1:number_of_replicates){
    lines(generations,SKYmedian[,i],col=rgb(0,0,0,0.5))
  }
  lines(generations,SKYtrue,col=cbbPalette[3],lwd=2)
  
  if (scen==26){
    mtext(label_x, side=1, adj=0.5, cex=1, outer=TRUE)
    mtext(label_y, side=2, adj=0.5, cex=1, outer=TRUE)
  }
  
  if(scen==1)  legend(x="topright",legend="contraction",cex=1.2,bty="n")
  if(scen==2) legend(x="topright",legend="expansion",cex=1.2,bty="n")
  if(scen==3) legend(x="topright",legend="constant size",cex=1.2,bty="n")
  
}
dev.off()




