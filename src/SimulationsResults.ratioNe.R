# Script to summarize the results from the test of the ABC Skyline Plot
# approach on simulated data sets:  Ne RATIO

# Miguel Navascués

pgsm_values <- c(0.00,0.22,0.74)
options(scipen = 999)
project <- "ratioNe"
number_of_replicates <- 100
scenarios_number <- 1:27
scenarios <- paste("Scenario", scenarios_number, sep="")

scen_table <- read.table("src/Scenari/scenari.table.txt",header=T,row.names=1)
mkdir_command <- paste0("mkdir results/",project,"/Results/ratioNe")
system( mkdir_command )

cbbPalette <- c("#000000", "#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00", 
                "#CC79A7", "#F0E442")



true_ratioNe <- scen_table$N1/scen_table$N0
true_ratioNe[25:27] <- 1
for (pgsm in seq_along(pgsm_values)){

  ratioNe          <- matrix(NA,nrow=number_of_replicates,ncol=27)
  lower95HPD       <- matrix(NA,nrow=number_of_replicates,ncol=27)
  upper95HPD       <- matrix(NA,nrow=number_of_replicates,ncol=27)
  
  
  lower_than_1  <- array(NA,27)
  higher_than_1 <- array(NA,27)
  includes_1    <- array(NA,27)
  bias          <- array(NA,27)
  MAE           <- array(NA,27)
  rel_bias          <- array(NA,27)
  rel_MAE           <- array(NA,27)
  
  for (scen in c(1:27)){
    load(paste0("results/",project,"/Results/Scenario",scen,"_",pgsm_values[pgsm],"_results.RData"))
    ratioNe[,scen]      <- ratioNe_hat
    lower95HPD[,scen]   <- ratioNe_95HPD[,1]
    upper95HPD[,scen]   <- ratioNe_95HPD[,2]
    lower_than_1[scen]  <- length(which(ratioNe_95HPD[,2]<1))/number_of_replicates
    higher_than_1[scen] <- length(which(ratioNe_95HPD[,1]>1))/number_of_replicates
    includes_1[scen]    <- 1-lower_than_1[scen]-higher_than_1[scen]
    bias[scen]          <- ratioNe_hat-true_ratioNe[scen]
    rel_bias[scen]      <- bias[scen]/true_ratioNe[scen]
    MAE[scen]           <- mean(abs(bias[scen]))
    rel_MAE[scen]       <- MAE[scen]/true_ratioNe[scen]
  }
  cbind(1:27,true_ratioNe,bias,MAE)
  cbind(1:27,true_ratioNe,rel_bias,rel_MAE)
  cbind(1:27,includes_1,higher_than_1,lower_than_1)

  
  pdf(file=paste0("results/",project,"/Results/ratioNe/RatioNe_",pgsm_values[pgsm],".pdf"),width=8,height=4)
  par(mar=c(4,5,1,1))
  position<-seq(from=0,to=0.9,length.out=number_of_replicates)
  plot(position,
       1/ratioNe[,8],
       ylim=c(10^-5,10^5),
       xlim=c(0,3),
       pch=19,
       cex=0.5,
       cex.axis=0.00001,
       log="y",
       ylab=expression(theta[0]/theta[1]),
       xlab="",
       tick=F)
  axis(2,
       at     = c(0.0001,0.01,1,100,10000) ,
       labels = c("0.0001","0.01","1","100","10000") , las=1)
  arrows(position,1/lower95HPD[,8],position,1/upper95HPD[,8], length=0.05, angle=90, code=3,col="grey")
  points(position,
         1/ratioNe[,8],
         pch=19,
         cex=0.5)
  position<-seq(from=1,to=1.9,length.out=number_of_replicates)
  arrows(position,1/lower95HPD[,20],position,1/upper95HPD[,20], length=0.05, angle=90, code=3,col="grey")
  points(position,
         1/ratioNe[,20],
         pch=19,
         cex=0.5)
  position<-seq(from=2,to=2.9,length.out=number_of_replicates)
  arrows(position,1/lower95HPD[,26],position,1/upper95HPD[,26], length=0.05, angle=90, code=3,col="grey")
  points(position,
         1/ratioNe[,26],
         pch=19,
         cex=0.5)
  abline(h=1)
  axis(1, at=c(0.5,1.5,2.5) , labels= c("contraction","","constant") , las=1, tick=)
  axis(1, at=c(0.5,1.5,2.5) , labels= c("","expansion","") , line=1, las=1, tick=F)
  dev.off()
  
  
  #pdf(file=paste0("results/",project,"/Results/ratioNe/RatioNe_",pgsm_values[pgsm],".pdf"),width=5,height=4)
  #boxplot(log10(ratioNe[,c(8,20,26)]),
  #        ylim=c(-5,4),
  #        names=NA, 
  #        ylab=expression(log[10](N[0]/N[1])), 
  #        pch=".")
  #boxplot(log10(lower95HPD[,c(8,20,26)]),
  #        names=NA, 
  #        border="grey", 
  #        pch=".",
  #        add=TRUE)
  #boxplot(log10(upper95HPD[,c(8,20,26)]),
  #        names=NA, 
  #        border="grey", 
  #        pch=".",
  #        add=TRUE)
  #axis(1, at=1:3 , labels= c("contraction","","constant") , las=1, tick=F)
  #axis(1, at=1:3 , labels= c("","expansion","") , line=1, las=1, tick=F)
  #dev.off()
  
  
    
  
}



