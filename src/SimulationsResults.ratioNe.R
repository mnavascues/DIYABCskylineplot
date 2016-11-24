# Script to summarize the results from the test of the ABC Skyline Plot
# approach on simulated data sets:  Ne RATIO

# Miguel Navascués

pgsm_values <- c(0.00,0.22,0.74)
options(scipen = 999)
project <- "ratioNeLU"
number_of_replicates <- 100
scenarios_number <- 1:27
scenarios <- paste("Scenario", scenarios_number, sep="")

scen_table <- read.table("src/Scenari/scenari.table.txt",header=T,row.names=1)
mkdir_command <- paste0("mkdir results/",project,"/Results/ratioNe")
system( mkdir_command )

for (pgsm in seq_along(pgsm_values)){

  ratioNe          <- matrix(NA,nrow=number_of_replicates,ncol=27)
  lower95HPD       <- matrix(NA,nrow=number_of_replicates,ncol=27)
  upper95HPD       <- matrix(NA,nrow=number_of_replicates,ncol=27)

  lower_than_1  <- array(NA,27)
  higher_than_1 <- array(NA,27)
  includes_1    <- array(NA,27)

  for (scen in c(1:27)){
    load(paste0("results/",project,"/Results/Scenario",scen,"_",pgsm_values[pgsm],"_results.RData"))
    ratioNe[,scen]      <- ratioNe_hat
    lower95HPD[,scen]   <- ratioNe_95HPD[,1]
    upper95HPD[,scen]   <- ratioNe_95HPD[,2]
    lower_than_1[scen]  <- length(which(ratioNe_95HPD[,2]<1))/number_of_replicates
    higher_than_1[scen] <- length(which(ratioNe_95HPD[,1]>1))/number_of_replicates
    includes_1[scen]    <- 1-lower_than_1[scen]-higher_than_1[scen]
  }
  
  
  
  pdf(file=paste0("results/",project,"/Results/ratioNe/RatioNe_",pgsm_values[pgsm],".pdf"),width=5,height=4)
  boxplot(log10(ratioNe[,c(8,20,26)]),
          ylim=c(-5,4),
          names=NA, 
          ylab=expression(log[10](N[0]/N[1])), 
          pch=".")
  boxplot(log10(lower95HPD[,c(8,20,26)]),
          names=NA, 
          border="grey", 
          pch=".",
          add=TRUE)
  boxplot(log10(upper95HPD[,c(8,20,26)]),
          names=NA, 
          border="grey", 
          pch=".",
          add=TRUE)
  
  axis(1, at=1:3 , labels= c("contraction","","constant") , las=1, tick=F)
  axis(1, at=1:3 , labels= c("","expansion","") , line=1, las=1, tick=F)
  dev.off()
  
  
    
  
}



