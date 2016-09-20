# Script to summarize the results from the test of the ABC Skyline Plot
# approach on simulated data sets:  MUTATION MODEL: PGSM

# Miguel Navascués

model <- "P22"
true_value <- 0.22

options(scipen = 999)
scen_table <- read.table("Scenari/scenari.table.txt",header=T,row.names=1)
mkdir_command <- paste0("mkdir ",model,"/Results")
system( mkdir_command )

number_of_replicates <- 1000

MAE  <- NA
BIAS <- NA
outCI   <- NA
mean_pGSM_hat <- NA

scen_table <- cbind(scen_table,mean_pGSM_hat,MAE,BIAS,outCI)
# transform parameters to theta and tau
mu <- 1e-3 
scen_table[,c(1,2)] <- scen_table[,c(1,2)]*4*mu
scen_table[,3]      <- scen_table[,3]*mu


for (scen in 1:27){
  load(paste0(model,"/Scenario",scen,"/Scenario",scen,"_results.RData"))

  scen_table[scen,"outCI"] <- (number_of_replicates - length(intersect(which(true_value>pGSM_95HPD[,1]), which(true_value<pGSM_95HPD[,2]))))/number_of_replicates

    
  error <- pGSM_hat-true_value
  mean_pGSM_hat <- mean(pGSM_hat)
  bias  <- mean(error)
  mae   <- mean(abs(error))
  
  scen_table[scen,"mean_pGSM_hat"] <- mean_pGSM_hat
  scen_table[scen,"MAE"] <- mae
  scen_table[scen,"BIAS"] <- bias
}

save(scen_table,file=paste0(model,"/Results/","pGSM.RData") )
