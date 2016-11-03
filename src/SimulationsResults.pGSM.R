# Script to summarize the results from the test of the ABC Skyline Plot
# approach on simulated data sets:  MUTATION MODEL: PGSM

# Miguel Navascués


pgsm_values <- 0.22 #c(0.00,0.22,0.74)
options(scipen = 999)
project <- "test"
number_of_replicates <- 30
scenarios_number <- c(8,20,26) #1:27
scenarios <- paste("Scenario", scenarios_number, sep="")


scen_table <- read.table("src/Scenari/scenari.table.txt",header=T,row.names=1)
mkdir_command <- paste0("mkdir results/",project,"/Results/pGSM")
system( mkdir_command )

MAE  <- NA
BIAS <- NA
outCI   <- NA
mean_pGSM_hat <- NA

scen_table <- cbind(scen_table,mean_pGSM_hat,MAE,BIAS,outCI)
# transform parameters to theta and tau
mu <- 1e-3 
scen_table[,c(1,2)] <- scen_table[,c(1,2)]*4*mu
scen_table[,3]      <- scen_table[,3]*mu

for (pgsm in seq_along(pgsm_values)){
  
  true_value <- pgsm_values[pgsm]
  for (scen in seq_along(scenarios_number)){
    load(paste0("results/",project,"/Results/",scenarios[scen],"_",pgsm_values[pgsm],"_results.RData"))
    
    scen_table[scenarios_number[scen],"outCI"] <- (number_of_replicates - length(intersect(which(true_value>pGSM_95HPD[,1]), which(true_value<pGSM_95HPD[,2]))))/number_of_replicates
    
    
    error <- pGSM_hat-true_value
    mean_pGSM_hat <- mean(pGSM_hat)
    bias  <- mean(error)
    mae   <- mean(abs(error))
    
    scen_table[scenarios_number[scen],"mean_pGSM_hat"] <- mean_pGSM_hat
    scen_table[scenarios_number[scen],"MAE"] <- mae
    scen_table[scenarios_number[scen],"BIAS"] <- bias
  }
  save(scen_table,file=paste0("results/",project,"/Results/pGSM/pGSMestimate_",true_value,".RData") )
  
}

