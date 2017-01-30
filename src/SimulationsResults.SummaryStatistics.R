# Script to summarize the results from the test of the ABC Skyline Plot
# approach on simulated data sets:  SUMMARY STATISTICS

# Miguel Navascués

pgsm_values <- c(0.00,0.22,0.74)
options(scipen = 999)
project <- "Poisson"
number_of_replicates <- 100
scenarios_number <- 1:27
scenarios <- paste("Scenario", scenarios_number, sep="")

scen_table <- read.table("src/Scenari/scenari.table.txt",header=T,row.names=1)
mkdir_command <- paste0("mkdir results/",project,"/Results/SumStats")
system( mkdir_command )

A_m   <- A_sd  <- NA
He_m  <- He_sd <- NA
V_m   <- V_sd  <- NA
M_m   <- M_sd  <- NA
B_m   <- B_sd  <- NA
Bp_m  <- Bp_sd <- NA
Bo_m  <- Bo_sd <- NA

scen_table <- cbind(scen_table,
                    He_m,He_sd,
                    A_m,A_sd,
                    M_m,M_sd,
                    V_m,V_sd,
                    #B_m,B_sd,
                    #Bp_m,Bp_sd,
                    Bo_m,Bo_sd)

# transform parameters to theta and tau
mu <- 10^-3 
scen_table[,c(1,2)] <- scen_table[,c(1,2)]*4*mu
scen_table[,3]      <- scen_table[,3]*mu

for (pgsm in seq_along(pgsm_values)){

  for (scen in 1:27){
    sumstats <- read.table(paste0("results/",project,"/Simulations/P",pgsm_values[pgsm],"/Scenario",scen,"/Scenario",scen,".sumstats"),header=T)
    sumstats <- sumstats[,c(1:4,7)]
    scen_table[scen,c("A_m","He_m","V_m","M_m","Bo_m")] <- apply(sumstats,2,function(x) round(mean(x),digits=2) )
    scen_table[scen,c("A_sd","He_sd","V_sd","M_sd","Bo_sd")] <- apply(sumstats,2,function(x) round(sd(x),digits=2))
  }
  write.table(scen_table,file=paste0("results/",project,"/Results/SumStats/SummaryStatistics_",pgsm_values[pgsm],".txt"))
  
  for (scen in 1:27){
    latex <- paste0(scen_table[scen,1],  " & ",
                    scen_table[scen,2],  " & ",
                    scen_table[scen,3],  " & ",
                    scen_table[scen,4],  " & (",
                    scen_table[scen,5],  ") & ",
                    scen_table[scen,6],  " & (",
                    scen_table[scen,7],  ") & ",
                    scen_table[scen,8],  " & (",
                    scen_table[scen,9],  ") & ",
                    scen_table[scen,10],  " & (",
                    scen_table[scen,11],  ") & ",
                    scen_table[scen,12],  " & (",
                    scen_table[scen,13],  ") \\\\")
    if (scen==1) first_line <- T
    write(latex,file=paste0("results/",project,"/Results/SumStats/SummaryStatistics_",pgsm_values[pgsm],"_LaTeX.txt"),append=!first_line)
    first_line <- F
  }
}  



