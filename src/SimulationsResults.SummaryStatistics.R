# Script to summarize the results from the test of the ABC Skyline Plot
# approach on simulated data sets:  SUMMARY STATISTICS

# Miguel Navascués

model <- "P22"

options(scipen = 999)
scen_table <- read.table("Scenari/scenari.table.txt",header=T,row.names=1)
mkdir_command <- paste0("mkdir ",model,"/Results")
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
                    B_m,B_sd,
                    Bp_m,Bp_sd,
                    Bo_m,Bo_sd)

# transform parameters to theta and tau
mu <- 10^-3 
scen_table[,c(1,2)] <- scen_table[,c(1,2)]*4*mu
scen_table[,3]      <- scen_table[,3]*mu


for (scen in 1:27){
  
  sumstats <- read.table(paste0(model,"/Scenario",scen,"/Scenario",scen,".sumstats"),header=T)
  scen_table[scen,c("A_m","He_m","V_m","M_m","B_m","Bp_m","Bo_m")] <- apply(sumstats,2,function(x) round(mean(x),digits=2) )
  scen_table[scen,c("A_sd","He_sd","V_sd","M_sd","B_sd","Bp_sd","Bo_sd")] <- apply(sumstats,2,function(x) round(sd(x),digits=2))
  
}
write.table(scen_table,file=paste0(model,"/Results/SummaryStatistics.txt"))

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
                  scen_table[scen,13],  ") & ",
                  scen_table[scen,14],  " & (",
                  scen_table[scen,15],  ") & ",
                  scen_table[scen,16], " & (",
                  scen_table[scen,17], ") \\\\")
  if (scen==1) first_line <- T
  write(latex,file=paste0(model,"/Results/SummaryStatisticsLaTeX.txt"),append=!first_line)
  first_line <- F
}



