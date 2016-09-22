bash_file <- "src/RatioNe.sh"

# write first line of BASH script
write(x="#!/bin/bash",file=bash_file,ncolumns=1,append=F)

R_command <- "R --no-save --args"
R_command <- paste(R_command,"num_of_sims             1000000")
R_command <- paste(R_command,"proportion_of_sims_kept 0.01")
R_command <- paste(R_command,"seed                    6132")
R_command <- paste(R_command,"num_of_points           50")
R_command <- paste(R_command,"prior_PERIODS          'constant'")
R_command <- paste(R_command,"max_num_of_periods      2")
R_command <- paste(R_command,"prior_THETA            'LU'")
R_command <- paste(R_command,"prior_THETA_min         1E-3")
R_command <- paste(R_command,"prior_THETA_max         1E4")
R_command <- paste(R_command,"prior_TAU              'UN'")
R_command <- paste(R_command,"prior_TAU_max           10")
R_command <- paste(R_command,"prior_GSM              'UN'")
R_command <- paste(R_command,"prior_GSM_min           0.0")
R_command <- paste(R_command,"prior_GSM_max           1.0")
R_command <- paste(R_command,"project                'RatioNe'")
R_command <- paste(R_command,"number_of_replicates    1000")
R_command <- paste(R_command,"sample_size             50")
R_command <- paste(R_command,"num_of_loci             100")
R_command <- paste(R_command,"true_mutrate            1e-3")
R_command <- paste(R_command,"true_gsm               'c(0.00,0.22,0.74)'")
R_command <- paste(R_command,"< ./src/ABCskylineplot.R")
write(x=R_command,file=bash_file,ncolumns=1,append=T)
system(paste("chmod +x",bash_file))

