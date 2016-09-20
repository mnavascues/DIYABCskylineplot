bash_file <- "src/RatioNe.sh"

# write first line of BASH script
write(x="#!/bin/bash",file=bash_file,ncolumns=1,append=F)

R_command <- "R --no-save --args"
R_command <- paste(R_command,"num_of_sims 1000000")
R_command <- paste(R_command,"proportion_of_sims_kept 0.01")
R_command <- paste(R_command,"seed 6132")
R_command <- paste(R_command,"num_of_points 50")
R_command <- paste(R_command,"prior_PERIODS 'constant'")
R_command <- paste(R_command,"")
R_command <- paste(R_command,"")
R_command <- paste(R_command,"")
R_command <- paste(R_command,"")
R_command <- paste(R_command,"")
R_command <- paste(R_command,"")

