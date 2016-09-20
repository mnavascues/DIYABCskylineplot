################################################################################
#
# SCRIPT FOR ABC SKYLINE PLOT: STEP 2. CALCULATE SUMMARY STATISTICS FOR TARGET DATA
#
################################################################################



# set name of DIYABC file of target data
diyabc_statobs <- paste(project,"/",max_num_of_periods,"period/statobs.txt",sep="")

if (!simulated_target_data){
      cat(paste("Rading summary statistics from target data\n"))
  
      # summary statistics output file
      sumstats_file <- paste(project,"/target.sumstats",sep="")
        
      sumstats <- scan( diyabc_statobs,skip=2)
      
      sumstats <- c(sumstats,
                    Kimmel_beta(sumstats[2],sumstats[3],log=T),
                    beta_prime(sumstats[1],sumstats[3],log=T),
                    bottleneck(sumstats[1],sumstats[2],data_sample_size))

      write(sumstats_header, file=sumstats_file, ncolumns=length(sumstats_header), append=F)
      write(sumstats,        file=sumstats_file, ncolumns=length(sumstats_header), append=T)
      
}else{ # CALCULATE SUMMARY STATS FOR SIMULATED TARGET DATA
  
  for (pGSMvalue in seq_along(true_gsm)){
    
    pGSMfolder <- paste0("P", true_gsm[pGSMvalue])
    for (scenario in scenarios_number) {
      cat("\nCalculating summary statistics from",scenarios[scenario],"\n")
      
      # summary statistics output file
      sumstats_file <- paste("Simulations/",pGSMfolder,"/",scenarios[scenario],"/",scenarios[scenario],".sumstats",sep="")
      
      sumstats_file_exist <- file.exists(sumstats_file)
      
      if(!sumstats_file_exist){
        pb <- txtProgressBar(min=0, max=number_of_replicates, initial=0, char=".", style=3)
        for (rep in 1:number_of_replicates){
          setTxtProgressBar(pb,rep)
          
          copy_genepop_command <- paste("cp ","Simulations/",pGSMfolder,"/Scenario",scenario,"/Scenario",scenario,"_",rep,".gen ",project,"/",max_num_of_periods,"period/",inputfile,sep="")
          if(.Platform$OS.type == "unix") {
            system( copy_genepop_command )
          }else{
            print("not implemented in non-unix OS")#shell( copy_genepop_command )
          }  
          
          #run 1 simulation in DIYABC to get sumstats from simulated target data
          diyabc_command <- paste(DIYABC_exe_name," -p ",directory,"/",project,"/",max_num_of_periods,"period/ -r 1 -g 1 -m -t 1 > ",directory,"/",project,"/",max_num_of_periods,"period/run_sims4target_data.log",sep="")
          if(!run_in_cluster) {
            diyabc_command <- paste("./",diyabc_command,sep="")
            system( diyabc_command )
          }else{
            system( diyabc_command )
          }
          
          sumstats <- scan( diyabc_statobs,skip=2)
          
          sumstats <- c(sumstats,
                        Kimmel_beta(sumstats[2],sumstats[3],log=T),
                        beta_prime(sumstats[1],sumstats[3],log=T),
                        bottleneck(sumstats[1],sumstats[2],sample_size))
          
          if (rep==1) write(sumstats_header, file=sumstats_file, ncolumns=length(sumstats_header), append=F)
          write(sumstats,                    file=sumstats_file, ncolumns=length(sumstats_header), append=T)
          
          
        }# end FOR replicates  
        
      }else{
        warning("Using summary statistics in a file that already exists")
      }
      
    }# end FOR scenarios
  }# end FOR pGSM value  
  
}# end IF-ELSE simulated target data




# save all results from step 2
save.image( file=paste(project,"/",project,"_step2.RData",sep="") ) 
