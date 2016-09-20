################################################################################
#
# SCRIPT FOR ABC SKYLINE PLOT: STEP 0.5 PERFORM SIMULATIONS TO ACT AS TARGET DATA 
#
################################################################################

# If simulations have already been performed they are in folder "Simulations"
sims_exist <- file.exists("Simulations")

if (simulated_target_data & !sims_exist){
  if(.Platform$OS.type == "unix") system( "mkdir Simulations" )  
  
  random_sim <- array(NA,number_of_scenarios)
  
  for (pGSMvalue in seq_along(true_gsm)){
    
    pGSMfolder <- paste0("P", true_gsm[pGSMvalue])
    mkdir_command <- paste("mkdir Simulations/",pGSMfolder,sep="")
    if(.Platform$OS.type == "unix") system( mkdir_command )  
    
    for (scenario in scenarios_number) {
      
      scenario_name <- scenarios[scenario]
      
      cat(paste("Simulation of target data for",scenario_name,"\n"))
      
      # generate seed.txt file for fastsimcoal
      write(paste("fastsimcoal seed initialized with",as.integer(runif(1,1,1000000))) , file="seed.txt", append = F) 
      
      # name of par file 
      par_file <- paste0(scenario_name,"_",true_gsm[pGSMvalue],".par")
      
      # write fastsimcoal command
      fastsimcoal_run <- paste( "fastsimcoal",
                                "-i", par_file,             # parameter file (.par)
                                "-n", number_of_replicates) # number of simulations per combination of parameter values
      if (quiet)  fastsimcoal_run <- paste(fastsimcoal_run, "-q")    # run in quiet mode
      fastsimcoal_run <- paste0(fastsimcoal_run, " > fastsimcoal",scenario,".log")
      
      # run fastsimcoal command
      if(.Platform$OS.type == "unix") {
        fastsimcoal_run <- paste( "./", fastsimcoal_run, sep="" )
        system( fastsimcoal_run )
      }else{
        print("not implemented in non-unix OS")#shell( fastsimcoal_run )
      }
      
      #sample one random simulation to look in more detail
      random_sim[scenario] <- sample(number_of_replicates,1)
      
      # one direcotory for each scenario
      ##############################################################
      mkdir_command <- paste("mkdir Simulations/",pGSMfolder,"/Scenario",scenario,sep="")
      if(.Platform$OS.type == "unix") {
        system( mkdir_command )
      }else{
        print("not implemented in non-unix OS")#shell( mkdir_command )
      }
      
      
      
      #create an input file to use with Beauti + BEAST
      if (do_BEAST_input){
        BEAST_file_name <- paste0("Simulations/",pGSMfolder,"/Scenario",scenario,"/Scen",scenario,"_sim",random_sim[scenario],"_input4BEAUTi.txt")
        temp_file_name <- paste0(scenario_name,"_",true_gsm[pGSMvalue],"/",scenario_name,"_",true_gsm[pGSMvalue],"_1_",random_sim[scenario],".arp")
        
        lines_to_skip <- which(readLines(temp_file_name)=="\t\tSampleData= {")
        
        write("#microsat",BEAST_file_name,1)
        write( paste("#name","Scenario",scenario) , BEAST_file_name , 1 ,append=T)
        BEAST_loci_line <- "id"
        for (l in 1:num_of_loci){
          BEAST_loci_line <- paste(BEAST_loci_line," Locus",l,sep="")
        }
        write(BEAST_loci_line,BEAST_file_name,1,append=T)
        
        random_sim_data <- read.table(temp_file_name,skip=lines_to_skip,nrows=sample_size)
        
        random_sim_data <- cbind(1:sample_size,random_sim_data[,(1:num_of_loci)+2])
        write(t(as.matrix(random_sim_data)),BEAST_file_name,num_of_loci+1,append=T)
        
      }
      
      #convert arlequin format into genepop format
      for (replic in 1:number_of_replicates){
        temp_file_name <- paste0(scenario_name,"_",true_gsm[pGSMvalue],"/",scenario_name,"_",true_gsm[pGSMvalue],"_1_",replic,".arp")
        out_file_name <- paste0("Simulations/",pGSMfolder,"/Scenario",scenario,"/Scenario",scenario,"_",replic,".gen")


                
        lines_to_skip <- which(readLines(temp_file_name)=="\t\tSampleData= {")
        
        temp <- read.table( file=temp_file_name, skip = lines_to_skip, nrows = sample_size)
        
        write( paste("Simulated data with FastSimCoal: scenario",scenario,"simulation",replic), file = out_file_name, append = F)
        for (l in 1:num_of_loci){
          write( paste("Locus",l,sep=""), file = out_file_name, append = T)
        }
        write( "POP", file = out_file_name, append = T)
        for (i in 1:(sample_size/2)){
          genotyp <- paste(i," , ")
          for (j in 1:num_of_loci+2){
            genotyp <- paste (genotyp," ",temp[i*2,j],temp[i*2-1,j],sep="")
          }
          write( genotyp, file = out_file_name, append = T)
        }
        
        
      }
      
      if(remove_fastsimcoal_output){
        cat(paste("\nRemoving fastsimcoal output\n"))
        
        
        if(.Platform$OS.type == "unix") {
          rm_command <- paste0("rm -r ",scenario_name,"_",true_gsm[pGSMvalue])
          system( rm_command )
          rm_command <- paste0("rm ",scenario_name,"_",true_gsm[pGSMvalue],".par")
          system( rm_command )
          rm_command <- paste0("rm fastsimcoal",scenario,".log")
          system( rm_command )
        }else{
          print("not implemented in non-unix OS")
          #del_command <- paste("del /f/s/q",scenarios[scenario],"> nul")
          #rmdir_command <- paste("rmdir /s/q",scenarios[scenario],"> nul")
          #shell( del_command )
          #shell( rmdir_command )
        }
      }  
      
      
    }#end FOR scenarios
    
  }#end FOR pGSM values
  
  
}#end IF simulated_target_data

  
  
  
  


# save all results from step 0.5
save.image( file=paste(project,"/",project,"_step0.5.RData",sep="") ) 