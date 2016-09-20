################################################################################
#
# SCRIPT FOR ABC SKYLINE PLOT: STEP 1. SAMPLE MODEL (NUMBER OF PERIODS) FROM PRIOR & RUN SIMULATIONS (with DIYABC)
#
################################################################################

num_of_sims_per_period <- array(0,max_num_of_periods)
# sample number of periods
if (prior_PERIODS=="Poisson"){
  PERIODS <- rpois(num_of_sims,Poisson_lambda)+1
  PERIODS[which(PERIODS>max_num_of_periods)]<-max_num_of_periods
  for (period in 1:max_num_of_periods){
    num_of_sims_per_period[period] <- length(which(PERIODS==period))
  }
  rm(PERIODS)
  num_of_sims_per_period[which(num_of_sims_per_period==0)]<-1
}else if (prior_PERIODS=="constant"){
  num_of_sims_per_period[max_num_of_periods] <- num_of_sims
}


for (period in min_num_of_periods:max_num_of_periods){

  
  # genepop file (original data file)
  ############################################################  
  if (simulated_target_data==F) {
    copy_genepop_command <- paste("cp ",inputfile," ",project,"/",period,"period/",inputfile,sep="")
  }else{
    copy_genepop_command <- paste("cp ","Simulations/P",true_gsm[1],"/Scenario",scenario,"/Scenario",scenario,"_1.gen ",project,"/",period,"period/",inputfile,sep="")
  }
  if(.Platform$OS.type == "unix") {
    system( copy_genepop_command )
  }else{
    print("not implemented in non-unix OS")#shell( copy_genepop_command )
  }  
  
  
  
  
  #RNGs check
  diyabc_command <- paste(DIYABC_exe_name," -z ",directory,"/",project,"/",period,"period/RNG_state_0000.bin > ",directory,"/",project,"/",period,"period/check_RNGs.log",sep="")
  if(!run_in_cluster) {
    diyabc_command <- paste("./",diyabc_command,sep="")
    system( diyabc_command )
  }else{
    system( diyabc_command )
  }
  
  
  #run simulations
  diyabc_command <- paste(DIYABC_exe_name," -p ",directory,"/",project,"/",period,"period/ -r ",num_of_sims_per_period[period]," -g ",batch_size," -m -t ",num_of_threads," > ",directory,"/",project,"/",period,"period/run_sims.log",sep="")
  if(!run_in_cluster) {
    diyabc_command <- paste("./",diyabc_command,sep="")
    system( diyabc_command )
  }else{
    system( diyabc_command )
  }
  
  #translating a reftable.bin in reftable.txt
  diyabc_command <- paste(DIYABC_exe_name," -p ",directory,"/",project,"/",period,"period/ -x > ",directory,"/",project,"/",period,"period/bin2txt.log",sep="")
  if(!run_in_cluster) {
    diyabc_command <- paste("./",diyabc_command,sep="")
    system( diyabc_command )
  }else{
    system( diyabc_command )
  }
}  

sumstats_header <- c("NAL_1_1","HET_1_1","VAR_1_1","MGW_1_1")

for (period in min_num_of_periods:max_num_of_periods){

  temp_file_name <- paste(directory,"/",project,"/",period,"period/reftable.txt",sep="")
  sims_in_reftable <- scan(temp_file_name,n=1)
  lines_to_skip <- which(readLines(temp_file_name)=="4 summary statistics")
  reftable <- read.table(temp_file_name,skip=lines_to_skip,nrows =sims_in_reftable)
  
  reftable_header <- c("scenario","N0")
  if (period>1){
    for (i in 1:(period-1)){
      reftable_header <- c(reftable_header,paste0("t",i),paste0("N",i))
    }
  }
  number_of_mut_parameters <- 0
  mut_params_header        <- character()
  if (!((prior_MUTRATE=="UN" || prior_MUTRATE=="LU") && (prior_MUTRATE_min==prior_MUTRATE_max))){
    reftable_header   <- c(reftable_header,"µmic_1")
    number_of_mut_parameters <- number_of_mut_parameters+1
    mut_params_header <- c(mut_params_header,"µmic_1")
  }                          
  if (!((prior_GSM=="UN" || prior_GSM=="LU") && (prior_GSM_min==prior_GSM_max))){
    reftable_header <- c(reftable_header,"pmic_1")  
    number_of_mut_parameters <- number_of_mut_parameters+1
    mut_params_header <- c(mut_params_header,"pmic_1")
  }                          
  if (!((prior_SNI=="UN" || prior_SNI=="LU") && (prior_SNI_min==prior_SNI_max))){
    reftable_header <- c(reftable_header,"snimic_1")
    number_of_mut_parameters <- number_of_mut_parameters+1
    mut_params_header <- c(mut_params_header,"snimic_1")
  }                          
  reftable_header<- c(reftable_header,sumstats_header)
  names(reftable) <- reftable_header

  number_of_parameters <- 2*period - 1

  if (period==min_num_of_periods) {
    stats <- reftable[,sumstats_header]
  }else{
    stats <- rbind (stats,reftable[,sumstats_header])
  }
  
  if (period==min_num_of_periods) {
    mut_params <- as.matrix(reftable[,mut_params_header])
  }else{
    mut_params <- rbind (mut_params, as.matrix(reftable[,mut_params_header]) )
  }
  
  
  if (!((prior_SNI=="UN" || prior_SNI=="LU") && (prior_SNI_min==prior_SNI_max))){
    reftable[,"N0"]<-reftable[,"N0"]*4*(MUTRATE+reftable[,"snimic_1"])
    if (period>1){
      for (i in 1:(period-1)){
        reftable[,paste0("t",i)] <- reftable[,paste0("t",i)]*(MUTRATE+reftable[,"snimic_1"])
        reftable[,paste0("N",i)] <- reftable[,paste0("N",i)]*4*(MUTRATE+reftable[,"snimic_1"])
      }
    }
  }else{
    reftable[,"N0"]<-reftable[,"N0"]*4*MUTRATE
    if (period>1){
      for (i in 1:(period-1)){
        reftable[,paste0("t",i)] <- reftable[,paste0("t",i)]*MUTRATE
        reftable[,paste0("N",i)] <- reftable[,paste0("N",i)]*4*MUTRATE
      }
    }
  }                          
  
  temp_params<-cbind(period,reftable[,reftable_header[1:number_of_parameters+1]])
  if (period!=max_num_of_periods){
    for(i in period:(max_num_of_periods-1)){
      temp_params <- cbind(temp_params,array(prior_TAU_max,sims_in_reftable),temp_params[,period*2])
    }
  }
  colnames(temp_params) <- params_header
  
  if (period==min_num_of_periods){
    params <- temp_params 
  }else{
    params <- rbind (params,temp_params)
  } 
  rm(temp_params)
  
}


bottleneck_stat <- array(NA,dim=nrow(stats))
for (sim in seq_len(nrow(stats))){
  bottleneck_stat[sim] <- bottleneck(stats$NAL_1_1[sim],stats$HET_1_1[sim],data_sample_size)
}

sumstats_header <- c(sumstats_header,"beta","beta_prime","bottleneck")
stats <- cbind(stats,
               Kimmel_beta(stats$HET_1_1,stats$VAR_1_1,log=T),
               beta_prime(stats$NAL_1_1,stats$VAR_1_1,log=T),
               bottleneck_stat)
names(stats)<-sumstats_header

# remove 100% monomorphic simulations
sims2keep <- which(stats$NAL_1_1!=1.0)
stats      <- stats[sims2keep,]
params     <- params[sims2keep,]
mut_params <- as.matrix(mut_params[sims2keep,])

save(stats, file = paste(project,"/",project,".sumstats.RData",sep="") )
rm(stats)

save(params, file = paste(project,"/",project,".params.RData",sep="") )
rm(params)

dimnames(mut_params) <- list(NULL,mut_params_header)
save(mut_params, file = paste(project,"/",project,".mutparams.RData",sep="") )
rm(mut_params)


# save all results from step 1
save.image( file=paste(project,"/",project,"_step1.RData",sep="") ) 

