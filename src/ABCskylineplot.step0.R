################################################################################
#
# SCRIPT FOR ABC SKYLINE PLOT: STEP 0. Additional operations related to settings
#
################################################################################
options(scipen=99)

set.seed(seed)
DIYABC_seed <- round(runif(max_num_of_periods,1,9999))

if (project=="BEAST" | project=="Simulations"){
  project <- paste0(project,"_renamed")
  warning(paste0("project name invalid, changed to ",project))
}

if (!file.exists(paste0("results/",project))){
  mkdir_command <- paste0("mkdir results/",project)
  if(.Platform$OS.type == "unix") system( mkdir_command )  
}else{
  warning("A project (folder) with the same name exists and will be overwritten")
}


if (!simulated_target_data){

  #Reads target (genepop file) data to get sample size and number of loci
  temp <- read.genepop(inputfile, ncode=3)
  data_names_of_loci <- levels(temp$loc.fac)
  data_num_of_loci   <- nlevels(temp$loc.fac)
  data_sample_size   <- as.integer(sum(temp$ploidy))
  remove(temp)
  
  
}else{ # i.e. simulated_target_data=T

  sims_exist <- file.exists("results/Simulations")
  
  if (!sims_exist){
    # creation of .par files for simulations
    number_of_scenarios  <- length(scenarios) 
    for (scenario in scenarios_number){
      scenario_name <- scenarios[scenario]
      cat(paste("Creating .par files for scenario:",scenario_name," (listed below)\n"))
      demographic_model <- readLines(paste("src/Scenari/",scenario_name,".simcoal",sep=""))
      
      for (pGSMvalue in seq_along(true_gsm)){
        
        # name of par file 
        par_file <- paste0(scenario_name,"_",true_gsm[pGSMvalue],".par")
        
        if (!file.exists(par_file)){
          # send message on screen
          cat(paste("Creating .par file:",par_file,"\n"))
          
          demographic_model[6] <- sample_size
          
          write(demographic_model,file=par_file,ncolumn=1)
          
          write("//Number of independent loci [chromosome]", file=par_file, append=T)
          write(paste(num_of_loci,0), file=par_file, append=T)
          write("//Number of linkage blocks per chormosome", file=par_file, append=T)
          write(paste(1), file=par_file, append=T)
          write(paste("////per Block: data type, num loci, rec. rate and mut rate + optional parameters"), file=par_file, append=T)
          
          write(paste( "MICROSAT 1 0 ",
                       true_mutrate,
                       " ",true_gsm[pGSMvalue],
                       " 0",sep=""), file=par_file, append=T)
        }
      }
    }# end FOR scenarios
  }# end if (!sims_exist)
  
  data_sample_size   <- sample_size
  data_num_of_loci   <- num_of_loci
  data_names_of_loci <- paste("locus",1:num_of_loci,sep="_")
}


if (length(motif)==1){
  motif <- array(data=motif,dim=data_num_of_loci)
}else if (length(motif)!=1 && length(motif)!=data_num_of_loci){
  motif <- array(data=motif,dim=data_num_of_loci)
  warning("The number of specified motif lengths differs from the number of loci")
}
if (length(range)==1){
  range <- array(data=range,dim=data_num_of_loci)
}else if (length(range)!=1 && length(range)!=data_num_of_loci){
  range <- array(data=range,dim=data_num_of_loci)
  warning("The number of specified range of alleles differs from the number of loci")
}







if (prior_PERIODS=="Poisson") min_num_of_periods <- 1
if (prior_PERIODS=="constant") min_num_of_periods <- max_num_of_periods
# Generation of DIYABC files
for (period in min_num_of_periods:max_num_of_periods){
  
  # one directory for each model
  ##############################################################
  mkdir_command <- paste0("mkdir results/",project,"/",period,"period")
  if(.Platform$OS.type == "unix") {
    system( mkdir_command )
  }
  
  
  # header file
  ############################################################
  header_file_name <- paste0("results/",project,"/",period,"period/header.txt")
  
  # 1st line
  write(inputfile, file=header_file_name) 

  # 2nd line
  number_of_parameters <- 2*period - 1 
  write(paste(number_of_parameters,"parameters and 4 summary statistics"), file=header_file_name, append=T) 
  
  # empty line
  write("", file=header_file_name, append=T)
  
  # 4th line
  number_of_lines <- 1+period
  write(paste("1 scenarios:",number_of_lines), file=header_file_name, append=T)
  
  # 5th line
  write(paste("scenario 1 [1.0] (",number_of_parameters,")",sep=""), file=header_file_name, append=T)
  
  # lines defining scenario
  write("N0", file=header_file_name, append=T)
  write("0 sample 1", file=header_file_name, append=T)
  if (period>1){
    for (i in 1:(period-1) ) {
      write(paste("t",i," varNe 1 N",i,sep=""), file=header_file_name, append=T)
    }
  }

  # empty line
  write("", file=header_file_name, append=T)
  
  # lines defining demographic priors
  if (period>2){conditions <- 1}else{conditions <- 0}
  write( paste("historical parameters priors (",number_of_parameters,",",conditions,")",sep=""), file=header_file_name, append=T)
  write( paste("N0 N ",prior_THETA,"[",prior_THETA_min/(4*MUTRATE),",",prior_THETA_max/(4*MUTRATE),",0,0]",sep=""), file=header_file_name, append=T)
  if (period>1){
    if (period==max_num_of_periods) params_header <- c("PERIODS","THETA0")
    for (i in 1:(period-1) ) {
      write(paste("t",i," T ",prior_TAU,"[1,",prior_TAU_max/MUTRATE,",0,0]",sep=""), file=header_file_name, append=T)
      write(paste("N",i," N ",prior_THETA,"[",prior_THETA_min/(4*MUTRATE),",",prior_THETA_max/(4*MUTRATE),",0,0]",sep=""), file=header_file_name, append=T)
      if (period==max_num_of_periods) params_header <- c(params_header,paste0("TAU",i),paste0("THETA",i))
    }
  }
  if (conditions==1){
    cond <- "t1"
    for (i in 2:(period-1)){
      cond <- paste(cond,"<t",i,sep="")
    }
    write(cond, file=header_file_name, append=T)
    write("DRAW ONCE", file=header_file_name, append=T)
  }

  # empty line
  write("", file=header_file_name, append=T)

  # lines defining the loci
  write( paste("loci description (",data_num_of_loci,")",sep=""), file=header_file_name, append=T)
  for (i in 1:data_num_of_loci) {
    write( paste(data_names_of_loci[i],"[M] G1", motif[i], range[i]), file=header_file_name, append=T)
  } 

  # empty line
  write("", file=header_file_name, append=T)
  
  # lines defining mutational prior
  write("group priors (1)", file=header_file_name, append=T)
  write("group G1 [M]", file=header_file_name, append=T)
  write( paste0("MEANMU ",prior_MUTRATE,"[",prior_MUTRATE_min,",",prior_MUTRATE_max,",",prior_MUTRATE_mean,",",prior_MUTRATE_shape,"]"), file=header_file_name, append=T)
  write( paste0("GAMMU GA[",prior_MUTRATE_i_min,",",prior_MUTRATE_i_max,",",prior_MUTRATE_i_mean,",",prior_MUTRATE_i_shape,"]"), file=header_file_name, append=T)
  write( paste0("MEANP ",prior_GSM,"[",prior_GSM_min,",",prior_GSM_max,",",prior_GSM_mean,",",prior_GSM_shape,"]"), file=header_file_name, append=T)
  write( paste0("GAMP GA[",prior_GSM_i_min,",",prior_GSM_i_max,",",prior_GSM_i_mean,",",prior_GSM_i_shape,"]"), file=header_file_name, append=T)
  write( paste0("MEANSNI ",prior_SNI,"[",prior_SNI_min,",",prior_SNI_max,",",prior_SNI_mean,",",prior_SNI_shape,"]"), file=header_file_name, append=T)
  write( paste0("GAMSNI GA[",prior_SNI_i_min,",",prior_SNI_i_max,",",prior_SNI_i_mean,",",prior_SNI_i_shape,"]"), file=header_file_name, append=T)

  # empty line
  write("", file=header_file_name, append=T)
  
  # lines defining groups of summary statistics
  write("group summary statistics (4)", file=header_file_name, append=T)
  write("group G1 [M] (4)", file=header_file_name, append=T)
  write("NAL 1", file=header_file_name, append=T)
  write("HET 1", file=header_file_name, append=T)
  write("VAR 1", file=header_file_name, append=T)
  write("MGW 1", file=header_file_name, append=T)
  
  # empty line
  write("", file=header_file_name, append=T)

  # header sensu stricto (column names for reference table, i.e. parameters and summary statistics)
  header <- "scenario      N0            "
  if (period>1){
    for (i in 1:(period-1) ) {
      header <- paste0(header,"t",i,"            N",i,"            ")
    }
  }
  if (!((prior_MUTRATE=="UN" || prior_MUTRATE=="LU") && (prior_MUTRATE_min==prior_MUTRATE_max))){
    header <- paste0(header,"µmic_1        ")  
  }                          
  if (!((prior_GSM=="UN" || prior_GSM=="LU") && (prior_GSM_min==prior_GSM_max))){
    header <- paste0(header,"pmic_1       ")  
  }                          
  if (!((prior_SNI=="UN" || prior_SNI=="LU") && (prior_SNI_min==prior_SNI_max))){
    header <- paste0(header,"snimic_1      ")  
  }                          
  header <- paste0(header,"NAL_1_1       HET_1_1       VAR_1_1       MGW_1_1    ")
  write(header, file=header_file_name, append=T)
  
  #initialization of RNGs
  diyabc_command <- paste(DIYABC_exe_name," -p ",directory,"/results/",project,"/",period,"period/ -n f:t:",num_of_threads,"s:",DIYABC_seed[period]," > ",directory,"/results/",project,"/",period,"period/init_RNGs.log",sep="")
  if(!run_in_cluster) {
    diyabc_command <- paste("./",diyabc_command,sep="")
    system( diyabc_command )
  }else{
    system( diyabc_command )
  }
     
}


# Check graphic output file format option
if (!is.element(g_out,c("pdf","png","svg"))){
  g_out<-"pdf"
  cat("Error on graphic output file format setting. Set to PDF instead.\n")
  errors_on_settings <- T
}

sumstats_header <- c("NAL_1_1","HET_1_1","VAR_1_1","MGW_1_1")





# functions for additional summary statistics
Kimmel_beta <- function(He,V,log_beta=F){
  stopifnot(length(V)==length(He))
  P0 <- 1-He
  theta_V <- V
  theta_P <- (1/P0^2-1)/2
  beta <- theta_V/theta_P
  if (log_beta){
    beta <- log(beta)
  }
  return(beta)
}
diffNa <- function(M,n,obsNa){
  j <- 1:n-1
  expNa <- sum(M/(M+j))
  diffNa <- obsNa-expNa
  return(abs(diffNa))
}
bottleneck <- function(Na,He,n){
  M <- optimize(f=diffNa, interval=c(10^-6,10^6),n=n,obsNa=Na)$minimum
  expHe <- M/(1+M)
  bottleneck <- He - expHe
  return(bottleneck)
}
beta_prime <- function(Na,V,log_beta=F){
  stopifnot(length(V)==length(Na))
  theta_V  <- V
  theta_Na <- (Na^2-1)/2
  beta <- theta_V/theta_Na
  if (log_beta){
    beta <- log(beta)
  }
  return(beta)
}












# save all results from step 0
save.image( file=paste0("results/",project,"/",project,"_step0.RData") ) 
