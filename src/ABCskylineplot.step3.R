################################################################################
#
# SCRIPT FOR ABC SKYLINE PLOT: STEP 3. ABC & SKYLINE PLOT
#
################################################################################



# load parameters reference table
load( file = paste0("results/",project,"/",project,".params.RData") )
load( file = paste0("results/",project,"/",project,".mutparams.RData") )
# load statistics for reference table
load( file = paste0("results/",project,"/",project,".sumstats.RData") )

G <- paste("G",1:num_of_points,sep="")
Tmax <- prior_TAU_max    

newparams <- matrix(data=NA,
                    nrow=dim(params)[1],
                    ncol=num_of_points,
                    dimnames = list(1:dim(params)[1],G))

generations <- seq(from=0, to=Tmax, length.out=num_of_points)

for (gen in 1:length(generations)){
  lower_than_TAU1 <- which( generations[gen] < params[,"TAU1"])
  newparams[ lower_than_TAU1 , gen ] <- params[ lower_than_TAU1, "THETA0"]
  if (max_num_of_periods==2){
    higher_than_TAU <- which( generations[gen] >= params[,"TAU1"])
    newparams[ higher_than_TAU , gen ] <- params[ higher_than_TAU, "THETA1"] 
  }else{
    for (period in min_num_of_periods:(max_num_of_periods-1)){
      higher_than_TAU <- which( generations[gen] >= params[,paste("TAU",period,sep="")])
      newparams[ higher_than_TAU , gen ] <- params[ higher_than_TAU, paste("THETA",period,sep="")] 
    }
  }
}
save(newparams,file = paste0("results/",project,"/",project,"_newparams.RData"))



if (simulated_target_data) {
  mkdir_command <- paste0("mkdir results/",project,"/Results/Error")
  if(.Platform$OS.type == "unix") system( mkdir_command )
  
  for (pGSMvalue in seq_along(true_gsm)){
    
    pGSMfolder <- paste0("P", true_gsm[pGSMvalue])

    for (scenario in scenarios_number) {
      
      source(paste0("src/Scenari/",scenarios[scenario],".R"))
      
      
      true_demo <- demography(generations/true_mutrate)*true_mutrate*2
      
      sky_median   <- matrix(data = NA, nrow = number_of_replicates , ncol = length(generations) )
      sky_lowerHPD <- matrix(data = NA, nrow = number_of_replicates , ncol = length(generations) )
      sky_upperHPD <- matrix(data = NA, nrow = number_of_replicates , ncol = length(generations) )
      
      test_constant_model_posterior <- matrix(NA, nrow = number_of_replicates , ncol = 2 , dimnames=list(NULL,c("Constant","Variable")) )
      
      pGSM_hat   <- array(NA,dim=number_of_replicates)
      pGSM_95HPD <- matrix(NA, nrow = number_of_replicates , ncol = 2 )
      
      ratioNe_hat   <- array(NA,dim=number_of_replicates)
      ratioNe_95HPD <- matrix(NA, nrow = number_of_replicates , ncol = 2 )
      
      target <- read.table(file = paste0("results/Simulations/",pGSMfolder,"/",scenarios[scenario],"/",scenarios[scenario],".sumstats"), header = T)
      target <- target[sumstats_header]
      
      constant_model <- params$PERIODS
      constant_model[constant_model==1]           <- "Constant"
      constant_model[constant_model!="Constant"]  <- "Variable"
      
      
      print("Calculating skyline plots for simulated target data\n")
      pb <- txtProgressBar(min=0, max=number_of_replicates, initial=0, char=".", style=3)
      
      for(sim in 1:number_of_replicates){
        setTxtProgressBar(pb,sim)
        
        abc_target <- target[sim,sumstats_header]
        abcresult <- NA
        abcresult <- abc(target  = abc_target,
                         param   = newparams,
                         sumstat = stats[sumstats_header],
                         tol     = proportion_of_sims_kept,
                         method  = "loclinear",
                         hcorr   = F,
                         transf  = "log",
                         trace   = F)
        abcresult <- summary(abcresult,print=F)
        sky_median[sim,]   <- abcresult[3,]
        sky_lowerHPD[sim,] <- abcresult[2,]
        sky_upperHPD[sim,] <- abcresult[6,]
        
        if (prior_PERIODS=="Poisson"){
          model_choice <- postpr(target  = abc_target,
                                 index   = constant_model,
                                 sumstat = stats[sumstats_header],
                                 tol     = proportion_of_sims_kept,
                                 corr    = F,
                                 method  = "mnlogistic",
                                 trace=F)
          test_constant_model_posterior[sim,] <- model_choice$pred
        }else{
          model_choice <-NULL
        }
        
        if (max_num_of_periods==2){
          ratioNe <- params[,"THETA1"]/params[,"THETA0"]
          abcresult <- abc(target  = abc_target,
                           param   = ratioNe,
                           sumstat = stats[,sumstats_header],
                           tol     = proportion_of_sims_kept,
                           method  = "loclinear",
                           transf  = "log",
                           hcorr   = F,
                           trace=T)
          ratioNe_hat[sim]    <- summary(abcresult,print=F)[3]  
          ratioNe_95HPD[sim,] <- c(summary(abcresult,print=F)[2],summary(abcresult,print=F)[6])
        }
        
        if (length(which(dimnames(mut_params)[[2]]=="pmic_1"))==1){
          abcresult <- abc(target  = abc_target,
                           param   = mut_params[,"pmic_1"],
                           sumstat = stats[,sumstats_header],
                           tol     = proportion_of_sims_kept,
                           method  = "loclinear",
                           hcorr   = T,
                           transf  = "logit",
                           logit.bounds = matrix(c(prior_GSM_min,prior_GSM_max),ncol=2),
                           trace=T)
          
          pGSM_hat[sim]    <- summary(abcresult,print=F)[3]  
          pGSM_95HPD[sim,] <- c(summary(abcresult,print=F)[2],summary(abcresult,print=F)[6])
        }
        
      }
      
      skylineplot <- list(median=sky_median,lower_95HPD=sky_lowerHPD,upper_95HPD=sky_upperHPD)
      test_constant_model <- list(prior=model_choice$nmodels/sum(model_choice$nmodels),posterior=test_constant_model_posterior)
      
      error    <- t(apply(sky_median,1,function(x) x-true_demo))
      bias     <- apply(error,2,function(x) mean(x, na.rm=T) )
      rel_bias <- bias/true_demo
      mae      <- apply(error,2,function(x) mean(abs(x), na.rm=T) )
      rel_mae  <- mae/true_demo
      sky_error <- list(bias=bias,relative_bias=rel_bias,mean_absolute_error=mae,relative_mean_absolute_error=rel_mae)
      
      file_name <- paste0("results/",project,"/Results/Error/",scenarios[scenario],"_",true_gsm[pGSMvalue],"_ABCsky_error.",g_out)
      if (g_out=="png") png(filename=file_name, width=11.7, height=8.3, units = "in", res=300)
      if (g_out=="svg") svg(filename=file_name, width=11.7, height=8.3)
      if (g_out=="pdf") pdf(file    =file_name, width=11.7, height=8.3)
      
      par(cex.axis=2.5,cex.lab=2.5,mar=c(5.5,5.5,2,2))
      
      generations <- seq(from=0, to=Tmax, length.out=num_of_points)
      limits_on_x <- c(0,max(generations))
      label_x     <- "t (mutations/locus)"
      label_y     <- expression("relative absolute error and relative bias on "*theta)
      
      limits_on_y <- c( min(rel_mae,rel_bias,0, na.rm=T) , max(rel_mae,rel_bias, na.rm=T) )
      
      plot( generations,
            rel_bias,
            col="black",type="l",lty=2,lwd=3,
            xlab=label_x,
            ylab=label_y,
            ylim=limits_on_y,
            xlim=limits_on_x)
      lines(generations,rel_mae,col="dodgerblue",type="l",lty=1,lwd=3)
      dev.off ( which=dev.cur() )
      
      save(generations,
           true_demo,
           skylineplot,
           sky_error,
           test_constant_model,
           pGSM_hat,
           pGSM_95HPD,
           ratioNe_hat,
           ratioNe_95HPD,
           file=paste0("results/",project,"/Results/",scenarios[scenario],"_",true_gsm[pGSMvalue],"_results.RData"))
    }  
  }

}else{ # END IF SIMULATED TARGET DATA
  

    
    
  # plot prior skyline
    
  file_name <- paste0("results/",project,"/",project,"_ABCsky_prior.",g_out)
  if (g_out=="png") png(filename=file_name, width=11.7, height=8.3, units = "in", res=300) 
  if (g_out=="svg") svg(filename=file_name, width=11.7, height=8.3)                        
  if (g_out=="pdf") pdf(file    =file_name, width=11.7, height=8.3)

    
  par(cex.axis=2.5,cex.lab=2.5,mar=c(5.5,5.5,2,2))
    
  generations <- seq(from=0, to=Tmax, length.out=num_of_points)
  limits_on_x <- c(0,max(generations))
  label_x     <- "t (mutations/locus)"
  label_y     <- expression("log"[10]*theta)
  
      
      
      
  params2 <- c(t(newparams))
  generations2 <- array(generations,length(params2))
  subset <- sample(1:length(params2),length(params2)*proportion_of_sims_kept)
  params2      <- params2[subset]
  generations2 <- generations2[subset]
  hist2d( generations2,
          log10(params2),
          nbins=num_of_points,
          col=c("white", grey(seq(1,0,-0.05))),
          xlab=label_x,
          ylab=label_y,
          ylim=c(log10(prior_THETA_min),log10(prior_THETA_max)),
          xlim=limits_on_x)
    
  prior_median <- array(NA,length(generations))
  prior_lower <- array(NA,length(generations))
  prior_higher <- array(NA,length(generations))
  for (gen in 1:length(generations)){
    prior_median[gen]  <- median(newparams[,gen])
    prior_lower[gen]   <- quantile(newparams[,gen],0.025)
    prior_higher[gen]  <- quantile(newparams[,gen],0.975)
  }
  lines(generations ,log10(prior_higher),col="black",type="l",lty=2,lwd=2)
  lines(generations ,log10(prior_lower),col="black",type="l",lty=2,lwd=2)
  lines(generations ,log10(prior_median),col="white",type="l",lty=1,lwd=4)

  
  box()
    
  dev.off ( which=dev.cur() )
  
  # load observed stats
  target <- read.table(file = paste0("results/",project,"/target.sumstats"), header = T)
  target <- target[sumstats_header]
  abc_target <- target[sumstats_header]

  # perform abc (calculate posterior)
  abcresult <- abc(target  = abc_target,
                   param   = newparams,
                   sumstat = stats[,sumstats_header],
                   tol     = proportion_of_sims_kept,
                   method  = "loclinear",
                   hcorr = F,
                   #method  = "neuralnet",
                   #subset = NULL,
                   transf  = "log",
                   trace=T)
  adj.values <- abcresult$adj.values 
  abcresult  <- summary(abcresult,print=F)
 
      
  # get scale for skykline plot
  generations <- seq(from=0, to=Tmax, length.out=num_of_points)
  limits_on_x <- c(0,max(generations))
  label_x     <- "t (mutations/locus)"
  label_y     <- expression("log"[10]*theta)
      
  limits_on_y <- c( min(log10(c(abcresult[2,],prior_THETA_min))),  max(log10(c(abcresult[6,],prior_THETA_max))) )
      
      
  # plot skyline
  file_name <- paste0("results/",project,"/",project,"_ABCsky_posterior.",g_out)
  if (g_out=="png") png(filename=file_name, width=11.7, height=8.3, units = "in", res=300)
  if (g_out=="svg") svg(filename=file_name, width=11.7, height=8.3)
  if (g_out=="pdf") pdf(file    =file_name, width=11.7, height=8.3)
            
  par(cex.axis=2.5,cex.lab=2.5,mar=c(5.5,5.5,2,2))
      
  adj.values2<-c(t(adj.values))
  generations2<-array(generations,length(adj.values2))
  hist2d( generations2,
          log10(adj.values2),
          nbins=num_of_points,
          col=c("white", grey(seq(1,0,-0.05))),
          xlab=label_x,
          ylab=label_y,
          ylim=limits_on_y,
          xlim=limits_on_x)
  
  lines(generations ,log10(abcresult[6,]),col="black",type="l",lty=2,lwd=2)
  lines(generations ,log10(abcresult[2,]),col="black",type="l",lty=2,lwd=2)
  #lines(generations ,log10(abcresult[3,]),col="white",type="l",lty=1,lwd=4)
  lines(generations ,log10(abcresult[3,]),col="black",type="l",lty=1,lwd=4)

  legend(x="topright",legend=project,cex=3,bty="n")
  
  box()
    
  dev.off ( which=dev.cur() )

  print("Skyline plot done")
  
  #Sky_txt_file<-paste0("results/",project,"/",project,"_SkylinePlot.txt")
  #write( c("time","median","upper","lower"), file=Sky_txt_file, ncolumns=4)
  #write.table( cbind(generations,abcresult[3,],abcresult[6,],abcresult[2,]), file=Sky_txt_file,  col.names=F, row.names=F, append=T)

  skylineplot <- cbind(generations,median=abcresult[3,],lower_95HPD=abcresult[2,],upper_95HPD=abcresult[6,])
  
  
  
  
  constant_model <- params$PERIODS
  constant_model[constant_model==1]           <- "Constant"
  constant_model[constant_model!="Constant"]  <- "Variable"
  
  #test_constant_model_txt_file   <- paste0("results/",project,"/",project,"_ABCsky_test_constant_model.txt")
  #write( c("Constant","Variable"),
  #       ncolumns = 2,
  #       file = test_constant_model_txt_file)
  
  
  
  
  if (prior_PERIODS=="Poisson"){
    model_choice <- postpr(target  = abc_target,
                           index   = constant_model,
                           sumstat = stats[sumstats_header],
                           tol     = proportion_of_sims_kept,
                           corr    = F,
                           method  = "mnlogistic",
                           trace=F)
    #write.table(t(model_choice$nmodels/sum(model_choice$nmodels)), col.names=F, file=test_constant_model_txt_file, row.names=F, append=T)
    #write.table(t(model_choice$pred), col.names=F, file=test_constant_model_txt_file, row.names=F, append=T)
    
    test_constant_model <- list(prior=model_choice$nmodels/sum(model_choice$nmodels),posterior=model_choice$pred)
  }else{
    test_constant_model <- NULL
  }

  print("Model choice done")

  if (max_num_of_periods==2){
    ratioNe <- params[,"THETA1"]/params[,"THETA0"]
    abcresult <- abc(target  = abc_target,
                     param   = ratioNe,
                     sumstat = stats[,sumstats_header],
                     tol     = proportion_of_sims_kept,
                     method  = "loclinear",
                     hcorr   = T,
                     transf  = "log",
                     trace=T)
    ratioNe_hat   <- summary(abcresult,print=F)[3]  
    ratioNe_95HPD <- c(summary(abcresult,print=F)[2],summary(abcresult,print=F)[6])
    # plot prior and posterior ratioNe
    file_name <- paste0("results/",project,"/",project,"_ratioNe.",g_out)
    if (g_out=="png") png(filename=file_name, width=11.7, height=8.3, units = "in", res=300)
    if (g_out=="svg") svg(filename=file_name, width=11.7, height=8.3)
    if (g_out=="pdf") pdf(file    =file_name, width=11.7, height=8.3)
    
    hist(log(ratioNe),
         #breaks=seq(-1,2,0.05),
         col="grey",
         freq=F,
         ylim=c(0,0.15),
         main="",
         xlab="ratio Ne (N1/N0)",
         ylab="probability density")
    hist(log(abcresult$adj.values),
         #breaks=seq(-1,2,0.05),
         col=rgb(1,0,0,0.5),
         freq=F,
         add=T)
    
    dev.off ( which=dev.cur() )
  }else{
    ratioNe_hat   <- NULL
    ratioNe_95HPD <- NULL
  }
  
  
  
  
  
  if (length(which(dimnames(mut_params)[[2]]=="pmic_1"))==1){
    abcresult <- abc(target  = abc_target,
                     param   = mut_params[,"pmic_1"],
                     sumstat = stats[,sumstats_header],
                     tol     = proportion_of_sims_kept,
                     method  = "loclinear",
                     hcorr = T,
                     transf  = "logit",
                     logit.bounds = matrix(c(prior_GSM_min,prior_GSM_max),ncol=2),
                     trace=T)
    
    pGSM_hat   <- summary(abcresult,print=F)[3]  
    pGSM_95HPD <- c(summary(abcresult,print=F)[2],summary(abcresult,print=F)[6])
    # plot prior and posterior pGSM
    file_name <- paste0("results/",project,"/",project,"_pGSM.",g_out)
    if (g_out=="png") png(filename=file_name, width=11.7, height=8.3, units = "in", res=300)
    if (g_out=="svg") svg(filename=file_name, width=11.7, height=8.3)
    if (g_out=="pdf") pdf(file    =file_name, width=11.7, height=8.3)
    
    hist(mut_params[,"pmic_1"],
         #breaks=seq(-1,2,0.05),
         col="grey",
         freq=F,
         ylim=c(0,6),
         main="",
         xlab="pGSM",
         ylab="probability density")
    hist(abcresult$adj.values,
         #breaks=seq(-1,2,0.05),
         col=rgb(1,0,0,0.5),
         freq=F,
         add=T)
    
    dev.off ( which=dev.cur() )
    
  }else{
    pGSM_hat   <- NULL
    pGSM_95HPD <- NULL
  }
  
  if (length(which(dimnames(mut_params)[[2]]=="snimic_1"))==1){
    abcresult <- abc(target  = abc_target,
                     param   = mut_params[,"snimic_1"],
                     sumstat = stats[,sumstats_header],
                     tol     = proportion_of_sims_kept,
                     method  = "loclinear",
                     hcorr = T,
                     transf  = "logit",
                     logit.bounds = matrix(c(prior_SNI_min,prior_SNI_max),ncol=2),
                     trace=T)
    
    rateSNI_hat   <- summary(abcresult,print=F)[3]  
    rateSNI_95HPD <- c(summary(abcresult,print=F)[2],summary(abcresult,print=F)[6])
    # plot prior and posterior rateSNI
    file_name <- paste0("results/",project,"/",project,"_rateSNI.",g_out)
    if (g_out=="png") png(filename=file_name, width=11.7, height=8.3, units = "in", res=300)
    if (g_out=="svg") svg(filename=file_name, width=11.7, height=8.3)
    if (g_out=="pdf") pdf(file    =file_name, width=11.7, height=8.3)
    
    hist(-log(mut_params[,"snimic_1"]),
         #breaks=seq(-1,2,0.05),
         col="grey",
         freq=F,
         ylim=c(0,6),
         main="",
         xlab="-log(SNI mutation rate)",
         ylab="probability density")
    hist(-log(abcresult$adj.values),
         #breaks=seq(-1,2,0.05),
         col=rgb(1,0,0,0.5),
         freq=F,
         add=T)
    
    dev.off ( which=dev.cur() )
  }else{
    rateSNI_hat   <- NULL
    rateSNI_95HPD <- NULL
  }
  
  save(skylineplot,
       test_constant_model,
       pGSM_hat,
       pGSM_95HPD,
       ratioNe_hat,
       ratioNe_95HPD,
       rateSNI_hat,
       rateSNI_95HPD,
       file=paste0("results/",project,"/",project,"_results.RData"))
}
    






rm(params)

rm(abc_target,
   newparams,
   params2,
   stats,
   target,
   generations2,
   adj.values2,
   lower_than_TAU1)

# save all results from step 3
save.image( file=paste0("results/",project,"/",project,"_step3.RData") ) 

