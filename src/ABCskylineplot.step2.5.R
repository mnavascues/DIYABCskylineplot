################################################################################
#
# SCRIPT FOR ABC SKYLINE PLOT: STEP 2.5. PCA ON SUMMARY STATISTICS
#
################################################################################

# load statistics for reference table
load( file = paste("results/",project,"/",project,".sumstats.RData",sep="") )

#sumstats_header <- sumstats_header[1:5]

PCA_sumstats <- c(1,2,3,4,7)

PCA_stats  <- princomp(stats[,PCA_sumstats])


# load observed stats
if (simulated_target_data){
  mkdir_command <- paste0("mkdir results/",project,"/Results")
  if(.Platform$OS.type == "unix") system( mkdir_command )
  mkdir_command <- paste0("mkdir results/",project,"/Results/PCA")
  if(.Platform$OS.type == "unix") system( mkdir_command )

  for (pGSMvalue in seq_along(true_gsm)){
    
    pGSMfolder <- paste0("P", true_gsm[pGSMvalue])
    for (scenario in seq_along(scenarios_number)) {
      target <- read.table(file = paste0("results/",project,"/Simulations/",pGSMfolder,"/",scenarios[scenario],"/",scenarios[scenario],".sumstats"), header = T)
      main_title <- paste("PCA on summary statistics for scenario",scenario)
      
      PCA_target <- predict(PCA_stats, target[PCA_sumstats])
      
      if (maxPCA>length(PCA_sumstats)) maxPCA <- length(PCA_sumstats)
      
      if (g_out=="pdf"){
        file_name <- paste0("results/",project,"/Results/PCA/",scenarios[scenario],"_",true_gsm[pGSMvalue],"_PCA.pdf")
        pdf(file=file_name, width=11.7, height=8.3)
      }
      
      for (pci in 1:(maxPCA-1)){
        for (pcj in (pci+1):maxPCA){
          if (!g_out=="pdf"){
            file_name <- paste0("results/",project,"/Results/PCA/",scenarios[scenario],"_",true_gsm[pGSMvalue],"_PCA_",pci,"_",pcj,".",g_out)
          }
          if      (g_out=="png") png(filename=file_name, width=11.7, height=8.3, units = "in", res=300)
          else if (g_out=="svg") svg(filename=file_name, width=11.7, height=8.3)
          
          hbin<-hexbin(PCA_stats$scores[,pci], PCA_stats$scores[,pcj],xbins=100,xlab=paste("PC",pci),ylab=paste("PC",pcj))
          pp<-plot(hbin,legend=FALSE, main=main_title)
          pushHexport(pp$plot.vp)
          
          grid::grid.points(PCA_target[,pci],PCA_target[,pcj],pch=".",gp=gpar(cex=2,col="red"))
          popViewport()
          if (!g_out=="pdf") dev.off ( which=dev.cur() )
        }
      }
      if (g_out=="pdf") dev.off ( which=dev.cur() )  
    }
    
  }  
}else{
  target <- read.table(file = paste0("results/",project,"/target.sumstats"), header = T)
  main_title <- paste("PCA on summary statistics for project",project)

  PCA_target <- predict(PCA_stats, target[PCA_sumstats])
  
  if (maxPCA>length(PCA_sumstats)) maxPCA <- length(PCA_sumstats)
  
  if (g_out=="pdf"){
    file_name <- paste0("results/",project,"/",project,"_PCA.pdf")
    pdf(file=file_name, width=11.7, height=8.3)
  }
  
  for (pci in 1:(maxPCA-1)){
    for (pcj in (pci+1):maxPCA){
      if     (!g_out=="pdf") file_name <- paste0("results/",project,"/",project,"_PCA_",pci,"_",pcj,".",g_out)
      if      (g_out=="png") png(filename=file_name, width=11.7, height=8.3, units = "in", res=300)
      else if (g_out=="svg") svg(filename=file_name, width=11.7, height=8.3)
  
      hbin<-hexbin(PCA_stats$scores[,pci], PCA_stats$scores[,pcj],xbins=100,xlab=paste("PC",pci),ylab=paste("PC",pcj))
      pp<-plot(hbin,legend=FALSE, main=main_title)
      pushHexport(pp$plot.vp)
  
      grid::grid.points(PCA_target[pci],PCA_target[pcj],pch="*",gp=gpar(cex=2,col="red"))
      if (!g_out=="pdf") dev.off ( which=dev.cur() )
    }
  }
  if (g_out=="pdf") dev.off ( which=dev.cur() )
}
# remove some unnecessary variables
rm(PCA_target,PCA_stats,stats,target)

# save all results from step 2.5
save.image( file=paste0("results/",project,"/",project,"_step2.5.RData") ) 

