# Script to summarize the results from the test of the ABC Skyline Plot
# approach on simulated data sets:  BAYES FACTOR

# Miguel Navascués

pgsm_values <- c(0.00,0.22,0.74)
options(scipen = 999)
project <- "Poisson"
number_of_replicates <- 100
scenarios_number <- 1:27
scenarios <- paste("Scenario", scenarios_number, sep="")

scen_table <- read.table("src/Scenari/scenari.table.txt",header=T,row.names=1)
mkdir_command <- paste0("mkdir results/",project,"/Results/BayesFactor")
system( mkdir_command )

# Calculate Bayes Factor

for (pgsm in seq_along(pgsm_values)){
  
  bayes_factor <- matrix(NA,nrow=number_of_replicates,ncol=length(scenarios_number))
  for (scen in seq_along(scenarios_number)){
    load(paste0("results/",project,"/Results/",scenarios[scen],"_",pgsm_values[pgsm],"_results.RData"))
    bayes_factor[,scen] <- test_constant_model$posterior[,2]/test_constant_model$posterior[,1]
    
    bayes_factor[which(is.infinite(bayes_factor[,scen])),scen] <- NA
  }
  save(bayes_factor,file=paste0("results/",project,"/Results/BayesFactor/BayesFactor_",pgsm_values[pgsm],".RData"))
  
}

# MAIN FIGURE

load(paste0("results/",project,"/Results/BayesFactor/BayesFactor_0.22.RData"))

pdf(file=paste0("results/",project,"/Results/BayesFactor/BayesFactor_main.pdf"),width=5,height=4)
plot.new()
par(mar=c(3,4,2,1))
plot.window(xlim=c(0,5), xaxs="i",
            ylim=c(0.1,1000), yaxs="i", log="y")
xx <- c(0,0,5,5)
yy <- c(0.3,1,1,0.3)
polygon(xx, yy, col="gray100",border = NA)
yy <- c(1,3,3,1)
polygon(xx, yy, col="gray95",border = NA)
yy <- c(3,10,10,3)
polygon(xx, yy, col="gray90",border = NA)
yy <- c(10,30,30,10)
polygon(xx, yy, col="gray85",border = NA)
yy <- c(30,100,100,30)
polygon(xx, yy, col="gray80",border = NA)
yy <- c(100,10000000,10000000,100)
polygon(xx, yy, col="gray75",border = NA)
text(4,0.5477226,"negative",cex=0.8)
text(4,1.732051,"weak",cex=0.8)
text(4,5.477226,"substantial",cex=0.8)
text(4,17.32051,"strong",cex=0.8)
text(4,54.77226,"very strong",cex=0.8)
text(4,173.2051,"decisive",cex=0.8)
main_title <- expression()
boxplot(bayes_factor[,c(8,20,26)], add=T, names=NA, ylab="Bayes factor", col="white",pch=".")
axis(1, at=1:3 , labels= c("contraction","","constant") , las=1, tick=F)
axis(1, at=1:3 , labels= c("","expansion","") , line=1, las=1, tick=F)
dev.off()










for (pgsm in seq_along(pgsm_values)){

  load(paste0("results/",project,"/Results/BayesFactor/BayesFactor_",pgsm_values[pgsm],".RData"))
  
  pdf(file=paste0("results/",project,"/Results/BayesFactor/BayesFactorContractions_",pgsm_values[pgsm],".pdf"),width=12,height=8)
  plot.new()
  plot.window(xlim=c(0,14), xaxs="i",
              ylim=c(0.1,1000), yaxs="i", log="y")
  xx <- c(0,0,14,14)
  yy <- c(0.3,1,1,0.3)
  polygon(xx, yy, col="gray100",border = NA)
  yy <- c(1,3,3,1)
  polygon(xx, yy, col="gray95",border = NA)
  yy <- c(3,10,10,3)
  polygon(xx, yy, col="gray90",border = NA)
  yy <- c(10,30,30,10)
  polygon(xx, yy, col="gray85",border = NA)
  yy <- c(30,100,100,30)
  polygon(xx, yy, col="gray80",border = NA)
  yy <- c(100,10000000,10000000,100)
  polygon(xx, yy, col="gray75",border = NA)
  text(13,0.5477226,"negative",cex=1)
  text(13,1.732051,"weak",cex=1)
  text(13,5.477226,"substantial",cex=1)
  text(13,17.32051,"strong",cex=1)
  text(13,54.77226,"very strong",cex=1)
  text(13,173.2051,"decisive",cex=1)
  main_title <- substitute(paste("Contractions, with ",theta[0],"=0.4, ",P[GSM], "=",v),list(v=pgsm_values[pgsm]))
  boxplot(bayes_factor[,1:12], add=T, names=NA, ylab="Bayes factor", col="white", pch=".")
  title(main=main_title)
  axis(1, at=1:12 , labels= c(scen_table[1:12,"N1"])*1e-3*4 , las=1, tick=F, cex.axis=1)
  axis(1, at=1:12 , labels= c(scen_table[1:12,"Ta"])*1e-3 , line=1, las=1, tick=F, cex.axis=1)
  mtext(expression(theta[1]), side=1, line=1, font=2, cex=1,at=0)
  mtext(expression(tau), side=1, line=2, font=2, cex=1,at=0)
  dev.off()
  
  

  
  pdf(file=paste0("results/",project,"/Results/BayesFactor/BayesFactorExpansions_",pgsm_values[pgsm],".pdf"),width=12,height=8)
  plot.new()
  plot.window(xlim=c(0,14), xaxs="i",
              ylim=c(0.1,1000), yaxs="i", log="y")
  xx <- c(0,0,14,14)
  yy <- c(0.3,1,1,0.3)
  polygon(xx, yy, col="gray100",border = NA)
  yy <- c(1,3,3,1)
  polygon(xx, yy, col="gray95",border = NA)
  yy <- c(3,10,10,3)
  polygon(xx, yy, col="gray90",border = NA)
  yy <- c(10,30,30,10)
  polygon(xx, yy, col="gray85",border = NA)
  yy <- c(30,100,100,30)
  polygon(xx, yy, col="gray80",border = NA)
  yy <- c(100,1000000,1000000,100)
  polygon(xx, yy, col="gray75",border = NA)
  text(13,0.5477226,"negative",cex=1)
  text(13,1.732051,"weak",cex=1)
  text(13,5.477226,"substantial",cex=1)
  text(13,17.32051,"strong",cex=1)
  text(13,54.77226,"very strong",cex=1)
  text(13,173.2051,"decisive",cex=1)
  main_title <- substitute(paste("Expansions, with ",theta[1],"=0.4, ",P[GSM], "=",v),list(v=pgsm_values[pgsm]))
  boxplot(bayes_factor[,13:24], add=T, names=NA, ylab="Bayes factor", col="white", pch=".")
  title(main=main_title)
  axis(1, at=1:12 , labels= c(scen_table[13:24,"N0"])*4*1e-3 , las=1, tick=F, cex.axis=1)
  axis(1, at=1:12 , labels= c(scen_table[1:12,"Ta"])*1e-3 , line=1, las=1, tick=F, cex.axis=1)
  mtext(expression(theta[0]), side=1, line=1, font=2, cex=1,at=0)
  mtext(expression(tau), side=1, line=2, font=2, cex=1,at=0)
  
  if(pgsm==2){
    arrows(12,300,12,1000,lwd=3,length=0.12)
  }
  if(pgsm==3){
    arrows(8,300,8,1000,lwd=3,length=0.12)
    arrows(9,300,9,1000,lwd=3,length=0.12)
    arrows(11,300,11,1000,lwd=3,length=0.12)
    arrows(12,300,12,1000,lwd=3,length=0.12)
  }
  
  
  dev.off()
  

  pdf(file=paste0("results/",project,"/Results/BayesFactor/BayesFactorConstantSize_",pgsm_values[pgsm],".pdf"),width=5,height=8)
  plot.new()
  plot.window(xlim=c(0,5), xaxs="i",
              ylim=c(0.1,1000), yaxs="i", log="y")
  xx <- c(0,0,5,5)
  yy <- c(0.3,1,1,0.3)
  polygon(xx, yy, col="gray100",border = NA)
  yy <- c(1,3,3,1)
  polygon(xx, yy, col="gray95",border = NA)
  yy <- c(3,10,10,3)
  polygon(xx, yy, col="gray90",border = NA)
  yy <- c(10,30,30,10)
  polygon(xx, yy, col="gray85",border = NA)
  yy <- c(30,100,100,30)
  polygon(xx, yy, col="gray80",border = NA)
  yy <- c(100,10000000,10000000,100)
  polygon(xx, yy, col="gray75",border = NA)
  text(4,0.5477226,"negative",cex=0.8)
  text(4,1.732051,"weak",cex=0.8)
  text(4,5.477226,"substantial",cex=0.8)
  text(4,17.32051,"strong",cex=0.8)
  text(4,54.77226,"very strong",cex=0.8)
  text(4,173.2051,"decisive",cex=0.8)
  main_title <- substitute(paste("Constant size, with ",P[GSM], "=",v),list(v=pgsm_values[pgsm]))
  boxplot(bayes_factor[,25:27], add=T, names=NA, ylab="Bayes factor", col="white", pch=".")
  title(main=main_title)
  axis(1, at=1:3 , labels= c(scen_table[25:27,"N0"])*4*1e-3 , las=1, tick=F)
  mtext(expression(theta), side=1, line=1, font=2, cex=1,at=0)
  dev.off()
  
  
  
}



