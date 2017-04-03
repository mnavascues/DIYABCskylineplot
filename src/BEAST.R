# BEAST
library(coda)

project <- "Scenario26"

beastlog <- read.table(paste0("results/BEAST/",project,"/",project,"_sky.log"), header=T, skip=0)
#names(beastlog)
beastlog <- as.mcmc(beastlog)

pdf(file = paste0("results/BEAST/",project,"/",project,".pdf"), paper="a4", width=0, height=0)
plot(beastlog)
dev.off()

#summary(beastlog)

sum(beastlog[,"demographic.populationSizeChanges"]==0)/length(beastlog[,"demographic.populationSizeChanges"])


names(beastlog)
