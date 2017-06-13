# BEAST
library(coda)

project <- "RC"

beastlog <- read.table(paste0("results/BEAST/",project,"/",project,"_sky.log"), header=T, skip=0)
#names(beastlog)
beastlog <- as.mcmc(beastlog)

pdf(file = paste0("results/BEAST/",project,"/",project,".pdf"), paper="a4", width=0, height=0)
plot(beastlog)
dev.off()

#summary(beastlog)

sum(beastlog[1000:5401,"demographic.populationSizeChanges"]==0)/length(beastlog[1000:5401,"demographic.populationSizeChanges"])


names(beastlog)
