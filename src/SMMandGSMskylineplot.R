require(abc)

load("~/Work/Research/ABC_Skyline_plot/DIYABCskylineplot/results/BWC_CNP/BWC_CNP.sumstats.RData")
statsSMM <- stats
load("~/Work/Research/ABC_Skyline_plot/DIYABCskylineplot/results/BWC_CNP_GSM/BWC_CNP_GSM.sumstats.RData")
statsGSM <- stats
remove(stats)
stats <- rbind(statsSMM,statsGSM)
remove(statsSMM,statsGSM)
gc()

load("~/Work/Research/ABC_Skyline_plot/DIYABCskylineplot/results/BWC_CNP/BWC_CNP_newparams.RData")
newparamsSMM <- newparams
load("~/Work/Research/ABC_Skyline_plot/DIYABCskylineplot/results/BWC_CNP_GSM/BWC_CNP_GSM_newparams.RData")
newparamsGSM <- newparams
remove(newparams)
newparams <- rbind(newparamsSMM,newparamsGSM)
remove(newparamsSMM,newparamsGSM)
gc()

abc_target <- read.table(file = "~/Work/Research/ABC_Skyline_plot/DIYABCskylineplot/results/BWC_CNP_GSM/target.sumstats", header = T)

abcresult <- NA
abcresult <- abc(target  = abc_target,
                 param   = newparams,
                 sumstat = stats,
                 tol     = 0.0005,
                 method  = "loclinear",
                 hcorr = F,
                 transf  = "log",
                 trace=T)
abcresult  <- summary(abcresult,print=F)
remove(newparams)
gc()

# get scale for skykline plot
generations <- 10^seq(from=log10(0.00025),to=log10(4),length.out=50)
limits_on_x <- c(min(generations),max(generations))
label_x     <- "t (mutations/locus)"
label_y     <- expression(theta)

limits_on_y <- c( min(c(abcresult[2,],1E-3)),  max(c(abcresult[6,],1E3)) )

# plot skyline
file_name <- "results/BWColobusABCskySSMandGSM.pdf"
pdf(file    =file_name, width=11.7, height=8.3)

par(cex.axis=2.5,cex.lab=2.5,mar=c(5.5,5.5,2,2))

plot(generations ,
     abcresult[6,],
     col="black",
     type="l",
     lty=2,
     lwd=2,
     log="xy",
     ylim=limits_on_y,
     xlim=limits_on_x)
lines(generations ,abcresult[2,],col="black",type="l",lty=2,lwd=2)
#lines(generations ,abcresult[3,],col="white",type="l",lty=1,lwd=4)
lines(generations ,abcresult[3,],col="black",type="l",lty=1,lwd=4)

legend(x="topright",legend="black-and-white colobus",cex=3,bty="n")

box()

dev.off ( which=dev.cur() )
skylineplot <- cbind(generations,median=abcresult[3,],lower_95HPD=abcresult[2,],upper_95HPD=abcresult[6,])


load("~/Work/Research/ABC_Skyline_plot/DIYABCskylineplot/results/BWC_CNP/BWC_CNP.params.RData")
paramsSMM <- params
load("~/Work/Research/ABC_Skyline_plot/DIYABCskylineplot/results/BWC_CNP_GSM/BWC_CNP_GSM.params.RData")
paramsGSM <- params
remove(params)
params <- rbind(paramsSMM,paramsGSM)
remove(paramsSMM,paramsGSM)
gc()

constant_model <- params$PERIODS
constant_model[constant_model==1]           <- "Constant"
constant_model[constant_model!="Constant"]  <- "Variable"
remove(params)
gc()

model_choice <- NA
model_choice <- postpr(target  = abc_target,
                       index   = constant_model,
                       sumstat = stats,
                       tol     = 0.0005,
                       corr    = F,
                       method  = "mnlogistic",
                       trace=F)
test_constant_model_posterior <- model_choice$pred
test_constant_model <- list(prior=model_choice$nmodels/sum(model_choice$nmodels),posterior=test_constant_model_posterior)
BF <- test_constant_model_posterior[2]/test_constant_model_posterior[1]

save(file="~/Work/Research/ABC_Skyline_plot/DIYABCskylineplot/results/BWC_SSMandGSM_results.RData",skylineplot,BF)











