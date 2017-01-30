require(abc)

load("~/Work/Research/ABC_Skyline_plot/DIYABCskylineplot/results/BWC_CNP/BWC_CNP.sumstats.RData")
statsSMM <- stats
load("~/Work/Research/ABC_Skyline_plot/DIYABCskylineplot/results/BWC_CNP_GSM/BWC_CNP_GSM.sumstats.RData")
statsGSM <- stats
load("~/Work/Research/ABC_Skyline_plot/DIYABCskylineplot/results/BWC_CNP_GSM/BWC_CNP_GSM.mutparams.RData")
mut_params <- c(array(0, dim=nrow(statsSMM)),mut_params)
mutation_model <- c(array("SMM", dim=nrow(statsSMM)),array("GSM", dim=nrow(statsGSM)))
stats <- rbind(statsSMM,statsGSM)
abc_target <- read.table(file = "~/Work/Research/ABC_Skyline_plot/DIYABCskylineplot/results/BWC_CNP_GSM/target.sumstats", header = T)



model_choice <- NA
model_choice <- postpr(target  = abc_target,
                       index   = mutation_model,
                       sumstat = stats,
                       tol     = 0.0005,
                       corr    = T,
                       method  = "mnlogistic",
                       trace=F)

summary(model_choice)

abcresult <- NA
abcresult <- abc(target  = abc_target,
                 param   = mut_params,
                 sumstat = stats,
                 tol     = 0.0005,
                 method  = "loclinear",
                 hcorr = T,
                 transf  = "logit",
                 logit.bounds = matrix(c(0,1),ncol=2),
                 trace=T)
prior      <- mut_params
posterior  <- abcresult$adj.values
hist(posterior)
pGSM_hat   <- summary(abcresult,print=F)[3]  
pGSM_95HPD <- c(summary(abcresult,print=F)[2],summary(abcresult,print=F)[6])
save(file = "~/Work/Research/ABC_Skyline_plot/DIYABCskylineplot/results/BWC_PGSM_results.RData",
     model_choice,
     prior,
     posterior,
     pGSM_hat,
     pGSM_95HPD)








load("~/Work/Research/ABC_Skyline_plot/DIYABCskylineplot/results/RC_CNP/RC_CNP.sumstats.RData")
statsSMM <- stats
load("~/Work/Research/ABC_Skyline_plot/DIYABCskylineplot/results/RC_CNP_GSM/RC_CNP_GSM.sumstats.RData")
statsGSM <- stats
remove(stats)
load("~/Work/Research/ABC_Skyline_plot/DIYABCskylineplot/results/RC_CNP_GSM/RC_CNP_GSM.mutparams.RData")
mut_params <- c(array(0, dim=nrow(statsSMM)),mut_params)
mutation_model <- c(array("SMM", dim=nrow(statsSMM)),array("GSM", dim=nrow(statsGSM)))
stats <- rbind(statsSMM,statsGSM)
abc_target <- read.table(file = "~/Work/Research/ABC_Skyline_plot/DIYABCskylineplot/results/RC_CNP_GSM/target.sumstats", header = T)

model_choice <- NA
model_choice <- postpr(target  = abc_target,
                       index   = mutation_model,
                       sumstat = stats,
                       tol     = 0.005,
                       corr    = T,
                       method  = "mnlogistic",
                       trace=F)

summary(model_choice)

abcresult <- NA
abcresult <- abc(target  = abc_target,
                 param   = mut_params,
                 sumstat = stats,
                 tol     = 0.0005,
                 method  = "loclinear",
                 hcorr = T,
                 transf  = "logit",
                 logit.bounds = matrix(c(0,1),ncol=2),
                 trace=T)
prior      <- mut_params
posterior  <- abcresult$adj.values
pGSM_hat   <- summary(abcresult,print=F)[3]  
pGSM_95HPD <- c(summary(abcresult,print=F)[2],summary(abcresult,print=F)[6])
save(file = "~/Work/Research/ABC_Skyline_plot/DIYABCskylineplot/results/RC_PGSM_results.RData",
     model_choice,
     prior,
     posterior,
     pGSM_hat,
     pGSM_95HPD)
