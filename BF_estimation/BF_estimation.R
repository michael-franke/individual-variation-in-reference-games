library(reshape2)
library(ggplot2)
library(np) # for npudens
library(polspline) # for logspline

source('jags_E10/RSA_speaker_mixed_E10.preamble.R')

useOldSamples = TRUE

if (useOldSamples) {
  load("jags_E10/samplesSpeaker_RSAMixE10.Rdata")
} else{
  source('jags_E10/RSA_speaker_mixed_E10.R')  
}

typeprior <- samples$BUGSoutput$sims.list$typeprior

## calculate Bayes Factor in favor of RSA-typeprior c(0,1,0), given data
## prior for c(0,1,0) under Dirichlet with concentration parameters c(1,1,1) is 2
## posterior can be estimated with a number of methods:
f = npudens(~typeprior, data=data.frame(typeprior = typeprior[,2]))
NPudensEst = predict(f, newdata=data.frame(typeprior=1))
estimates = data.frame(method = c("density", 
                                  "npudens",
                                  "logspline"),
                       enumerator = c(density(typeprior[,2], from=0, to = 1, n = 1000001)$y[1000001],
                                      NPudensEst,
                                      dlogspline(1, logspline(typeprior[,2], lbound = 0, ubound = 1))))
estimates$BFs = estimates$enumerator/2
estimates$BFsInv = 1/estimates$BFs

show(estimates)

g = npudens(~S0 + S1 + S2, data=data.frame(S0 = typeprior[,1], S1 = typeprior[,2], S2 = typeprior[,3]))
