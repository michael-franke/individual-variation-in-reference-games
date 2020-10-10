library(reshape2)
library(ggplot2)
require('gtools') # for dirichlet distribution
source('~/Desktop/data/svn/programming/R/helpers/helpers.R')

speaker = TRUE
useOldSamples = TRUE

if (speaker) {
  source('jags_E10/RSA_speaker_mixed_E10.preamble.R')
  if (useOldSamples) {
    load("jags_E10/samplesSpeaker_RSAMixE10.Rdata")
  } else{
    source('jags_E10/RSA_speaker_mixed_E10.R')  
  } 
  start.values = c(6, 39,  2)
} else{
  source('jags_E10/RSA_listener_mixed_E10.preamble.R')
  if (useOldSamples) {
    load("jags_E10/samplesListener_RSAMixE10.Rdata")
  } else{
    source('jags_E10/RSA_listener_mixed_E10.R')  
  }  
  start.values = c(13, 15,  7)
}

# samples for estimation
typeprior <- samples$BUGSoutput$sims.list$typeprior
typeMeans = sapply(1:3, function(i) mean(typeprior[,i]))

negLogLike = function(par) {
  # par is triple of dirichlet weights
  return(-sum(log(ddirichlet(typeprior, par))))
}

o = optim(par = start.values, fn = negLogLike)
alpha = o$par
o.negLogLike = o$value

## validate method 
nIteration = 1000
nSamples = 10000
dirchSamples = sapply(1:nIteration, function(i) -sum((log(ddirichlet(rdirichlet(nSamples,alpha),alpha)))))
hdi = HDIofMCMC(dirchSamples)
show(hdi[1] <= o.negLogLike & hdi[2] >= o.negLogLike)
show(1/((hdi[2] - hdi[1]) / (o.negLogLike - hdi[1])))

## estimate Bayes factor for "approximation of RSA"
e = 0.05
show((ddirichlet(c(e/2, 1-e, e/2), alpha) / 2))
