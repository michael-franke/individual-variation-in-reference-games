library(R2jags)

source('jags_E10/RSA_speaker_mixed_E10.preamble.R')

# model = "jags_E10/RSA_speaker_mixed_E10.S0trembles.jags.R"
model = "jags_E10/RSA_speaker_mixed_E10.jags.R"

# JAGS

data <- list(
             "nS", "nT",
             "uS0G1", "uS0G2", "uS0G3", "uS0G4", 
             "uS1G1", "uS1G2", "uS1G3", "uS1G4", 
             "uS2G1", "uS2G2", "uS2G3", "uS2G4", 
             "obsBS") # to be passed on to JAGS

parameters <- c("lambda0", "epsilon"
                , "ppG1", "ppG2", "ppG3", "ppG4", "typeprior",
                "p"
)

samples <- jags(data,  parameters = parameters,
                model.file = model, n.chains=2, n.iter=20000, 
                n.burnin=10000, n.thin=2, DIC=T)

# check convergence results
  ## NB: high R.hat for categorical p can happen if exactly one chain jumps exactly once,
  ## in a case of otherwise close-certainty about a subject's type
# show(samples$BUGSoutput$summary[samples$BUGSoutput$summary[,"Rhat"] >= 1.1, ])

# save(samples, file = "jags_E10/samplesSpeaker_RSAMixE10.Rdata")
save(samples, file = "jags_E10/samplesSpeaker_RSAMixE10-full.Rdata")





