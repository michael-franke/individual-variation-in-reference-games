library(R2jags)

source('jags_E10/RSA_listener_mixed_E10.preamble.R')

# model = "jags_E10/RSA_listener_mixed_E10.R0trembles.jags.R"
model = "jags_E10/RSA_listener_mixed_E10.jags.R"

data <- list("salienceG1R2", "salienceG2R2", "salienceG3R2", "salienceG4R2",
             "nS", "nT",
             "uS1G1", "uS1G2", "uS1G3", "uS1G4", 
             "uR0G1", "uR0G2", "uR0G3", "uR0G4", 
             "uR1G1", "uR1G2", "uR1G3", "uR1G4", 
             "obsBS", "obsR") # to be passed on to JAGS

parameters <- c("lambda0","epsilon"
                , "ppG1"
                 , "ppG2"
                 , "ppG3"
                 , "ppG4"
                 , "typeprior" 
                 ,"p"
                )

samples <- jags(data, parameters = parameters,
                model.file = model, n.chains=2, n.iter=20000, 
                n.burnin=10000, n.thin=2, DIC=T)

# check convergence results
## NB: high R.hat for categorical p can happen if exactly one chain jumps exactly once,
## in a case of otherwise close-certainty about a subject's type
# show(samples$BUGSoutput$summary[samples$BUGSoutput$summary[,"Rhat"] >= 1.1, ])

# save(samples, file = "jags_E10/samplesListener_RSAMixE10.Rdata")
save(samples, file = "jags_E10/samplesListener_RSAMixE10-full.Rdata")