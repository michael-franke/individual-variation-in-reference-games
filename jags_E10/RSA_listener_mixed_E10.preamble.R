source('Monster-data.R')
source('Monster-games.R')
source('iqr-helpers.r')
source('rsa.r')
source('helpers/helpers.R')

lMax = 25
eMax = 0.25

salience_E10 = prop.table(salience_E10,1)

experiment = 5
conditions = c(1,2,3,4)

## flat priors for R1 ## great if lambda is fixed over types
salienceG1R1 = rep(1/3,3)
salienceG2R1 = rep(1/3,3)
salienceG3R1 = rep(1/3,3)
salienceG4R1 = rep(1/3,3)
## estimated salience priors for R1
# salienceG1R1 = salience_E10[1,]
# salienceG2R1 = salience_E10[2,]
# salienceG3R1 = salience_E10[3,]
# salienceG4R1 = salience_E10[4,]
## estimated salience priors for R2
salienceG1R2 = salience_E10[1,]
salienceG2R2 = salience_E10[2,]
salienceG3R2 = salience_E10[3,]
salienceG4R2 = salience_E10[4,]
# ## estimated salience priors for R2 ## worse than using salience priors!
# salienceG1R2 = rep(1/3,3)
# salienceG2R2 = rep(1/3,3)
# salienceG3R2 = rep(1/3,3)
# salienceG4R2 = rep(1/3,3)
# R0's utils
uR0G1 = get.R0.utils(simple,rep(1/3,3))
uR0G2 = get.R0.utils(complex,rep(1/3,3))
uR0G3 = get.R0.utils(unamb,rep(1/3,3))
uR0G4 = get.R0.utils(amb,rep(1/3,3))
# R1's utils
uR1G1 = get.R1.utils(simple,salienceG1R1)
uR1G2 = get.R1.utils(complex,salienceG2R1)
uR1G3 = get.R1.utils(unamb,salienceG3R1)
uR1G4 = get.R1.utils(amb,salienceG4R1)
# Speaker utils
uS1G1 = log(get.speaker.utils(simple,rep(1/3,3)))
uS1G2 = log(get.speaker.utils(complex,rep(1/3,3)))
uS1G3 = log(get.speaker.utils(unamb,rep(1/3,3)))
uS1G4 = log(get.speaker.utils(amb,rep(1/3,3)))
# # Try without the log
# uS1G1 = (get.speaker.utils(simple,rep(1/3,3)))
# uS1G2 = (get.speaker.utils(complex,rep(1/3,3)))
# uS1G3 = (get.speaker.utils(unamb,rep(1/3,3)))
# uS1G4 = (get.speaker.utils(amb,rep(1/3,3)))

obsBS = obsBSR.L[[experiment]]
obsR = obsR.L[[experiment]]
obsS = obsS.L[[experiment]]
succ = succ.L[[experiment]]

# number of participants
nS = dim(obsBS)[2]
# number of trials
nG1 = sum(obsR[1,])
nG2 = sum(obsR[2,])
nG3 = sum(obsR[3,])
nG4 = sum(obsR[4,])
n = c(nG1, nG2, nG3, nG4)

# number of trials per participant
nT = sapply(1:4, function(x) sum(obsR[x,])/nS)