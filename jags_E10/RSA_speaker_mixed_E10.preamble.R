source('Monster-data.R')
source('Monster-games.R')
source('iqr-helpers.r')
source('rsa.r')
source('helpers/helpers.R')

lMax = 20
eMax = 0.25

# select what to test
experiment = 5
conditions = 1:4

obsBS = obsBSS.L[[experiment]]
obsR = obsR.L[[experiment]]
obsS = obsS.L[[experiment]]
succ = succS.L[[experiment]]

# number of participants
nS = dim(obsBS)[2]
# number of trials
nG1 = sum(obsS[1,])
nG2 = sum(obsS[2,])
nG3 = sum(obsS[3,])
nG4 = sum(obsS[4,])
n = c(nG1, nG2, nG3, nG4)

# number of trials per participant
nT = sapply(1:4, function(x) sum(obsS[x,])/nS)

nconditions = length(conditions)
k = obsS.L[[experiment]][conditions,]
n = sapply(conditions, function(x) sum(obsS.L[[experiment]][x,]) )
costVec = c(0,0,0,0)
succ = succS.L[[experiment]]

# utils S0
uS0G1 = get.S0.utils(simple)
uS0G2 = get.S0.utils(complex)
uS0G3 = get.S0.utils(unamb)
uS0G4 = get.S0.utils(amb)

# utils S1
uS1G1 = log(get.speaker.utils(simple,rep(1/3,3)))
uS1G2 = log(get.speaker.utils(complex,rep(1/3,3)))
uS1G3 = log(get.speaker.utils(unamb,rep(1/3,3)))
uS1G4 = log(get.speaker.utils(amb,rep(1/3,3)))

# utils S2
## flat priors for R1 
salienceG1R1 = rep(1/3,3)
salienceG2R1 = rep(1/3,3)
salienceG3R1 = rep(1/3,3)
salienceG4R1 = rep(1/3,3)
## estimated salience priors for R1
# salienceG1R1 = salience_E10[1,]
# salienceG2R1 = salience_E10[2,]
# salienceG3R1 = salience_E10[3,]
# salienceG4R1 = salience_E10[4,]
# R1 (lambda -> infty)
R1G1 = get.br(get.R1.utils(simple,salienceG1R1))
R1G2 = get.br(get.R1.utils(complex,salienceG2R1))
R1G3 = get.br(get.R1.utils(unamb,salienceG2R1))
R1G4 = get.br(get.R1.utils(amb,salienceG4R1))
# utils S2
uS2G1 = log(t(R1G1))
uS2G2 = log(t(R1G2))
uS2G2[,4] = rep(-Inf,3) # knock out nonsensical distractor choices
uS2G3 = log(t(R1G3))
uS2G4 = log(t(R1G4))