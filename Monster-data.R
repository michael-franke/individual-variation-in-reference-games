require('plyr')
require('catspec') 

load('data/production.RData')
load('data/comprehension.RData')
load('data/Exp5_criticaltrials-inference.RData') # variable name: 'critical'
load('data/Exp6_prior_md.RData') # variable name: 'md'

load("data/salience_E10.Rdata") # salience_E10

## comprehension 1
c1 = subset(comp,comp$Experiment == 1)
c1.obs = table(c1$ImpType,
           c1$ResponseType)[c(5,2,8,1,6,7,3,4),c(3,1,2)]
c1 = prop.table(c1.obs,1)

## comprehension 2
c2 = subset(comp,comp$Experiment == 2)
c2.obs = table(c2$ImpType,
           c2$ResponseType)[c(5,2,8,1,6,7,3,4),c(3,1,2)]
c2 = prop.table(c2.obs,1)

## comprehension 3
c3 = subset(comp,comp$Experiment == 3)
c3.obs = table(c3$ImpType,
           c3$ResponseType)[c(5,2,8,1,6,7,3,4),c(3,1,2)]
c3 = prop.table(c3.obs,1)

## comprehension 4
c4 = subset(comp,comp$Experiment == 4)
c4.obs = table(c4$ImpType,
           c4$ResponseType)[c(5,2,8,1,6,7,3,4),c(3,1,2)]
c4 = prop.table(c4.obs,1)

## comprehension 5 (variably costly messages)
log.odds.diff.costlier = -0.44
log.odds.diff.costliest = -0.83
c5.obs = table(critical$Condition, critical$ResponseType)[,c(3,1,2)]
c5 = prop.table(c5.obs,1)

## comprehension all 
ca.obs = c1.obs + c2.obs + c3.obs + c4.obs
ca = prop.table(ca.obs,1)

## production 1
p1 = subset(prod,prod$Experiment == 1)
p1.obs = table(p1$ImpType,
           p1$ResponseType)[c(5,2,8,1,6,7,3,4),c(3,1,2,4)]
p1 = prop.table(p1.obs,1)

## production 2
p2 = subset(prod,prod$Experiment == 2)
p2.obs = table(p2$ImpType,
           p2$ResponseType)[c(5,2,8,1,6,7,3,4),c(3,1,2,4)]
p2 = prop.table(p2.obs,1)

## production 3
p3 = subset(prod,prod$Experiment == 3)
p3.obs = table(p3$ImpType,
           p3$ResponseType)[c(5,2,8,1,6,7,3,4),c(3,1,2,4)]
p3 = prop.table(p3.obs,1)

## production 4
p4 = subset(prod,prod$Experiment == '4')
p4.obs = table(p4$ImpType,p4$ResponseType)[c(5,2,8,1,6,7,3,4),c(3,1,2,4)]
p4 = prop.table(p4.obs,1)

## comprehension all 
pa.obs = p1.obs + p2.obs + p3.obs + p4.obs
pa = prop.table(pa.obs,1)

## create data look-up table
lu = data.frame(TrialType = rep(rownames(c1),2),
                Mode = rep(c('comp','prod'),each=8),
                TriggerIndex = c(2, 1, 1, 1, 1, 3, 3, 2,
                                 2, 1, 1, 3, 1, 3, 3, 2),
                TarIndex =     c(1, 1, 1, 1, 2, 3, 2, 3,
                                 1, 1, 1, 2, 2, 3, 2, 3),
                CIndex =       c(2, 2, 2, 2, 1, 1, 3, 1, 
                                 2, 2, 2, 3, 1, 4, 1, 1),
                DIndex =       c(3, 3, 3, 3, 3, 2, 1, 2,
                                 3, 3, 3, 1, 3, 1, 3, 2),
                D2Index =      c(NA,NA,NA,NA,NA,NA,NA,NA,
                                 4, 4, 4, 4, 4, 2, 4, 4),
                TarValue = as.vector(cbind(c1[,1],p1[,1])),
                CValue = as.vector(cbind(c1[,2],p1[,2])),
                DValue = as.vector(cbind(c1[,3],p1[,3])),
                D2Value = as.vector(cbind(rep(c(NA),8),p1[,4]))
)

## salience priors
md.r = subset(md, assignmentsubmittime != "Wed Aug 21 23:38:47 CEST 2013") # delete second trial of subject "A2SHU5ZKDJB176"
t = table(md.r$ImpType,md.r$RType)
salience = prop.table(as.matrix(t[c(5,2,8,1,6,7,3,4),]),1)

## by-subject data
## comprehension 1
c1.bs = subset(comp,comp$Experiment == 1)
c1.bs = droplevels.data.frame(c1.bs)
c1.bs.obs = table(c1.bs$ImpType,  
                  c1.bs$workerid, c1.bs$ResponseType)[c(5,2,8,1,6,7,3,4),,c(3,1,2)]
c1.bs.obs.readable = table(c1.bs$ImpType,  
                   c1.bs$ResponseType,c1.bs$workerid)[c(5,2,8,1,6,7,3,4),c(3,1,2),]
# dimnames(c1.bs.obs.readable)[[3]] = 1:dim(c1.bs.obs.readable)[3]

## comprehension 2
c2.bs = subset(comp,comp$Experiment == 2)
c2.bs = droplevels.data.frame(c2.bs)
c2.bs.obs = table(c2.bs$ImpType,  
                  c2.bs$workerid, c2.bs$ResponseType)[c(5,2,8,1,6,7,3,4),,c(3,1,2)]
c2.bs.obs.readable = table(c2.bs$ImpType,  
                           c2.bs$ResponseType,c2.bs$workerid)[c(5,2,8,1,6,7,3,4),c(3,1,2),]

## comprehension 3
c3.bs = subset(comp,comp$Experiment == 3)
c3.bs = droplevels.data.frame(c3.bs)
c3.bs.obs = table(c3.bs$ImpType,  
                  c3.bs$workerid, c3.bs$ResponseType)[c(5,2,8,1,6,7,3,4),,c(3,1,2)]
c3.bs.obs.readable = table(c3.bs$ImpType,  
                           c3.bs$ResponseType,c3.bs$workerid)[c(5,2,8,1,6,7,3,4),c(3,1,2),]

## comprehension 4
c4.bs = subset(comp,comp$Experiment == 4)
c4.bs = droplevels.data.frame(c4.bs)
c4.bs.obs = table(c4.bs$ImpType,  
                  c4.bs$workerid, c4.bs$ResponseType)[c(5,2,8,1,6,7,3,4),,c(3,1,2)]
c4.bs.obs.readable = table(c4.bs$ImpType,  
                           c4.bs$ResponseType,c4.bs$workerid)[c(5,2,8,1,6,7,3,4),c(3,1,2),]

## production 1
p1.bs = subset(prod,prod$Experiment == 1)
p1.bs = droplevels.data.frame(p1.bs)
p1.bs.obs = table(p1.bs$ImpType,  
                  p1.bs$workerid, p1.bs$ResponseType)[c(5,2,8,1,6,7,3,4),,c(3,1,2)]
p1.bs.obs.readable = table(p1.bs$ImpType,  
                           p1.bs$ResponseType,p1.bs$workerid)[c(5,2,8,1,6,7,3,4),c(3,1,2),]

## production 2
p2.bs = subset(prod,prod$Experiment == 2)
p2.bs = droplevels.data.frame(p2.bs)
p2.bs.obs = table(p2.bs$ImpType,  
                  p2.bs$workerid, p2.bs$ResponseType)[c(5,2,8,1,6,7,3,4),,c(3,1,2)]
p2.bs.obs.readable = table(p2.bs$ImpType,  
                           p2.bs$ResponseType,p2.bs$workerid)[c(5,2,8,1,6,7,3,4),c(3,1,2),]

## production 3
p3.bs = subset(prod,prod$Experiment == 3)
p3.bs = droplevels.data.frame(p3.bs)
p3.bs.obs = table(p3.bs$ImpType,  
                  p3.bs$workerid, p3.bs$ResponseType)[c(5,2,8,1,6,7,3,4),,c(3,1,2)]
p3.bs.obs.readable = table(p3.bs$ImpType,  
                           p3.bs$ResponseType,p3.bs$workerid)[c(5,2,8,1,6,7,3,4),c(3,1,2),]

## production 4
p4.bs = subset(prod,prod$Experiment == 4)
p4.bs = droplevels.data.frame(p4.bs)
p4.bs.obs = table(p4.bs$ImpType,  
                  p4.bs$workerid, p4.bs$ResponseType)[c(5,2,8,1,6,7,3,4),,c(3,1,2)]
p4.bs.obs.readable = table(p4.bs$ImpType,  
                           p4.bs$ResponseType,p4.bs$workerid)[c(5,2,8,1,6,7,3,4),c(3,1,2),]

## data from experiment 10
source("Monster-data_E10.R")
# source("Monster-data_E10-full.R")

## success rates by individual
get.succ = function(data){
  succ = matrix(0,nrow = 8, ncol=7)
  rownames(succ) = dimnames(data)[[1]]
  colnames(succ) = 0:6
  for (i in 1:8) {
    succ[i,] = table(factor(data[i,1,],levels=0:6))
  }
  return(t(succ))
}

succ1 = get.succ(c1.bs.obs.readable)[,1:2]
succ2 = get.succ(c2.bs.obs.readable)[,1:2]
succ3 = get.succ(c3.bs.obs.readable)[,1:2]
succ4 = get.succ(c4.bs.obs.readable)[,1:2]

succS1 = get.succ(p1.bs.obs.readable)[,1:2]
succS2 = get.succ(p2.bs.obs.readable)[,1:2]
succS3 = get.succ(p3.bs.obs.readable)[,1:2]
succS4 = get.succ(p4.bs.obs.readable)[,1:2]


## lists of data

obsBSR.L = list(c1.bs.obs, c2.bs.obs, c3.bs.obs, c4.bs.obs, c10.bs.obs)
obsBSS.L = list(p1.bs.obs, p2.bs.obs, p3.bs.obs, p4.bs.obs, p10.bs.obs)
obsR.L = list(c1.obs, c2.obs, c3.obs, c4.obs, c10.obs)
obsS.L = list(p1.obs, p2.obs, p3.obs, p4.obs, p10.obs)
succR.L = list(succ1,succ2,succ3,succ4,succ10)
succ.L = succR.L
succS.L = list(succS1,succS2,succS3,succS4,succS10)

