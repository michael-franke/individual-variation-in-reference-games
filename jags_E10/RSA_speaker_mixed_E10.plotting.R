library(reshape2)
library(ggplot2)

source('jags_E10/RSA_speaker_mixed_E10.preamble.R')

# useOldSamples = TRUE
# 
# if (useOldSamples) {
#   load("jags_E10/samplesSpeaker_RSAMixE10.Rdata")
# } else{
#   source('jags_E10/RSA_speaker_mixed_E10.R')  
# }

# where to save PDFs
path = "../pics_dev/sender_rsa_mixed/"
# colors for plots
mycol = c("blue","red","green","orange","purple")

# select what to test
experiment = 5
conditions = 1:4

lMax = 20
eMax = 0.25

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

lambda0.post <- samples$BUGSoutput$sims.list$lambda0
lambda1.post <- samples$BUGSoutput$sims.list$lambda1
lambda2.post <- samples$BUGSoutput$sims.list$lambda2
epsilon.post <- samples$BUGSoutput$sims.list$epsilon
ppG1 <- samples$BUGSoutput$sims.list$ppG1
ppG2 <- samples$BUGSoutput$sims.list$ppG2
ppG3 <- samples$BUGSoutput$sims.list$ppG3
ppG4 <- samples$BUGSoutput$sims.list$ppG4
p <- samples$BUGSoutput$sims.list$p
typeprior <- samples$BUGSoutput$sims.list$typeprior
population = table(factor(p, levels = 1:3)) / length(p)
x = samples$BUGSoutput$sims.list

dl0 = density(lambda0.post,adjust=2)
dl1 = density(lambda1.post,adjust=2)
dl2 = density(lambda2.post,adjust=2)

ppG1.t = sapply(1:dim(ppG1)[1], function(x) sum(ppG1[x,,1]))
ppG2.t = sapply(1:dim(ppG2)[1], function(x) sum(ppG2[x,,1]))
ppG3.t = sapply(1:dim(ppG3)[1], function(x) sum(ppG3[x,,1]))
ppG4.t = sapply(1:dim(ppG4)[1], function(x) sum(ppG4[x,,1]))

## Plot posteriors #######
fname = paste(path,"Posterior-Exp",experiment,".pdf",collapse="", sep="")
pdf(file=fname,width=5.00,height=4.00)
ymax = max(c(dl0$y,dl1$y,dl2$y))
par(mfrow=c(1,2))
plot(dl0, xlim=c(0,lMax), ylim=c(0,ymax), xlab="lambda", ylab="posterior", main="P(lambda|D)", col=mycol[1])
lines(dl1,col=mycol[2])
lines(dl2,col=mycol[3])
legend("topright",legend=c("lambda_0", "lambda_1", "lambda_2"), fill=mycol[1:3])
plot(density(epsilon.post), xlim=c(0,eMax), xlab="epsilon", ylab="posterior", main="P(epsilon|D)")
dev.off()

## Plot PPCs #######
fname = paste(path,"PPC-Exp",experiment,".pdf",collapse="", sep="")
pdf(file=fname,width=5.00,height=6.00)
par(mfrow=c(1,1))
d.list = list(d1 = density(ppG1.t,adjust=2),
              d2 = density(ppG2.t,adjust=2),
              d3 = density(ppG3.t,adjust=2),
              d4 = density(ppG4.t,adjust=2))
ymax = max(sapply(1:length(d.list), function(x) max(d.list[[x]]$y) ))
plot(d.list[[1]],xlim=c(30,max(n)), ylim=c(0,ymax*1.2), col=mycol[1], 
     main=paste("Experiment", experiment),
     xlab="number of target observations",
     ylab="sampled frequency")
for (i in 1:length(conditions)){
  lines(d.list[[i]], col=mycol[i])  
}
for (i in 1:length(conditions)){
  points(obsS[conditions[i],1],d.list[[i]]$y[which.min(abs(d.list[[i]]$x - obsS[conditions[i],1]))],lwd=5,col=mycol[i])
}
legend('top', c("simple_1", "complex", "unambig", "ambig", "simple_2"),fill=mycol)
dev.off()


## plot by-subject data #######
fname = paste(path,"BS-Exp",experiment,".pdf",collapse="", sep="")
pdf(file=fname,width=5.00,height=6.00)
plot.bs.targets(pS= c( S0(simple, lambda=estimateMax(lambda0.post))[2,1] , 
                       rsa(simple, lambda=estimateMax(lambda1.post), prior=salienceG1R1, epsilon=estimateMax(epsilon.post))[[1]][2,1], 
                       S2(simple, lambda=estimateMax(lambda2.post), prior=salienceG1R1, epsilon=estimateMax(epsilon.post))[2,1]) ,
                pC= c( S0(complex, lambda=estimateMax(lambda0.post))[1,1] , 
                       rsa(complex, lambda=estimateMax(lambda1.post), prior=salienceG2R1)[[1]][1,1], 
                       S2(complex, lambda=estimateMax(lambda2.post), prior=salienceG2R1, epsilon=estimateMax(epsilon.post), complex.adjust = T)[1,1]),
                q=population,
                succ=succ, total.choices = 12)
dev.off()

## plot subject-type posterior #######
fname = paste(path,"SubjPosterior",experiment,".pdf",collapse="", sep="")
pdf(file=fname,width=5.00,height=6.00)
prop = matrix(0,nrow=nS,ncol=3)
for (i in 1:nS){
  prop[i,] = sapply(1:3, function(x) sum(p[,i] == x))
}
prop = prop.table(prop,1)
rownames(prop) = dimnames(obsBS)[[2]]
weight = sapply(1:nS , function(x) 0*prop[x,1] + 2 * prop[x,2] + 3* prop[x,3])
barplot(t(prop[order(weight),]),xlab="subject",ylab="posterior")
legend("topright",legend=c("S0","S1","S2"),fill=gray.colors(3))
dev.off()

show(c("population:", round(population,3)))

show(c("lambda:", round(estimateMax(lambda0.post),3), round(estimateMax(lambda1.post),3), round(estimateMax(lambda2.post),3)))

show(samples$BUGSoutput$DIC)

## calculate Bayes Factor in favor of RSA-typeprior c(0,1,0), given data
## prior for c(0,1,0) under Dirichlet with concentration parameters c(1,1,1) is 2
## posterior is:
d2 = density(typeprior[,2], from=0, to = 1, n = 101)
postRSATypes = d2$y[101]
show(paste("BF in favor of RSA types: ", postRSATypes/2, collapse = "", sep = ""))


# map each participant to its most likely type:

# sender
type = sapply(1: dim(p)[2], function(subject) mean(as.numeric(mymode(p[,subject]))))
bsDataL = get.bsData(prod)
bsDataL$type = factor(type)
obsPlotL = ggplot(data = bsDataL, aes(x = simple, y = complex, shape = type, color = type)) + 
  geom_point(position=position_jitter(width=0.015,height=.015)) +
  xlim(c(0,1.05)) + ylim(c(0,1.05)) 
show(obsPlotL)

# try plotting typeprior density estimates:

dtp = data.frame(S0 = typeprior[,1], S1 = typeprior[,2], S2 = typeprior[,3])

p2d = ggplot(dtp, aes(x = S1, y = S2)) + 
  stat_density2d(aes(fill=..level..), geom="polygon") + 
  #   xlim(0,0.3) + ylim(0.6,1) +
  geom_point(colour="red",alpha=0.1, size = 1.1) +
  geom_abline(intercept = 1, slope = -1) +
  theme_bw()
show(p2d)