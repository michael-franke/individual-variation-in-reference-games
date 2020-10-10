library(reshape2)
library(ggplot2)

source('~/Desktop/data/svn/programming/R/helpers/helpers.R')
source('jags_E10/RSA_speaker_mixed_E10.preamble.R')

useOldSamples = TRUE

if (useOldSamples) {
  load("jags_E10/samplesSpeaker_RSAMixE10.Rdata")
} else{
  source('jags_E10/RSA_speaker_mixed_E10.R')  
}

x = samples$BUGSoutput$sims.list

lambda <- x$lambda0
epsilon <- x$epsilon
ppG1 <- x$ppG1
ppG2 <- x$ppG2
ppG3 <- x$ppG3
ppG4 <- x$ppG4
p <- x$p
typeprior <- x$typeprior
population = table(factor(p, levels = 1:3)) / length(p)

summaryPosteriors = data.frame(variable = c("epsilon", "lambda", "P1", "P2", "P3"),
                               HDImin = c(HDIofMCMC(epsilon)[1], HDIofMCMC(lambda)[1],
                                          HDIofMCMC(typeprior[,1])[1], HDIofMCMC(typeprior[,2])[1], HDIofMCMC(typeprior[,3])[1]),
                               mean = c(mean(epsilon), mean(lambda),
                                        mean(typeprior[,1]), mean(typeprior[,2]), mean(typeprior[,3])),
                               HDImax = c(HDIofMCMC(epsilon)[2], HDIofMCMC(lambda)[2],
                                          HDIofMCMC(typeprior[,1])[2], HDIofMCMC(typeprior[,2])[2], HDIofMCMC(typeprior[,3])[2])
                               )

# posteriors on epsilon and lambda
dataPost = data.frame(epsilon = epsilon, lambda = lambda)
dataPost = melt(dataPost)
postEpsilonLambdaSpeaker = ggplot(dataPost, aes(x = value)) + geom_density() + facet_wrap(~ variable, scales = "free")
show(postEpsilonLambdaSpeaker)
ggsave(filename = "../text/PLoSone_2/pics/postEpsilonLambdaSpeaker.pdf", postEpsilonLambdaSpeaker, width = 6, height = 3)


# posteriors on typeprior
dataPost = data.frame(S0 = typeprior[,1], S1 = typeprior[,2], S2 = typeprior[,3])
dataPost = melt(dataPost)
dataPost$type = factor(dataPost$variable)
postTypesS = ggplot(dataPost, aes(x = value, lty = type, color = type)) +
  geom_density(size = 1.15) + scale_colour_grey(start = 0, end = .8) + 
  theme_bw() + xlab('probability')
show(postTypesS)
ggsave(filename = "../text/PLoSone_2/pics/postTypesSpeaker.pdf", postTypesS, width = 6, height = 3)

# plotting typeprior density estimates:
dtp = data.frame(S0 = typeprior[,1], S1 = typeprior[,2], S2 = typeprior[,3])
p2d = ggplot(dtp, aes(x = S1, y = S2)) + 
  stat_density2d(aes(fill=..level..), geom="polygon") + 
  #   xlim(0,0.3) + ylim(0.6,1) +
  xlab("population prior S1") + ylab("population prior S2") +
  geom_point(colour="red",alpha=0.1, size = 1.1) +
  geom_abline(intercept = 1, slope = -1) +
  guides(fill=FALSE) +
  theme_bw()
show(p2d)
ggsave(filename = "../text/PLoSone_2/pics/posteriorPopulationDistributionSpeaker.pdf", p2d, width = 5, height = 5)



mycols = gg_color_hue(4)[c(1,2,4,3)]

# posterior predictive check
simplePPC = sapply(1:dim(ppG1)[1], function(x) sum(ppG1[x,,1]))
complexPPC = sapply(1:dim(ppG1)[1], function(x) sum(ppG2[x,,1]))
ambigPPC = sapply(1:dim(ppG1)[1], function(x) sum(ppG4[x,,1]))
unambPPC = sapply(1:dim(ppG1)[1], function(x) sum(ppG3[x,,1]))
d.list = list(d1 = density(simplePPC,adjust=1),
              d2 = density(complexPPC,adjust=1),
              d3 = density(unambPPC,adjust=1),
              d4 = density(ambigPPC,adjust=1))
hdi.list = list(h1 = HDIofMCMC(simplePPC,credMass=0.95),
                h2 = HDIofMCMC(complexPPC,credMass=0.95),
                h3 = HDIofMCMC(unambPPC,credMass=0.95),
                h4 = HDIofMCMC(ambigPPC,credMass=0.95))
ypos = - max(sapply(1:4, function(i) d.list[[i]]$y))/200 # length of HDI segment
yskip = 0.001 # height of marker for observed counts
dPPC = data.frame(simple = simplePPC, complex = complexPPC,
                  ambig = ambigPPC, unambig = unambPPC)
# dPPC = data.frame(simple = d.list[[1]]$x , complex = d.list[[2]]$x,
#                   ambig = d.list[[3]]$x, unambig = d.list[[4]]$x)
dPPC = melt(dPPC)
dPPC$condition = factor(dPPC$variable)
pPPC = ggplot(dPPC, aes(x = value, color = condition, group = condition)) + 
  geom_density() + xlab("predicted/observed number of target choices") +
#   xlim(0,1500) + 
  geom_segment(aes(x = hdi.list[[1]][1], y = ypos, xend = hdi.list[[1]][2], yend = ypos), colour=mycols[1]) + # HDI interval  
  geom_segment(aes(x = obsS[1,1], y = ypos - yskip, xend = obsS[1,1], yend = ypos + yskip), colour=mycols[1]) + # observations  
  geom_segment(aes(x = hdi.list[[2]][1], y = ypos, xend = hdi.list[[2]][2], yend = ypos), colour=mycols[2]) + # HDI interval  
  geom_segment(aes(x = obsS[2,1], y = ypos - yskip, xend = obsS[2,1], yend = ypos + yskip), colour=mycols[2]) + # observations  
  geom_segment(aes(x = hdi.list[[3]][1], y = ypos, xend = hdi.list[[3]][2], yend = ypos), colour=mycols[3]) + # HDI interval
  geom_segment(aes(x = obsS[3,1], y = ypos - yskip, xend = obsS[3,1], yend = ypos + yskip), colour=mycols[3]) + # observations  
  geom_segment(aes(x = hdi.list[[4]][1], y = ypos, xend = hdi.list[[4]][2], yend = ypos), colour=mycols[4]) + # HDI interval
geom_segment(aes(x = obsS[4,1], y = ypos - yskip, xend = obsS[4,1], yend = ypos + yskip), colour=mycols[4])  # observations  
show(pPPC)
ggsave("../text/PLoSone_2/pics/PPCSpeaker.pdf", pPPC, width = 6, height = 3.5 )




# posterior on subjective types tau_i
prop = matrix(0,nrow=nS,ncol=3)
for (i in 1:nS){
  prop[i,] = sapply(1:3, function(x) sum(p[,i] == x))
}
prop = prop.table(prop,1)
rownames(prop) = dimnames(obsBS)[[2]]
weight = sapply(1:nS , function(x) 0*prop[x,1] + 2 * prop[x,2] + 3* prop[x,3])
prop = prop[order(weight),]
d.tau = data.frame(subject = factor(1:dim(prop)[1]),
                   S0 = prop[,1],
                   S1 = prop[,2],
                   S2 = prop[,3])
d.tauM = melt(d.tau, id.vars = c("subject"))
d.tauM$type = factor(d.tauM$variable)
plot.tau = ggplot(d.tauM, aes(x = subject, y = value, fill = type)) + geom_bar(stat = "identity") + theme_bw() + scale_fill_grey(start = 0, end = .8) +
  xlab("subject (ordered for visual presentation)") +
  ylab("probability") + ylim(c(0,1)) + scale_x_discrete(labels=c(""))
ggsave(filename = "../text/PLoSone_2/pics/posteriorReasoningTypesSpeaker.pdf", plot.tau, width = 6, height = 6)


