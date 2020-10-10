library(reshape2)
library(ggplot2)

source('jags_E10/RSA_listener_mixed_E10.preamble.R')

useOldSamples = TRUE

if (useOldSamples) {
  load("jags_E10/samplesListener_RSAMixE10.Rdata")
} else{
  source('jags_E10/RSA_listener_mixed_E10.R')  
}

# where to save PDFs
path = "../pics_dev/receiver_rsa_mixed/"
# colors for plots
mycol = c("blue","red","green","orange","purple")

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

dl0 = density(lambda0.post,adjust=2)
dl1 = density(lambda1.post,adjust=2)
dl2 = density(lambda2.post,adjust=2)

ppG1.t = sapply(1:dim(ppG1)[1], function(x) sum(ppG1[x,,1]))
ppG2.t = sapply(1:dim(ppG2)[1], function(x) sum(ppG2[x,,1]))
ppG3.t = sapply(1:dim(ppG3)[1], function(x) sum(ppG3[x,,1]))
ppG4.t = sapply(1:dim(ppG4)[1], function(x) sum(ppG4[x,,1]))

# #### This can go; just for fun
# get.bsSim = function(bsSamples){
#   bsSim = matrix(0, nrow = dim(bsSamples)[1], ncol = 13)
#   colnames(bsSim) = 0:12
#   for(i in 1:dim(bsSamples)[1]){
#     bsSim[i,] = as.vector(table(factor(bsSamples[i,,1], levels=0:12)))
#   }
#   return(bsSim)
# }
# 
# bsSimG1 = get.bsSim(ppG1)
# bsSimG2 = get.bsSim(ppG2)
# 
# meanBSSimG1 = sapply(1:13, function(x) mean(bsSimG1[,x]))
# meanBSSimG2 = sapply(1:13, function(x) mean(bsSimG2[,x]))
# #### This can go; just for fun


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
  points(obsR[conditions[i],1],d.list[[i]]$y[which.min(abs(d.list[[i]]$x - obsR[conditions[i],1]))],lwd=5,col=mycol[i])
}
legend('top', c("simple_1", "complex", "unambig", "ambig", "simple_2"),fill=mycol)
dev.off()


## plot by-subject data #######
fname = paste(path,"BS-Exp",experiment,".pdf",collapse="", sep="")
pdf(file=fname,width=5.00,height=6.00)
plot.bs.targets(pS= c( R0(simple,lambda=estimateMax(lambda0.post))[2,1] , 
                       R1(simple,lambda=estimateMax(lambda1.post),prior=salienceG1R1)[2,1], 
                       rsa(simple,lambda=estimateMax(lambda2.post),prior=salienceG1R2,epsilon=estimateMax(epsilon.post))[[2]][2,1]) ,
                pC= c( R0(complex,lambda=estimateMax(lambda0.post))[1,1] , 
                       R1(complex,lambda=estimateMax(lambda1.post),prior=salienceG2R1)[1,1], 
                       rsa(complex,lambda=estimateMax(lambda2.post),prior=salienceG2R2,epsilon=estimateMax(epsilon.post))[[2]][1,1]),
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
rownames(prop) = 1:nS
weight = sapply(1:nS , function(x) 0*prop[x,1] + 2 * prop[x,2] + 3* prop[x,3])
barplot(t(prop[order(weight),]),xlab="subject",ylab="posterior")
legend("topright",legend=c("R0","R1","R2"),fill=gray.colors(3))
dev.off()

show(c("population:", round(population,3)))

show(c("lambda:", round(estimateMax(lambda0.post),3), round(estimateMax(lambda1.post),3), round(estimateMax(lambda2.post),3)))

show(samples$BUGSoutput$DIC)

## calculate Bayes Factor in favor of RSA-typeprior c(0,0,1), given data
## prior for c(0,0,1) udner Dirichlet with concentration parameters c(1,1,1) is 2
## posterior is:
d3 = density(typeprior[,3], from=0, to = 1, n = 101)
postRSATypes = d3$y[101]
show(paste("BF in favor of RSA types: ", postRSATypes/2, collapse = "", sep = ""))

# receiver
type = sapply(1: dim(p)[2], function(subject) mean(as.numeric(mymode(p[,subject]))))
bsDataL = get.bsData(comp)
bsDataL$type = factor(type)
obsPlotL = ggplot(data = bsDataL, aes(x = simple, y = complex, shape = type, color = type)) + 
  geom_point(position=position_jitter(width=0.015,height=.015)) +
  xlim(c(0,1.05)) + ylim(c(0,1.05)) 
show(obsPlotL)

# try plotting typeprior density estimates:

dtp = data.frame(R0 = typeprior[,1], R1 = typeprior[,2], R2 = typeprior[,3])

p2d = ggplot(dtp, aes(x = R1, y = R2)) + 
  stat_density2d(aes(fill=..level..), geom="polygon") + 
  #   xlim(0,0.3) + ylim(0.6,1) +
  geom_point(colour="red",alpha=0.1, size = 1.1) +
  geom_abline(intercept = 1, slope = -1) +
  theme_bw()
show(p2d)

