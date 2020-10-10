library(R2jags)

source('rsa.r')
source('iqr-helpers.r')
source('Monster-data.R')
source('Monster-games.R')
source('helpers/helpers.R')

model = "jags_E10/RSA_listener_E10.jags.R"

# where to save PDFs
path = "../pics_dev/receiver_rsa/"
# colors for plots
mycol = c("blue","red","green","orange","purple")


experiment = 5
conditions = c(1,2,3,4)

lMax = 20
eMax = 0.25

## flat priors
salienceG1 = rep(1/3,3)
salienceG2 = rep(1/3,3)
salienceG3 = rep(1/3,3)
salienceG4 = rep(1/3,3)
# salienceG8 = rep(1/3,3)
## estimated salience priors
salienceG1 = salience_E10[1,]
salienceG2 = salience_E10[2,]
salienceG3 = salience_E10[3,]
salienceG4 = salience_E10[4,]

# salienceG1 = salience[1,]
# salienceG2 = salience[2,]
# salienceG3 = salience[3,]
# salienceG4 = salience[4,]

uS1G1 = log(get.speaker.utils(simple,rep(1/3,3)))
uS1G2 = log(get.speaker.utils(complex,rep(1/3,3)))
uS1G3 = log(get.speaker.utils(unamb,rep(1/3,3)))
uS1G4 = log(get.speaker.utils(amb,rep(1/3,3)))

uS1G1 = get.speaker.utils(simple,rep(1/3,3))
uS1G2 = get.speaker.utils(complex,rep(1/3,3))
uS1G3 = get.speaker.utils(unamb,rep(1/3,3))
uS1G4 = get.speaker.utils(amb,rep(1/3,3))
# uS1G8 = get.speaker.utils(complex,rep(1/3,3))

obsBS = obsBSR.L[[experiment]]
obsR = obsR.L[[experiment]]
obsS = obsS.L[[experiment]]
succ = succ.L[[experiment]]

# number of trials
nG1 = sum(obsR[1,])
nG2 = sum(obsR[2,])
nG3 = sum(obsR[3,])
nG4 = sum(obsR[4,])
# nG8 = sum(obsR[8,])
n = c(nG1,nG2,nG3,nG4)

data <- list("salienceG1", "salienceG2", "salienceG3", "salienceG4", 
             "nG1", "nG2", "nG3", "nG4", 
             "uS1G1", "uS1G2", "uS1G3", "uS1G4", 
             "obsS", "obsR",
             "lMax", "eMax") # to be passed on to JAGS

myinits <-  list(
  list(lambda = runif(1)*lMax,
       epsilon = runif(1)*eMax), #chain 1 starting value
  list(lambda = runif(1)*lMax,
       epsilon = runif(1)*eMax)) #chain 2 starting value

parameters <- c("lambda", "lambdaPrior", "epsilon", "epsilonPrior"
                , "s1G1target", "s1G2target", "s1G3target", "s1G4target" 
                , "r2G1target", "r2G2target", "r2G3target", "r2G4target" 
                , "ppS1G1", "ppS1G2", "ppS1G3", "ppS1G4" 
                , "ppR2G1", "ppR2G2", "ppR2G3", "ppR2G4" 
                )



samples <- jags(data, inits=myinits, parameters,
                model.file = model, n.chains=2, n.iter=10000, 
                n.burnin=3000, n.thin=1, DIC=T)

lambda.post <- samples$BUGSoutput$sims.list$lambda
lambda.prior <- samples$BUGSoutput$sims.list$lambdaPrior
epsilon.post <- samples$BUGSoutput$sims.list$epsilon
epsilon.prior <- samples$BUGSoutput$sims.list$epsilonPrior
ppS1G1 <- samples$BUGSoutput$sims.list$ppS1G1
ppS1G2 <- samples$BUGSoutput$sims.list$ppS1G2
ppS1G3 <- samples$BUGSoutput$sims.list$ppS1G3
ppS1G4 <- samples$BUGSoutput$sims.list$ppS1G4
ppR2G1 <- samples$BUGSoutput$sims.list$ppR2G1
ppR2G2 <- samples$BUGSoutput$sims.list$ppR2G2
ppR2G3 <- samples$BUGSoutput$sims.list$ppR2G3
ppR2G4 <- samples$BUGSoutput$sims.list$ppR2G4


# lambda.post.list[[e]] = density(lambda.post)
# epsilon.post.list[[e]] = density(epsilon.post)
# samp.list.RSA.r[[e]] = samples

## Plot posteriors #######
fname = paste(path,"Posterior-Exp",experiment,".pdf",collapse="", sep="")
pdf(file=fname,width=5.00,height=4.00)
par(mfrow=c(1,2))
plot(density(lambda.post), xlim=c(0,lMax), xlab="lambda", ylab="posterior", main="P(lambda|D)")
plot(density(epsilon.post), xlim=c(0,eMax), xlab="epsilon", ylab="posterior", main="P(epsilon|D)")
dev.off()


## Plot PPCs #####
fname = paste(path,"PPC-Exp",experiment,".pdf",collapse="", sep="")
pdf(file=fname,width=5.00,height=6.00)
par(mfrow=c(1,1))
d.list = list(d1 = density(ppR2G1[,1],adjust=2),
              d2 = density(ppR2G2[,1],adjust=2),
              d3 = density(ppR2G3[,1],adjust=2),
              d4 = density(ppR2G4[,1],adjust=2))

ymax = max(sapply(1:length(conditions), function(c) d.list[[c]]$y))
cond.list = list(ppR2G1[,1],ppR2G2[,1],ppR2G3[,1],ppR2G4[,1])

plot(d.list[[1]],xlim=c(30,max(n)), col=mycol[1], ylim=c(0,ymax) ,
     main=paste("extended"),
     xlab="number of target observations",
     ylab="sampled frequency")
for (i in 1:length(conditions)){
  lines(d.list[[i]], col=mycol[i])  
  hdi = HDIofMCMC(cond.list[[i]],credMass=0.95)
#   hdi.min = which.min(abs(dlistTFp[[i]]$x-hdi[1]))
#   hdi.max = which.min(abs(dlistTFp[[i]]$x-hdi[2]))
#   hdi.min.x = dlistTFp[[i]]$x[hdi.min]
#   hdi.max.x = dlistTFp[[i]]$x[hdi.max]
#   hdi.min.y = dlistTFp[[i]]$y[hdi.min]
#   hdi.max.y = dlistTFp[[i]]$y[hdi.max]
#   #if (i==2) {show(hdi.min.x)}
  lines(c(hdi[1],hdi[2]), 
        rep(0,2),
        type="l", lty="solid", col=mycol[i])
}

for (i in 1:length(conditions)){
  points(obsR[conditions[i],1],d.list[[i]]$y[which.min(abs(d.list[[i]]$x - obsR[conditions[i],1]))],lwd=5,col=mycol[i])
  lines(rep(obsR[conditions[i],1],2), 
        c(0,d.list[[i]]$y[which.min(abs(d.list[[i]]$x - obsR[conditions[i],1]))]),
        type="l", lty="solid", col=mycol[i])
  
}
legend('top', c("simple", "complex", "unambig", "ambig", "simple_2")[conditions],fill=mycol)
dev.off()


## Plot BS-data ####
fname = paste(path,"BS-Exp",experiment,".pdf",collapse="", sep="")
pdf(file=fname,width=5.00,height=6.00)
plot.bs.targets(pS= rsa(simple,lambda=estimateMax(lambda.post),prior=salienceG1,epsilon=estimateMax(epsilon.post))[[2]][2,1]  ,
                pC= rsa(complex,lambda=estimateMax(lambda.post),prior=salienceG2,epsilon=estimateMax(epsilon.post))[[2]][1,1] ,
                q=1,
                succ=succ, total.choices = 12)
dev.off()


show(samples$BUGSoutput$DIC)



