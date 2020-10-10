library(R2jags)

source('Monster-data.R')
source('Monster-games.R')
source('iqr-helpers.r')
source('rsa.r')
source('helpers/helpers.R')

model = "jags_E10/RSA_speaker_E10.jags.R"

lMax = 20
cMax = 0.00001
eMax = 0.25

# select what to test
experiment = 5
conditions = c(1:4)

# where to save PDFs
path = "../pics_dev/sender_rsa/"
# colors for plots
mycol = c("blue","red","green","orange","purple")

nconditions = length(conditions)
k = obsS.L[[experiment]][conditions,]
n = sapply(conditions, function(x) sum(obsS.L[[experiment]][x,]) )
costVec = c(0,0,0,0)
succ = succS.L[[experiment]]

u = matrix(c(1, .5, 0, 0,
             .5, .5, 0, 0,
             1, 0, 0, 0,
             1, 1, 0, 0), nrow=4, byrow=T)
# u = log(u)

data <- list("k", "n", "lMax", "cMax", "costVec","u", "eMax", "nconditions") # to be passed on to JAGS

myinits <-  list(
  list(lambda = runif(1)*lMax,
       cost = cMax*runif(1)-cMax/2,
       epsilon = runif(1)*eMax), #chain 1 starting value
  list(lambda = runif(1)*lMax,
       cost = cMax*runif(1)-cMax/2,
       epsilon = runif(1)*eMax)) #chain 2 starting value

parameters <- c("lambda", "lambdaPrior", "cost", "costPrior", "epsilon", "epsilonPrior"
                , "postpred1"
                , "postpred2"
                , "postpred3"
                , "postpred4"
                )

samples <- jags(data, inits=myinits, parameters,
                model.file = model, n.chains=2, n.iter=1000, 
                n.burnin=100, n.thin=1, DIC=T)

lambda.post <- samples$BUGSoutput$sims.list$lambda
lambda.prior <- samples$BUGSoutput$sims.list$lambdaPrior
cost.post <- samples$BUGSoutput$sims.list$cost
cost.prior <- samples$BUGSoutput$sims.list$costPrior
epsilon.post <- samples$BUGSoutput$sims.list$epsilon
epsilon.prior <- samples$BUGSoutput$sims.list$epsilonPrior
postpred1 <- samples$BUGSoutput$sims.list$postpred1
postpred2 <- samples$BUGSoutput$sims.list$postpred2
postpred3 <- samples$BUGSoutput$sims.list$postpred3
postpred4 <- samples$BUGSoutput$sims.list$postpred4

## Plot Posteriors #####
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
d.list = list(d1 = density(postpred1[,1],adjust=2),
              d2 = density(postpred2[,1],adjust=2),
              d3 = density(postpred3[,1],adjust=3),
              d4 = density(postpred4[,1],adjust=2))

plot(d.list[[3]],xlim=c(50,max(n)), col="green", 
     main=paste("Experiment", experiment),
     xlab="number of target observations",
     ylab="sampled frequency")
for (i in c(1,2,4)){
  lines(d.list[[i]], col=mycol[i])  
}

for (i in 1:4){
  points(k[i,1],d.list[[i]]$y[which.min(abs(d.list[[i]]$x - k[i,1]))],lwd=5,col=mycol[i])
}
legend('top', c("simple_1", "complex", "unambig", "ambig", "simple_2"),fill=mycol)
dev.off()

## Plot BS-data ####
fname = paste(path,"BS-Exp",experiment,".pdf",collapse="", sep="")
pdf(file=fname,width=5.00,height=6.00)
plot.bs.targets(pS= rsa(simple,  lambda=estimateMax(lambda.post),prior=rep(1/3,3), epsilon=estimateMax(epsilon.post))[[1]][2,1]  ,
                pC= rsa(complex, lambda=estimateMax(lambda.post),prior=rep(1/3,3), epsilon=estimateMax(epsilon.post))[[1]][1,1] ,
                q=1,
                succ=succ, total.choices = 12)
dev.off()

show(samples$BUGSoutput$DIC)