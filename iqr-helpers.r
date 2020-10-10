source("helpers/HDIofMCMC.R")


######################################################################

get.posterior = function(sen,pr) {
  # returns the receiver's posterior beliefs
  # sen: sender strategy matrix
  # pr:  priors; vector of length dim(sen)[1]
  n.s = dim(sen)[1] # number of states
  n.m = dim(sen)[2] # number of messages
  post = matrix(rep(x=1,n.m*n.s), ncol=n.s)
  for (m in 1:n.m) {  
    for (s in 1:n.s){
      post[m,s] = sen[s,m]*pr[s] / sum(sen[,m]*pr)
    }
  }
  rownames(post) = colnames(sen)
  colnames(post) = rownames(sen)
  post
}

######################################################################

get.k.max = function(tau,threshold=0.05){
  # for a given value $\tau$, look for the smallest $k$
  # such that the probability of all types $>k$ is 
  # smaller than the specified threshold of relevance
  #  --- see below for motivation & use ---
  
  x = ppois(0:100,tau)
  length(x[x<=1-threshold])
}

######################################################################

get.qbr = function(utils,lambda,succ.prop=F){
  # returns the quantal best response given:
  # utils: matrix of expected utilities
  # lambda: rationality parameter
  # succ.prob: boolean parameter;
  #  if false, a single lamda is used for each choice point
  #  as in standard quantal response;
  #  if true, each choice point receives a lambda
  #  proportional to the maximal probability of 
  #  communicative success;
  #  motivation is that decision makers may be more
  #  rational when they can be sure of success than
  #  when they cannot;
  #  the latter is only a first idea for solving
  #  Horn's DoPL using IQR
  if (succ.prop) {
    lambda.matrix =  matrix( 
      rep( lambda * (1-(1 - apply(utils,1,max))) , each=dim(utils)[2]),
      byrow=T,nrow=dim(utils)[1])
    #     lambda.matrix =  matrix( 
    #       rep( lambda * (apply(utils,1,max) + minimum) / (maximum - minimum), each=dim(utils)[1]),
    #       byrow=T,nrow=dim(utils)[2])
  }
  if (succ.prop == F) {
    lambda.matrix = lambda
  }
  prop.table(exp(lambda.matrix*utils),1)
}


######################################################################


plot.population = function(tau,k.max=0,maintitle="") {
  # get and plot population distribution
  if (k.max<=0){
    k.max = get.k.max(tau)
  }
  p.k = dpois(0:k.max,tau)
#   p.k[1] = 0.00001 # dirty hack to leave out level-0 players!
  p.k = p.k / sum(p.k)
  barplot(p.k,
          #           width = 1,
          names.arg = 0:k.max,
          ylab = 'frequency',
          xlab = 'strategic type',
          main = maintitle,
          ylim = c(0,1))
}


######################################################################

nLL = function(observations,predictions) {
  ## observations: matrix of observed frequencies
  ##    rows are trials or experiments
  ##    cols are categories
  ## predictions: probabilities predicted by a model
  
  ll = 0
  
  for (r in 1:dim(observations)[1]){
    ll = ll +  dmultinom(observations[r,],prob=predictions[r,],log=TRUE) 
  }
  
  return(- ll)
}

######################################################################

get.S1.utils <- function(game,prior) {
  
  # get information about the game
  n.states = nrow(game)-1
  n.messages = ncol(game)-1
  semantics = game[1:n.states,1:n.messages]
  prior.matrix = matrix(rep(prior,n.messages),ncol=n.messages,byrow=F)
  
  # dummy receiver
  bel.r0 = prop.table(t(semantics * prior.matrix),1)
  bel.r0[is.nan(bel.r0)] = 0
  
  # sender
  utils.s = t((bel.r0))
  return(utils.s)
}

get.speaker.utils = get.S1.utils

S1 = function(game, lambda = 1, prior = rep(1/3,3), epsilon = 0){
  u = get.S1.utils(game,prior)
  SPrime = get.qbr(u, lambda)
  S1 = prop.table(SPrime + epsilon, 1)
  return(S1)
}

get.S0.utils = function(game) {
  # get information about the game
  n.states = nrow(game)-1
  n.messages = ncol(game)-1
  semantics = game[1:n.states,1:n.messages]
  utils = prop.table(semantics,1)
  return(utils)
}

get.R1.utils <- function(game,prior) {
  # get information about the game
  n.states = nrow(game)-1
  n.messages = ncol(game)-1
  semantics = game[1:n.states,1:n.messages]
  s0 = prop.table(semantics,1)
  prior.matrix = matrix(rep(prior,n.messages),ncol=n.messages,byrow=F)
  # compute R1's utilities
  bel.r1 = prop.table(t(s0 * prior.matrix),1)
  bel.r1[is.nan(bel.r1)] = 0
  return(bel.r1)
}

get.R0.utils <- function(game,prior=rep(1/3,3)) {
  # get information about the game
  n.states = nrow(game)-1
  n.messages = ncol(game)-1
  semantics = game[1:n.states,1:n.messages]
  prior.matrix = matrix(rep(prior,n.messages),ncol=n.messages,byrow=F)
  # compute R0's utilities
  bel.r1 = prop.table(t(semantics*prior.matrix),1)
  bel.r1[is.nan(bel.r1)] = 1/n.states
  return(bel.r1)
}

get.br <- function(utils) { ## TODO: double check this!!
  # utils: matrix with choice points in rows and choices in column
  br <- t(apply(utils,1,function(x) x==max(x)))
  for (i in 1:(dim(br)[1])){
    if (sum(br[i,]) == 0){
      br[i,] = 1/dim(br)[1]
    }
  }
  return(prop.table(br,1))
}

get.R2.utils <- function(game,prior=rep(1/3,3)) {
  # get information about the game
  R0 = get.R0.utils(game)
  S1 = get.br(t(R0))
  u.R2 = prop.table(t(S1)*prior,1)
  return(u.R2)
}

R1 <- function(game,lambda=1,prior=rep(1/3,3)) {
  u = get.R1.utils(game,prior)
  return(get.qbr(u,lambda))
}

R0 = function(game = simple, lambda = 1) {
  u = get.R0.utils(game)
  return(get.qbr(u,lambda))
}

S0 = function(game = simple, lambda = 1) {
  u = get.S0.utils(game)
  return(get.qbr(u,lambda))
}

S0eps = function(game = simple, epsilon = 0) {
  u = get.S0.utils(game)
  u = u + epsilon
  return(prop.table(u,1))
}

S2 = function(game, lambda = 1, prior = rep(1/3,3), epsilon = 0, complex.adjust = F) {
  R1mat = get.br(get.R1.utils(game,prior))
  if (complex.adjust){
    R1mat[4,] = rep(0,3)
  }
  S2Prime = get.qbr(t(log(R1mat)),lambda)
  S2 = prop.table(S2Prime + epsilon, 1)
  return(S2)
}

######################################################################

mybin = function(p,q,total.choices=6) {
  out = rep(0,total.choices+1)
  for (i in 1:length(p)){
    out = out + q[i] * dbinom(0:total.choices,size=total.choices,prob=p[i])
  }
  return(out) 
}

mybin.sample = function(p,q,size) {
  probs = mybin(p,q,size)
  return(rmultinom(n=1,size=size,prob=probs))
}

plot.bs.targets <- function(pS,pC,q,succ=succ1,total.choices=6) {
  nS = sum(succ[,1])
  psuccS = mybin(pS,q,total.choices)
  psuccC = mybin(pC,q,total.choices)
  barplot(t(succ),beside=T,col=c('pink','white'),
          xlab='Number of target responses',
          ylab='Number of subjects',names.arg=rownames(succ),
          ylim=c(0,nS))
  legend(2,20,c('simple','complex'),fill=c('pink','white'),title='Condition')
  points(3*(0:total.choices)+1.5,nS*psuccS,type='b',col='red')
  points(3*(0:total.choices)+2.5,nS*psuccC,type='b',col='black')
    
  rmsd = c( sqrt(mean((succ[,1]/nS-psuccS)^2) ), 
            sqrt(mean((succ[,2]/nS-psuccC)^2) ))
  show(round(rmsd,3))
  show(c("chisquared simple: " , chisq.test(succ[,1],p=psuccS, simulate.p.value=TRUE)[3],
         "chisquared complex: ", chisq.test(succ[,2],p=psuccC, simulate.p.value=TRUE)[3]))
}

get_chi2_values = function(pS,pC,q,succ=succ1,total.choices=6){
  nS = sum(succ[,1])
  psuccS = mybin(pS,q, total.choices)
  psuccC = mybin(pC,q, total.choices)
  show(c("chisquared simple: " , chisq.test(succ[,1],p=psuccS, simulate.p.value=TRUE)[3],
         "chisquared complex: ", chisq.test(succ[,2],p=psuccC, simulate.p.value=TRUE)[3]))
}

get.bsData = function(data){
  bsData = ddply(subset(data, !grepl(pattern = "ambiguous", x = redimptype)), .(workerid, redimptype), summarise, 
                 meanScore = sum(response == "target") / length(response) )
  bsData =  dcast(bsData, workerid ~ redimptype , value.var="meanScore")
  return(bsData)
}

get.bsMatrix = function(bsData){
  bsMatrix = as.matrix(bsData[,c("simple", "complex")])
  rownames(bsMatrix) = bsData$workerid
  return(bsMatrix)
}
