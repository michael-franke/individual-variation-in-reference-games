source("iqr-helpers.r")
source("Monster-games.R")

# main function to compute RSA's prediction
rsa <- function(game,lambda, epsilon = 0,prior=0) {
  
  # get information about the game
  n.states = nrow(game)-1
  n.messages = ncol(game)-1
  semantics = game[1:n.states,1:n.messages]
  if (length(prior) != n.states){
    prior = as.vector(game[1:n.states,n.messages+1])  
  }
  cost.matrix = matrix(rep(game[n.states+1,1:n.messages],n.states),nrow=n.messages)
  prior.matrix = matrix(rep(rep(1/n.states,n.states),n.messages),ncol=n.messages,byrow=F)
  
  # dummy receiver
  bel.r0 = prop.table(t(semantics * prior.matrix),1)
  bel.r0[is.nan(bel.r0)] = 0
  
  # sender
  utils.s = t(log(bel.r0) - cost.matrix) 
  sen = get.qbr(utils.s,lambda)
  sen = (1-(epsilon*n.messages)) * sen + matrix(rep(epsilon,times=n.states*n.messages),nrow=n.states)

  # receiver
  rec = get.posterior(sen,prior)
  
  rec[is.nan(rec)] = 1/n.states
  
  return(list(sender = sen, receiver = rec))
}

rsa.R0.R2 <- function(game,lambda.s,lambda.r=-1) {
  
  if (lambda.r == -1) {
    lambda.r = lambda.s
  }
  
  # get information about the game
  n.states = nrow(game)-1
  n.messages = ncol(game)-1
  semantics = game[1:n.states,1:n.messages]
  prior = as.vector(game[1:n.states,n.messages+1])
  cost.matrix = matrix(rep(game[n.states+1,1:n.messages],n.states),nrow=n.messages)
  prior.matrix = matrix(rep(prior,n.messages),ncol=n.messages,byrow=F)
  
  ######## new stuff 
  
  # determine level-0 players
  utils.r0 = prop.table(t(semantics * prior.matrix),1)
  for (i in 1:n.messages){
    if (is.nan(sum(utils.r0[i,]))){
      utils.r0[i,] = rep(1/n.states,n.states)
    }
  }
  rec.0 = get.qbr(utils.r0,lambda.r)
  
  # sender
  utils.s = t((rec.0) - cost.matrix) 
  sen = get.qbr(utils.s,lambda.s)
  
  # receiver
  posterior = get.posterior(sen,prior)
#   rec[is.nan(rec)] = 1/n.states
  rec = get.qbr(posterior,lambda.r)
  
  return(list(sender = sen, receiver = rec, rec.0 = rec.0))
}

rsa.R2 <- function(game,lambda.s,lambda.r=-1) {
  
    epsilon = 0.0086
  
  if (lambda.r == -1) {
    lambda.r = lambda.s
  }
  
  # get information about the game
  n.states = nrow(game)-1
  n.messages = ncol(game)-1
  semantics = game[1:n.states,1:n.messages]
  prior = as.vector(game[1:n.states,n.messages+1])
  cost.matrix = matrix(rep(game[n.states+1,1:n.messages],n.states),nrow=n.messages)
  prior.matrix = matrix(rep(prior,n.messages),ncol=n.messages,byrow=F)
  
  bel.r0 = prop.table(t(semantics * prior.matrix),1)
  bel.r0[is.nan(bel.r0)] = 0
  
  # sender
  utils.s = t(log(bel.r0) - cost.matrix) 
  sen = get.qbr(utils.s,lambda.s)
  sen = (1-(epsilon*n.messages)) * sen + matrix(rep(epsilon,times=n.states*n.messages),nrow=n.states)
  
  
  # receiver
  posterior = get.posterior(sen,prior)
  #   rec[is.nan(rec)] = 1/n.states
  rec = get.qbr(posterior,lambda.r)
  
  return(list(sender = sen, receiver = rec))
}

rsa.linear <- function(game, epsilon = 0) {
  

  # get information about the game
  n.states = nrow(game)-1
  n.messages = ncol(game)-1
  semantics = game[1:n.states,1:n.messages]
  prior = as.vector(game[1:n.states,n.messages+1])
  cost.matrix = matrix(rep(game[n.states+1,1:n.messages],n.states),nrow=n.messages)
  prior.matrix = matrix(rep(prior,n.messages),ncol=n.messages,byrow=F)
  
  # determine level-0 players
  utils.r0 = prop.table(t(semantics * prior.matrix),1)
  for (i in 1:n.messages){
    if (is.nan(sum(utils.r0[i,]))){
      utils.r0[i,] = rep(1/n.states,n.states)
    }
  }
  rec.0 = prop.table(utils.r0,1)
  rec.0 = (1-(epsilon*n.states)) * rec.0 + matrix(rep(epsilon,times=n.states*n.messages),nrow=n.messages)
  
  # sender
  utils.s = t((rec.0) - cost.matrix) 
  sen = prop.table(utils.s,1)
  sen = (1-(epsilon*n.messages)) * sen + matrix(rep(epsilon,times=n.states*n.messages),nrow=n.states)
  
  # receiver
  posterior = get.posterior(sen,prior)
  rec = prop.table(posterior,1)
  rec = (1-(epsilon*n.states)) * rec + matrix(rep(epsilon,times=n.states*n.messages),nrow=n.messages)
  
  return(list(sender = sen, receiver = rec, rec.0 = rec.0))
}

rsaTargetSimple = function(lambda = 1, epsilon = 0, prior = rep(1/3,3)){
  predictions = rsa(game = simple, lambda = lambda, prior = prior , epsilon = epsilon )
  return(c(predictions[[1]][2,1], predictions[[2]][2,1]) )
}

rsaTargetComplex = function(lambda = 1, epsilon = 0, prior = rep(1/3,3)){
  predictions = rsa(game = complex, lambda = lambda, prior = prior , epsilon = epsilon )
  return(c(predictions[[1]][1,1], predictions[[2]][1,1]) )
}

rsaTarget = function(lambda = 1, epsilon = 0, prior= matrix(1/3, nrow = 4, ncol = 3)){
  predictions = matrix(c(rsaTargetSimple(lambda, epsilon, prior[1,]),
                         rsaTargetComplex(lambda, epsilon, prior[1,])), 
                       ncol = 2)
  colnames(predictions) = c("simple", "complex")
  rownames(predictions) = c("speaker", "listener")
  return(predictions)
}