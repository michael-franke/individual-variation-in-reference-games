# RSA speaker model, mixed types
var S0G1[3,4], S0G2[3,4], S0G3[3,4], S0G4[3,4],
S1tmptG1[3,4], S1tmptG2[3,4], S1tmptG3[3,4], S1tmptG4[3,4],
S2tmptG1[3,4], S2tmptG2[3,4], S2tmptG3[3,4], S2tmptG4[3,4],
G1target[nS,4],
G2target[nS,4],
G3target[nS,4],
G4target[nS,4],
ppG1[nS,4],
ppG2[nS,4],
ppG3[nS,4],
ppG4[nS,4];
model{
#   lambda0 ~ dunif(0,lMax)
   lambda1 <- lambda0
   lambda2 <- lambda0
#     lambda1 ~ dunif(0,lMax)
#     lambda2 ~ dunif(0,lMax)
  lambda0 ~ dgamma(2,0.5)
#   lambda1 ~ dgamma(2,0.5)
#   lambda2 ~ dgamma(2,0.5)
#   epsilon ~ dunif(0,eMax)
  epsilon ~ dgamma(0.25,0.1)
  for (i in 1:3){
    typehyperprior[i] <- 1
  }
  typeprior ~ ddirich(typehyperprior)
  for (i in 1:nS){
    p[i] ~ dcat(typeprior)
  }
  ########
  # S0
  ########
  # G1
  for (i in 1:3){
    for (j in 1:4){
      S0G1tmp[i,j] <- exp( lambda0 * uS0G1[i,j] )
    }
  }
  for (i in 1:3){
    S0G1[i,] <- S0G1tmp[i,] / sum(S0G1tmp[i,])
  }
  S0G1target <- S0G1[2,]
  # G2
  for (i in 1:3){
    for (j in 1:4){
      S0G2tmp[i,j] <- exp( lambda0 * uS0G2[i,j] )
    }
  }
  for (i in 1:3){
    S0G2[i,] <- S0G2tmp[i,] / sum(S0G2tmp[i,])
  }
  S0G2target <- S0G2[1,]
  # G3
  for (i in 1:3){
    for (j in 1:4){
      S0G3tmp[i,j] <- exp( lambda0 * uS0G3[i,j] )
    }
  }
  for (i in 1:3){
    S0G3[i,] <- S0G3tmp[i,] / sum(S0G3tmp[i,])
  }
  S0G3target <- S0G3[1,]
  # G4
  for (i in 1:3){
    for (j in 1:4){
      S0G4tmp[i,j] <- exp( lambda0 * uS0G4[i,j] )
    }
  }
  for (i in 1:3){
    S0G4[i,] <- S0G4tmp[i,] / sum(S0G4tmp[i,])
  }
  S0G4target <- S0G4[1,]
  ########
  # S1
  ########
  # G1
  for (i in 1:3){
    for (j in 1:4){
      S1tmpG1[i,j] <- exp( lambda1 * (uS1G1[i,j] ))
    }
  }
  for (i in 1:3){
    S1tmptG1[i,] <- S1tmpG1[i,] / sum(S1tmpG1[i,])
    for (j in 1:4){
      S1G1[i,j] <- (1-4*epsilon) * S1tmptG1[i,j] + epsilon
    }
  }
  S1G1target <- S1G1[2,]
  # G2
  for (i in 1:3){
    for (j in 1:4){
      S1tmpG2[i,j] <- exp( lambda1 * (uS1G2[i,j] ))
    }
  }
  for (i in 1:3){
    S1tmptG2[i,] <- S1tmpG2[i,] / sum(S1tmpG2[i,])
    for (j in 1:4){
      S1G2[i,j] <- (1-4*epsilon) * S1tmptG2[i,j] + epsilon
    }
  }
  S1G2target <- S1G2[1,]
  # G3
  for (i in 1:3){
    for (j in 1:4){
      S1tmpG3[i,j] <- exp( lambda1 * (uS1G3[i,j] ))
    }
  }
  for (i in 1:3){
    S1tmptG3[i,] <- S1tmpG3[i,] / sum(S1tmpG3[i,])
    for (j in 1:4){
      S1G3[i,j] <- (1-4*epsilon) * S1tmptG3[i,j] + epsilon
    }
  }
  S1G3target <- S1G3[1,]
  # G4
  for (i in 1:3){
    for (j in 1:4){
      S1tmpG4[i,j] <- exp( lambda1 * (uS1G4[i,j] ))
    }
  }
  for (i in 1:3){
    S1tmptG4[i,] <- S1tmpG4[i,] / sum(S1tmpG4[i,])
    for (j in 1:4){
      S1G4[i,j] <- (1-4*epsilon) * S1tmptG4[i,j] + epsilon
    }
  }
  S1G4target <- S1G4[1,]
  ########
  # S2
  ########
  # G1
  for (i in 1:3){
    for (j in 1:4){
      S2tmpG1[i,j] <- exp( lambda2 * (uS2G1[i,j] ))
    }
  }
  for (i in 1:3){
    S2tmptG1[i,] <- S2tmpG1[i,] / sum(S2tmpG1[i,])
    for (j in 1:4){
      S2G1[i,j] <- (1-4*epsilon) * S2tmptG1[i,j] + epsilon
    }
  }
  S2G1target <- S2G1[2,]
  # G2
  for (i in 1:3){
    for (j in 1:4){
      S2tmpG2[i,j] <- exp( lambda2 * (uS2G2[i,j] ))
    }
  }
  for (i in 1:3){
    S2tmptG2[i,] <- S2tmpG2[i,] / sum(S2tmpG2[i,])
    for (j in 1:4){
      S2G2[i,j] <- (1-4*epsilon) * S2tmptG2[i,j] + epsilon
    }
  }
  S2G2target <- S2G2[1,]
  # G3
  for (i in 1:3){
    for (j in 1:4){
      S2tmpG3[i,j] <- exp( lambda2 * (uS2G3[i,j] ))
    }
  }
  for (i in 1:3){
    S2tmptG3[i,] <- S2tmpG3[i,] / sum(S2tmpG3[i,])
    for (j in 1:4){
      S2G3[i,j] <- (1-4*epsilon) * S2tmptG3[i,j] + epsilon
    }
  }
  S2G3target <- S2G3[1,]
  # G4
  for (i in 1:3){
    for (j in 1:4){
      S2tmpG4[i,j] <- exp( lambda2 * (uS2G4[i,j] ))
    }
  }
  for (i in 1:3){
    S2tmptG4[i,] <- S2tmpG4[i,] / sum(S2tmpG4[i,])
    for (j in 1:4){
      S2G4[i,j] <- (1-4*epsilon) * S2tmptG4[i,j] + epsilon
    }
  }
  S2G4target <- S2G4[1,]
  ###############
  ## Data fitting
  ###############
  # get target choice probabilities
  for (i in 1:nS){
    G1target[i,] <- equals(p[i],1)*S0G1target + equals(p[i],2)*S1G1target + equals(p[i],3)*S2G1target
    G2target[i,] <- equals(p[i],1)*S0G2target + equals(p[i],2)*S1G2target + equals(p[i],3)*S2G2target
    G3target[i,] <- equals(p[i],1)*S0G3target + equals(p[i],2)*S1G3target + equals(p[i],3)*S2G3target
    G4target[i,] <- equals(p[i],1)*S0G4target + equals(p[i],2)*S1G4target + equals(p[i],3)*S2G4target
  }
  # macht to observations
  for (i in 1:nS){
    obsBS[1,i,] ~ dmulti(G1target[i,], nT[1])
    obsBS[2,i,] ~ dmulti(G2target[i,], nT[2])
    obsBS[3,i,] ~ dmulti(G3target[i,], nT[3])
    obsBS[4,i,] ~ dmulti(G4target[i,], nT[4])
  }
  # Posterior predictive
  for (i in 1:nS){
    ppG1[i,] ~ dmulti(G1target[i,], nT[1])
    ppG2[i,] ~ dmulti(G2target[i,], nT[2])
    ppG3[i,] ~ dmulti(G3target[i,], nT[3])
    ppG4[i,] ~ dmulti(G4target[i,], nT[4])
  }
  
}