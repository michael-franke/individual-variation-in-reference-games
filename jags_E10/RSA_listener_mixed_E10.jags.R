# RSA listener model
var s1G1[3,4], s1tmpG1[3,4], s1tmptG1[3,4],
s1G2[3,4], s1tmpG2[3,4], s1tmptG2[3,4],
s1G3[3,4], s1tmpG3[3,4], s1tmptG3[3,4],
s1G4[3,4], s1tmpG4[3,4], s1tmptG4[3,4],
r0G1[4,3], r0tmpG1[4,3],
r0G2[4,3], r0tmpG2[4,3],
r0G3[4,3], r0tmpG3[4,3],
r0G4[4,3], r0tmpG4[4,3],
r1G1[4,3], r1tmpG1[4,3],
r1G2[4,3], r1tmpG2[4,3],
r1G3[4,3], r1tmpG3[4,3],
r1G4[4,3], r1tmpG4[4,3],
r2G1[4,3], r2tmpG1[4,3], r2tmptG1[4,3],
r2G2[4,3], r2tmpG2[4,3], r2tmptG2[4,3],
r2G3[4,3], r2tmpG3[4,3], r2tmptG3[4,3],
r2G4[4,3], r2tmpG4[4,3], r2tmptG4[4,3],
obsBS[4,nS,3], obsR[4,3],
uS1G1[3,4], uS1G2[3,4], uS1G3[3,4], uS1G4[3,4], 
uR0G1[4,3], uR0G2[4,3], uR0G3[4,3], uR0G4[4,3], 
uR1G1[4,3], uR1G2[4,3], uR1G3[4,3], uR1G4[4,3], 
salienceG1R2R2[3],
salienceG2R2[3],
salienceG3R2[3],
salienceG4R2[3],
G1target[nS,3],
G2target[nS,3],
G3target[nS,3],
G4target[nS,3],
ppG1[nS,3],
ppG2[nS,3],
ppG3[nS,3],
ppG4[nS,3],
typeprior[3],
p[nS];
#
model{
#   lambda0 ~ dunif(0,lMax)
  lambda1 <- lambda0
  lambda2 <- lambda0
#   lambda1 ~ dunif(0,lMax)
#   lambda2 ~ dunif(0,lMax)
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
  #######
  # R0
  #######
  # G1
  for (i in 1:4){
    for (j in 1:3){
      r0G1tmp[i,j] <- exp( lambda0 * uR0G1[i,j] )
    }
  }
  for (i in 1:4){
    r0G1[i,] <- r0G1tmp[i,] / sum(r0G1tmp[i,])
  }
  r0G1target <- r0G1[2,]
  # G2
  for (i in 1:4){
    for (j in 1:3){
      r0G2tmp[i,j] <- exp( lambda0 * uR0G2[i,j] )
    }
  }
  for (i in 1:4){
    r0G2[i,] <- r0G2tmp[i,] / sum(r0G2tmp[i,])
  }
  r0G2target <- r0G2[1,]
  # G3
  for (i in 1:4){
    for (j in 1:3){
      r0G3tmp[i,j] <- exp( lambda0 * uR0G3[i,j] )
    }
  }
  for (i in 1:4){
    r0G3[i,] <- r0G3tmp[i,] / sum(r0G3tmp[i,])
  }
  r0G3target <- r0G3[1,]
  # G4
  for (i in 1:4){
    for (j in 1:3){
      r0G4tmp[i,j] <- exp( lambda0 * uR0G4[i,j] )
    }
  }
  for (i in 1:4){
    r0G4[i,] <- r0G4tmp[i,] / sum(r0G4tmp[i,])
  }
  r0G4target <- r0G4[1,]
  #######
  # R1
  #######
  # G1
  for (i in 1:4){
    for (j in 1:3){
      r1G1tmp[i,j] <- exp( lambda1 * uR1G1[i,j] )
    }
  }
  for (i in 1:4){
    r1G1[i,] <- r1G1tmp[i,] / sum(r1G1tmp[i,])
  }
  r1G1target <- r1G1[2,]
  # G2
  for (i in 1:4){
    for (j in 1:3){
      r1G2tmp[i,j] <- exp( lambda1 * uR1G2[i,j] )
    }
  }
  for (i in 1:4){
    r1G2[i,] <- r1G2tmp[i,] / sum(r1G2tmp[i,])
  }
  r1G2target <- r1G2[1,]
  # G3
  for (i in 1:4){
    for (j in 1:3){
      r1G3tmp[i,j] <- exp( lambda1 * uR1G3[i,j] )
    }
  }
  for (i in 1:4){
    r1G3[i,] <- r1G3tmp[i,] / sum(r1G3tmp[i,])
  }
  r1G3target <- r1G3[1,]
  # G4
  for (i in 1:4){
    for (j in 1:3){
      r1G4tmp[i,j] <- exp( lambda1 * uR1G4[i,j] )
    }
  }
  for (i in 1:4){
    r1G4[i,] <- r1G4tmp[i,] / sum(r1G4tmp[i,])
  }
  r1G4target <- r1G4[1,]
  #######
  # S1
  #######
  # G1
  for (i in 1:3){
    for (j in 1:4){
      s1tmpG1[i,j] <- exp( lambda2 * (uS1G1[i,j] ))
    }
  }
  for (i in 1:3){
    s1tmptG1[i,] <- s1tmpG1[i,] / sum(s1tmpG1[i,])
    for (j in 1:4){
      s1G1[i,j] <- (1-4*epsilon) * s1tmptG1[i,j] + epsilon
    }
  }
  # G2
  for (i in 1:3){
    for (j in 1:4){
      s1tmpG2[i,j] <- exp( lambda2 * (uS1G2[i,j] ))
    }
  }
  for (i in 1:3){
    s1tmptG2[i,] <- s1tmpG2[i,] / sum(s1tmpG2[i,])
    for (j in 1:4){
      s1G2[i,j] <- (1-4*epsilon) * s1tmptG2[i,j] + epsilon
    }
  }
  # G3
  for (i in 1:3){
    for (j in 1:4){
      s1tmpG3[i,j] <- exp( lambda2 * (uS1G3[i,j] ))
    }
  }
  for (i in 1:3){
    s1tmptG3[i,] <- s1tmpG3[i,] / sum(s1tmpG3[i,])
    for (j in 1:4){
      s1G3[i,j] <- (1-4*epsilon) * s1tmptG3[i,j] + epsilon
    }
  }
  # G4
  for (i in 1:3){
    for (j in 1:4){
      s1tmpG4[i,j] <- exp( lambda2 * (uS1G4[i,j] ))
    }
  }
  for (i in 1:3){
    s1tmptG4[i,] <- s1tmpG4[i,] / sum(s1tmpG4[i,])
    for (j in 1:4){
      s1G4[i,j] <- (1-4*epsilon) * s1tmptG4[i,j] + epsilon
    }
  }
  #######
  # R2
  ######
  # G1
  for (i in 1:4){
    for (j in 1:3){
      r2tmpG1[i,j] <- s1G1[j,i] * salienceG1R2[j]
    }
  }
  for (i in 1:4){
    r2G1[i,] <- r2tmpG1[i,] / sum(r2tmpG1[i,]) 
  }
  r2G1target <- r2G1[2,]
  # G2
  for (i in 1:4){
    for (j in 1:3){
      r2tmpG2[i,j] <- s1G2[j,i] * salienceG2R2[j]
    }
  }
  for (i in 1:4){
    r2G2[i,] <- r2tmpG2[i,] / sum(r2tmpG2[i,]) 
  }
  r2G2target <- r2G2[1,]
  # G3
  for (i in 1:4){
    for (j in 1:3){
      r2tmpG3[i,j] <- s1G3[j,i] * salienceG3R2[j]
    }
  }
  for (i in 1:4){
    r2G3[i,] <- r2tmpG3[i,] / sum(r2tmpG3[i,]) 
  }
  r2G3target <- r2G3[1,]
  # G4
  for (i in 1:4){
    for (j in 1:3){
      r2tmpG4[i,j] <- s1G4[j,i] * salienceG4R2[j]
    }
  }
  for (i in 1:4){
    r2G4[i,] <- r2tmpG4[i,] / sum(r2tmpG4[i,]) 
  }
  r2G4target <- r2G4[1,]
  # target choice probabilities
  for (i in 1:nS){
    G1target[i,] <- equals(p[i],1)*r0G1target + equals(p[i],2)*r1G1target + equals(p[i],3)*r2G1target
    G2target[i,] <- equals(p[i],1)*r0G2target + equals(p[i],2)*r1G2target + equals(p[i],3)*r2G2target
    G3target[i,] <- equals(p[i],1)*r0G3target + equals(p[i],2)*r1G3target + equals(p[i],3)*r2G3target
    G4target[i,] <- equals(p[i],1)*r0G4target + equals(p[i],2)*r1G4target + equals(p[i],3)*r2G4target
  }
  # match to observations
  for (i in 1:nS){
    obsBS[1,i,] ~ dmulti(G1target[i,], nT[1])
    obsBS[2,i,] ~ dmulti(G2target[i,], nT[2])
    obsBS[3,i,] ~ dmulti(G3target[i,], nT[3])
    obsBS[4,i,] ~ dmulti(G4target[i,], nT[4])
  }
#   for (i in 1:3){
#     G1targetFull[i] <- mean(G1target[,i])
#     G2targetFull[i] <- mean(G2target[,i])
#     G8targetFull[i] <- mean(G8target[,i])
#   }
#   obsR[1,] ~ dmulti(G1targetFull,nG1)
#   obsR[2,] ~ dmulti(G2targetFull,nG2)
#   obsR[8,] ~ dmulti(G8targetFull,nG8)
  # Posterior predictive
  for (i in 1:nS){
    ppG1[i,] ~ dmulti(G1target[i,], nT[1])
    ppG2[i,] ~ dmulti(G2target[i,], nT[2])
    ppG3[i,] ~ dmulti(G3target[i,], nT[3])
    ppG4[i,] ~ dmulti(G4target[i,], nT[4])
  }
}