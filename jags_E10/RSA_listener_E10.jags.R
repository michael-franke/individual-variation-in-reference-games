# RSA listener model
var s1G1[3,4], s1tmpG1[3,4], s1tmptG1[3,4],
    s1G2[3,4], s1tmpG2[3,4], s1tmptG2[3,4],
    s1G3[3,4], s1tmpG3[3,4], s1tmptG3[3,4],
    s1G4[3,4], s1tmpG4[3,4], s1tmptG4[3,4],
    r2G1[4,3], r2tmpG1[4,3], r2tmptG1[4,3],
    r2G2[4,3], r2tmpG2[4,3], r2tmptG2[4,3],
    r2G3[4,3], r2tmpG3[4,3], r2tmptG3[4,3],
    r2G4[4,3], r2tmpG4[4,3], r2tmptG4[4,3],
    obsS[4,4], obsR[4,3],
    uS1G1[3,4],
    uS1G2[3,4],
    uS1G3[3,4],
    uS1G4[3,4],
    salienceG1[3],
    salienceG2[3],
    salienceG3[3],
    salienceG4[3];
#
model{
  #cost ~ dunif(-cMax/2,cMax/2)
  #costPrior ~ dunif(-cMax/2,cMax/2)
  lambda ~ dunif(0,lMax)
  lambdaPrior ~ dunif(0,lMax)
  epsilon ~ dunif(0,eMax)
  epsilonPrior ~ dunif(0,eMax)
  #######
  # S1
  #######
  # G1
  for (i in 1:3){
    for (j in 1:4){
      s1tmpG1[i,j] <- exp( lambda * (uS1G1[i,j] ))
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
      s1tmpG2[i,j] <- exp( lambda * (uS1G2[i,j] ))
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
      s1tmpG3[i,j] <- exp( lambda * (uS1G3[i,j] ))
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
      s1tmpG4[i,j] <- exp( lambda * (uS1G4[i,j] ))
    }
  }
  for (i in 1:3){
    s1tmptG4[i,] <- s1tmpG4[i,] / sum(s1tmpG4[i,])
    for (j in 1:4){
      s1G4[i,j] <- (1-4*epsilon) * s1tmptG4[i,j] + epsilon
    }
  }
#   # G8
#   for (i in 1:3){
#     for (j in 1:4){
#       s1tmpG8[i,j] <- exp( lambda * (uS1G8[i,j] ))
#     }
#   }
#   for (i in 1:3){
#     s1tmptG8[i,] <- s1tmpG8[i,] / sum(s1tmpG8[i,])
#     for (j in 1:4){
#       s1G8[i,j] <- (1-4*epsilon) * s1tmptG8[i,j] + epsilon
#     }
#   }
  s1G1target <-s1G1[2,]
  s1G2target <-s1G2[1,]
  s1G3target[1] <-s1G3[3,2]
  s1G3target[2] <-s1G3[3,3]
  s1G3target[3] <-s1G3[3,1]
  s1G3target[4] <-s1G3[3,4]
  s1G4target <-s1G4[1,]
#   s1G8target[1] <-s1G8[2,3]
#   s1G8target[2] <-s1G8[2,1]
#   s1G8target[3] <-s1G8[2,2]
#   s1G8target[4] <-s1G8[2,4]
  ## match to observations
  #obsS[1,] ~ dmulti(s1G1target,nG1)
  #obsS[2,] ~ dmulti(s1G2target,nG2)
  #obsS[3,] ~ dmulti(s1G2target,nG3)
  #obsS[4,] ~ dmulti(s1G2target,nG4)
  #obsS[8,] ~ dmulti(s1G8target,nG8)
  # Posterior predictive
  ppS1G1 ~ dmulti(s1G1target,nG1)
  ppS1G2 ~ dmulti(s1G2target,nG2)
  ppS1G3 ~ dmulti(s1G3target,nG3)
  ppS1G4 ~ dmulti(s1G4target,nG4)
#   ppS1G8 ~ dmulti(s1G8target,nG8)
  #######
  # R2
  ######
  # G1
  for (i in 1:4){
    for (j in 1:3){
      r2tmpG1[i,j] <- s1G1[j,i] * salienceG1[j]
    }
  }
  for (i in 1:4){
    r2G1[i,] <- r2tmpG1[i,] / sum(r2tmpG1[i,]) 
  }
  r2G1target <- r2G1[2,]
  # G2
  for (i in 1:4){
    for (j in 1:3){
      r2tmpG2[i,j] <- s1G2[j,i] * salienceG2[j]
    }
  }
  for (i in 1:4){
    r2G2[i,] <- r2tmpG2[i,] / sum(r2tmpG2[i,]) 
  }
  r2G2target <- r2G2[1,]
  # G3
  for (i in 1:4){
    for (j in 1:3){
      r2tmpG3[i,j] <- s1G3[j,i] * salienceG3[j]
    }
  }
  for (i in 1:4){
    r2G3[i,] <- r2tmpG3[i,] / sum(r2tmpG3[i,]) 
  }
  r2G3target <- r2G3[1,]
  # G4
  for (i in 1:4){
    for (j in 1:3){
      r2tmpG4[i,j] <- s1G4[j,i] * salienceG4[j]
    }
  }
  for (i in 1:4){
    r2G4[i,] <- r2tmpG4[i,] / sum(r2tmpG4[i,]) 
  }
  r2G4target <- r2G4[1,]
  # G8
#   for (i in 1:4){
#     for (j in 1:3){
#       r2tmpG8[i,j] <- s1G8[j,i] * salienceG8[j]
#     }
#   }
#   for (i in 1:4){
#     r2G8[i,] <- r2tmpG8[i,] / sum(r2tmpG8[i,]) 
#   }
#   r2G8target[1] <- r2G8[2,3]
#   r2G8target[2] <- r2G8[2,1]
#   r2G8target[3] <- r2G8[2,2]
  # match to observations
  obsR[1,] ~ dmulti(r2G1target,nG1)
  obsR[2,] ~ dmulti(r2G2target,nG2)
  obsR[3,] ~ dmulti(r2G3target,nG3)
  obsR[4,] ~ dmulti(r2G4target,nG4)
#   obsR[8,] ~ dmulti(r2G8target,nG8)
  # Posterior predictive
  ppR2G1 ~ dmulti(r2G1target,nG1)
  ppR2G2 ~ dmulti(r2G2target,nG2)
  ppR2G3 ~ dmulti(r2G3target,nG3)
  ppR2G4 ~ dmulti(r2G4target,nG4)
#   ppR2G8 ~ dmulti(r2G8target,nG8)
}