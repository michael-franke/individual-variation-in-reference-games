# RSA speaker model
var p[4,4], ptmp[4,4], ptmpt[4,4], k[nconditions,4], u[4,4], costVec[4];
model{
  cost ~ dunif(-cMax/2,cMax/2)
  costPrior ~ dunif(-cMax/2,cMax/2)
  lambda ~ dunif(0,lMax)
  lambdaPrior ~ dunif(0,lMax)
  epsilon ~ dunif(0,eMax)
  epsilonPrior ~ dunif(0,eMax)
  for (i in 1:4){
    for (j in 1:4){
      ptmp[i,j] <- exp( lambda * (u[i,j]   - cost * costVec[j]  ))
    }
  }
  for (i in 1:4){
    ptmpt[i,] <- ptmp[i,] / sum(ptmp[i,])
    for (j in 1:4){
      p[i,j] <- (1-4*epsilon) * ptmpt[i,j] + epsilon
    }
  }
  k[1,] ~ dmulti(p[1,],n[1])
  k[2,] ~ dmulti(p[2,],n[2])
  k[3,] ~ dmulti(p[3,],n[3])
  k[4,] ~ dmulti(p[4,],n[4])
  postpred1 ~ dmulti(p[1,],n[1])
  postpred2 ~ dmulti(p[2,],n[2])
  postpred3 ~ dmulti(p[3,],n[3])
  postpred4 ~ dmulti(p[4,],n[4])
}