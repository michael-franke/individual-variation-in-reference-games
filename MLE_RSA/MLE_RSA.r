require('ggplot2')

source('rsa.r')
source('iqr-helpers.r')
source('Monster-data.R')
source('Monster-games.R')
source('helpers/helpers.R')

# data to explain
obsR = c10.obs
obsS = p10.obs
observationsPropL = as.vector(prop.table(obsR,1))
observationsPropS = as.vector(prop.table(obsS,1))

# salience priors 
salienceG1 = salience_E10[1,]
salienceG2 = salience_E10[2,]
salienceG3 = salience_E10[3,]
salienceG4 = salience_E10[4,]

get.predictionsL = function(lambda, epsilon) {
  matrix(c(rsa(game = simple, lambda = lambda, prior = salienceG1, epsilon = epsilon)[[2]][2,],
            rsa(game = complex, lambda = lambda, prior = salienceG2, epsilon = epsilon)[[2]][1,],
            rsa(game = unamb, lambda = lambda, prior = salienceG3, epsilon = epsilon)[[2]][1,],
            rsa(game = amb, lambda = lambda, prior = salienceG4, epsilon = epsilon)[[2]][1,]),
           byrow = T, nrow = 4)
}
get.predictionsS = function(lambda, epsilon) {
  matrix(c(rsa(game = simple, lambda = lambda, prior = salienceG1, epsilon = epsilon)[[1]][2,],
           rsa(game = complex, lambda = lambda, prior = salienceG2, epsilon = epsilon)[[1]][1,],
           rsa(game = unamb, lambda = lambda, prior = salienceG3, epsilon = epsilon)[[1]][1,],
           rsa(game = amb, lambda = lambda, prior = salienceG4, epsilon = epsilon)[[1]][1,]),
         byrow = T, nrow = 4)
}



nLLL = function(par) {
  lambda = par[1]
  epsilon = par[2]
  if (lambda < 0 | epsilon <=0 ) {
    return(NA)
  }
  pred = get.predictionsL(lambda, epsilon)
#   - sum(sapply(1:4, function(i) dmultinom(obsR[i,], prob = pred[i,], log = TRUE)))
  sum((observationsPropL - as.vector(pred))^2)
}
nLLS = function(par) {
  lambda = par[1]
  epsilon = par[2]
  if (lambda < 0 | epsilon <=0 ) {
    return(NA)
  }
  pred = get.predictionsS(lambda, epsilon)
#   - sum(sapply(1:4, function(i) dmultinom(obsS[i,], prob = pred[i,], log = TRUE)))
  sum((observationsPropS - as.vector(pred))^2)
}

# listener fit
oL = optim(par = c(lambda = 1, epsilon = 0.001), fn = nLLL)
lambdaMLEL = oL$par[1]
epsilonMLEL = oL$par[2]
predMLEL = as.vector(get.predictionsL(lambdaMLEL, epsilonMLEL)) 

# speaker fit
oS = optim(par = c(lambda = 1, epsilon = 0.001), fn = nLLS)
lambdaMLES = oS$par[1]
epsilonMLES = oS$par[2]
predMLES = as.vector(get.predictionsS(lambdaMLES, epsilonMLES)) 

# plotting 
data = data.frame(prediction = c(predMLEL,predMLES),
                  observation = c(as.vector(observationsPropL), as.vector(observationsPropS)),
                  role = c(rep("listener", 12), rep("speaker", 16)) )
p = ggplot(data, aes(x = prediction, y = observation, color = role, shape = role)) + geom_point(size = 3) +
    geom_abline(slope = 1, intercept = 0, colour = "darkgray") + theme_bw() + scale_color_grey(start = 0.1, end = 0.6) 
show(p)
ggsave(filename = "../text/plos1_monsterpaper/pics/RSA_population-fit.pdf", height = 4, width = 6)
show(cor.test(data$prediction, data$observation))
