require('ggplot2')
require('reshape2')

source('Monster-games.R')
source('rsa.r')
source('iqr-helpers.r')

# wrapper functions that give me the predictions of the types

## receiver

predictions.R0 = Vectorize(function(lambda = 1) {
  return(c(simple = R0(simple, lambda)[2,1], 
           complex = R0(complex,lambda)[1,1]))
})

predictions.R1 = Vectorize(function(lambda = 1) {
  return(c(simple = R1(simple, lambda)[2,1], 
           complex = R1(complex,lambda)[1,1]))
})

predictions.R2 = Vectorize(function(lambda = 1) {
  simple = rsa(game = simple, lambda = lambda)$receiver[2,1]
  complex = rsa(game = complex, lambda = lambda )$receiver[1,1]
  return(c(simple = simple, 
           complex = complex))
})

## sender

predictions.S0 = Vectorize(function(lambda = 1) {
  return(c(simple = S0(simple, lambda)[2,1], 
           complex = S0(complex,lambda)[1,1]))
})

predictions.S1 = Vectorize(function(lambda = 1) {
  simple = rsa(game = simple, lambda = lambda)$sender[2,1]
  complex = rsa(game = complex, lambda = lambda )$sender[1,1]
  return(c(simple = simple, 
           complex = complex))
})

predictions.S2 = Vectorize(function(lambda = 1) {
  return(c(simple = S2(simple, lambda)[2,1], 
           complex = S2(complex,lambda, complex.adjust = T)[1,1]))
})


lambdas = c(0.00000001, seq(0.5, 10, by = 0.5))
nL = length(lambdas)

preds = t(cbind(predictions.R0(lambdas),
          predictions.R1(lambdas),
          predictions.R2(lambdas),
          predictions.S0(lambdas),
          predictions.S1(lambdas),
          predictions.S2(lambdas)))

data = data.frame(type = rep(c("R0", "R1", "R2", "S0", "S1", "S2"), each = nL),
                  lambda = rep(lambdas, times = 6),
                  simple = preds[,1],
                  complex = preds[,2])

data = melt(data, id.vars = c("type", "lambda"))
data$condition = data$variable
data$type = factor(data$type, levels = c("S0", "S1", "S2", "R0", "R1", "R2"))

p = ggplot(data, aes(x = lambda, y = value)) + 
  ylab("target choice probability") +
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks=c(1, 3, 6, 9)) +
  facet_grid(condition ~ type)
show(p)
ggsave(filename = "../text/monsterpaper/graphs/targetPredsPerType.pdf", plot = p, width = 7, height = 4.5)
