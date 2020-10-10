library(reshape2)
library(ggplot2)
library(np) # for npudens
library(polspline) # for logspline
require('gtools') # for dirichlet distribution
require('LCA') # for dirichlet distribution ALTERNATIVE

x = c(10,15,15) # true concentration vector for Dirichlet

# get samples
P = rdirichlet(10000, x)
c = sapply(1:10000, function(i) sample(1:3, size = 50, replace = T,prob = P[i,]))

# get counts / new weights
counts = data.frame(t(unlist(apply(MARGIN = 2, X = c, FUN = function(j) table(factor(j, levels = c(1,2,3)))))) + 1)
mean.weights = sapply(1:3, function(i) mean(counts[,i]))

ddirichlet(c(0.1, 0.8, 0.1), alpha = c(1,1,1))

tests = rdirichlet(10, c(1,1,1))
show(ddirichlet(tests, x))
show(ddirichlet(tests, mean.weights))
