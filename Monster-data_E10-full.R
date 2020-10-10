require('plyr')

# helper function adapted here for Exp 10

get.succ = function(data){  
  succ = matrix(0,nrow = 2, ncol=13)
  rownames(succ) = dimnames(data)[[1]][c(1:2)]
  colnames(succ) = 0:12
  for (i in 1:2) {
    succ[i,] = table(factor(data[i,1,],levels=0:12))
  }
  return(t(succ))
}

####################
#### Production ####
####################

prod = read.csv("data/production-full.tsv", sep = "\t")
# exclude subjects who scored badly

# aggregate data

p10.obs = with(prod, table(redimptype, response))[c(3,2,4,1), c(4,1,2,3)]

## by-subject data

p10.bs.obs = table(prod$redimptype,  
                   prod$workerid, 
                   prod$response)[c(3,2,4,1),,c(4,1,2,3)]
p10.bs.obs.readable = table(prod$redimptype,  
                            prod$response, 
                            prod$workerid)[c(3,2,4,1),c(4,1,2,3),]

succS10 = get.succ(p10.bs.obs.readable)

#######################
#### Comprehension ####
#######################

# comp = read.csv("/Users/titlis/cogsci/projects/xprag.de/RatReasRef/code/data/comprehension_E10.csv", sep="\t")
comp = read.csv("data/comprehension-full.tsv", sep = "\t")

# aggregate data

c10.obs = with(comp, table(redimptype, response))[c(3,2,4,1), c(3,1,2)]

## by-subject data

c10.bs.obs = table(comp$redimptype,  
                   comp$workerid, 
                   comp$response)[c(3,2,4,1),,c(3,1,2)]
c10.bs.obs.readable = table(comp$redimptype,  
                            comp$response, 
                            comp$workerid)[c(3,2,4,1),c(3,1,2),]

succ10 = get.succ(c10.bs.obs.readable)

#################
#### Summary ####
#################

obsTarget_E10 = matrix(c(prop.table(p10.obs,1)[1:2,1],
                         prop.table(c10.obs,1)[1:2,1]),
                       nrow = 2, byrow = TRUE)
colnames(obsTarget_E10) = c("simple", "complex")
rownames(obsTarget_E10) = c("speaker", "listener")