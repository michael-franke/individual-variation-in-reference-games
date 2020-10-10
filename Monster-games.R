# just a test game
scalar = matrix(c(1.0,0.0, 1/2,
                  1.0,1.0, 1/2,
                  0.0,0.0, 2),nrow=3,byrow=T)
rownames(scalar) = c('t-some', 't-all','costs')
colnames(scalar) = c('m-some', 'm-all', 'priors')

#####################################

simple = matrix(c(0.0,1.0,0.0,0.0, 1/3,
                  1.0,1.0,0.0,0.0, 1/3,
                  0.0,0.0,1.0,1.0, 1/3,
                  0.0,0.0,0.0,0.0, 3),nrow=4,byrow=T)

rownames(simple) = c('tt', 'tc', 'td','costs')
colnames(simple) = c('mt', 'mc', 'md1', 'md2', 'priors')

complex = matrix(c(1.0,1.0,0.0,0.0, 1/3,
                   1.0,0.0,1.0,0.0, 1/3,
                   0.0,1.0,0.0,0.0, 1/3,
                   0.0,0.0,0.0,0.0, 3),nrow=4,byrow=T)

rownames(complex) = c('tt', 'tc', 'td','costs')
colnames(complex) = c('mt', 'mc', 'md1', 'md2', 'priors')

unamb <- matrix(c(1, 0, 0, 0, 1/3,
                  0, 1, 1, 0, 1/3,
                  0, 0, 0, 1, 1/3,
                  0, 0, 0, 0, 1/3),
                nrow=4,byrow=T)

rownames(unamb) = c('tt', 'tc', 'td','costs')
colnames(unamb) = c('mt', 'mc', 'md1', 'md2', 'priors')

amb <- matrix(c(1, 1, 0, 0, 1/3,
                1, 1, 0, 0, 1/3,
                0, 0, 1, 1, 1/3,
                0, 0, 0, 0, 1/3),
              nrow=4,byrow=T)

rownames(amb) = c('tt', 'tc', 'td','costs')
colnames(amb) = c('mt', 'mc', 'md1', 'md2', 'priors')

free = matrix(c(1.0, 0.0, 0.0, 0.0, 1.0, 1/3,
                0.0, 1.0, 0.0, 0.0, 1.0, 1/3,
                0.0, 0.0, 1.0, 1.0, 0.0, 1/3,                
                0.0, 0.0, 0.0, 0.0, 0.0, 3),nrow=4,byrow=T)

rownames(free) = c('Robot-red', 'M1-red', 'M2-blue','costs')
colnames(free) = c('robot' , 'M1', 'M2', 'blue', 'red', 'priors')

# costs depend on Lambda, so we fix these dynamically
costlier = free
costliest = free
