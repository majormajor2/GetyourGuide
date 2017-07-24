rm(list = ls())

mig_full_dyad = readRDS("mig_full_dyad.rds")

dep_var = c("persnr","value","move")

dist_matrix = c("persnr","value","distance")
neighbor_matrix = c("persnr","value","neighbor")
east_matrix = c("persnr","value","east_east_")


#create dep var matrix
move = mig_full_dyad[,dep_var]
move = reshape(data = move, idvar = "persnr", timevar = "value", direction = "wide")
move$persnr= NULL
move5 = rowSums(x = move, na.rm = T)
move$m5 = as.vector(move5)

library(caret)
set.seed(555)
idx_sna = createDataPartition(y = move$m5, p = 0.01, list = FALSE)
move = move[idx_sna,]
move$m5 = NULL
move = as.matrix(move)

#create indep vars matrix
dist = mig_full_dyad[,dist_matrix]
dist = reshape(data = dist, idvar = "persnr", timevar = "value", direction = "wide")
dist = dist[idx_sna,]
dist$persnr = NULL
dist = as.matrix(dist)

neighbor = mig_full_dyad[,neighbor_matrix]
neighbor = reshape(data = neighbor, idvar = "persnr", timevar = "value", direction = "wide")
neighbor = neighbor[idx_sna,]
neighbor$persnr = NULL
neighbor = as.matrix(neighbor)

east = mig_full_dyad[,east_matrix]
east = reshape(data = east, idvar = "persnr", timevar = "value", direction = "wide")
east = east[idx_sna,]
east$persnr = NULL
east = as.matrix(east)

#create array - not needed because I'm using a list instead
#indeps = array(data = c(dist, neighbor, east), dim = c(3,8398,16))

#str(indeps)



#####

#########################################################################
##
## Code to allow QAP regressions in 'sna' package to analyse rectangular matrices
## ----------------------------------------------------------------
## written by Matthew Gwynfryn Thomas
## released under a CC Attribution ShareAlike license (http://creativecommons.org/licenses/by-sa/4.0/)
##
#########################################################################

## By default, netlogit and netlm assume adjacency matrices will be square
## The code below will allow for asymmetric matrices
##
## see: http://stats.stackexchange.com/questions/114728/why-might-ucinet-and-r-return-different-results-for-a-qap-logistic-regression-o/114831?iemail=1&noredirect=1#114831

## Need to change code in the sna package function 'rmperm'
## these three lines replace lines 13-15 in rmperm
#o1 <- sample(1:dim(m)[2])
#o2 <- sample(1:dim(m)[3])
#p[i, , ] <- array(m[i, o1, o2])
##

# this will be the replacement function
rmperm.2 <- function (m) 
{
  m <- as.sociomatrix.sna(m)
  if (is.list(m)) 
    return(lapply(m, rmperm))
  if (length(dim(m)) == 2) {
    o <- sample(1:dim(m)[1])
    p <- matrix(data = m[o, o], nrow = dim(m)[1], ncol = dim(m)[2])
  }
  else {
    p <- array(dim = c(dim(m)[1], dim(m)[2], dim(m)[3]))
    for (i in 1:dim(m)[1]) {
      o1 <- sample(1:dim(m)[2])
      o2 <- sample(1:dim(m)[3])
      p[i, , ] <- array(m[i, o1, o2])
    }
  }
  p
}

require(sna)

# this block of code taken from http://stackoverflow.com/a/23280442
tmpfun <- get("rmperm", envir = asNamespace("sna"))
environment(rmperm.2) <- environment(tmpfun)
attributes(rmperm.2) <- attributes(tmpfun)  # don't know if this is really needed
assignInNamespace("rmperm", rmperm.2, ns="sna")

# check the changes have been made
sna::rmperm

#####
library(sna)

## try creating a 16*16 random sample...



dyadic_regression = netlm(y = move, x = list(dist, east, neighbor), nullhyp = "qap")
summary(dyadic_regression)

std_coef <- function (model) {
  sigma2 <- sum(model$residuals ^ 2) / model$df.residual
  Rinv <- backsolve(model$qr$qr, diag(model$rank))
  sqrt(rowSums(Rinv ^ 2) * sigma2)
}


std_coef(dyadic_regression)







