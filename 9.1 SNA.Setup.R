rm(list = ls())

mig_full_dyad = readRDS("mig_full_dyad.rds")

dep_var = c("persnr","value","move")
#create dep var matrix
move = mig_full_dyad[,dep_var]
move = reshape(data = move, idvar = "persnr", timevar = "value", direction = "wide")
move$persnr= NULL
move5 = rowSums(x = move, na.rm = T)
move$m5 = as.vector(move5)

library(caret)
set.seed(555)
idx_sna = createDataPartition(y = move$m5, p = 0.1, list = FALSE)
move = move[idx_sna,]
move$m5 = NULL
move = as.matrix(move)


#independent variables
#create differences
mig_full_dyad$price_index = mig_full_dyad$price_index.x - mig_full_dyad$price_index.y
mig_full_dyad$area_km = mig_full_dyad$area_km.x - mig_full_dyad$area_km.y
mig_full_dyad$patents = mig_full_dyad$patents.x - mig_full_dyad$patents.y
mig_full_dyad$auslander = mig_full_dyad$auslander.x - mig_full_dyad$auslander.y
mig_full_dyad$sun_hours = mig_full_dyad$sun_hours.x - mig_full_dyad$sun_hours.y
mig_full_dyad$gdp_billion = mig_full_dyad$gdp_billion.x - mig_full_dyad$gdp_billion.y
mig_full_dyad$catholics_share = mig_full_dyad$catholics_share.x - mig_full_dyad$catholics_share.y
mig_full_dyad$population_sq_km = mig_full_dyad$population_sq_km.x - mig_full_dyad$population_sq_km.y
mig_full_dyad$unemployed_percent = mig_full_dyad$unemployed_percent.x - mig_full_dyad$unemployed_percent.y

#create indices
dist_matrix = c("persnr","value","distance")
neighbor_matrix = c("persnr","value","neighbor")
east_matrix = c("persnr","value","east_east_")
price_idx = c("persnr","value","price_index")
area_idx = c("persnr","value","area_km")
patents_idx = c("persnr","value","patents")
auslander_idx = c("persnr","value","auslander")
sun_idx = c("persnr","value","sun_hours")
gdp_idx = c("persnr","value","gdp_billion")
catholics_idx = c("persnr","value","catholics_share")
pop_idx = c("persnr","value","population_sq_km")
unemployed_idx = c("persnr","value","unemployed_percent")
age_idx = c("persnr","value","age")
bilzeit_idx = c("persnr","value","bilzeit")
married_idx = c("persnr","value","married")
kids_idx = c("persnr","value","kids")
owner_idx = c("persnr","value","owner")
zrisk_idx = c("persnr","value","zrisk")
zLOC_idx = c("persnr","value","zLOC")
zimpat_idx = c("persnr","value","zimpat")
big5_item_o_z1_idx = c("persnr","value","big5_item_o_z1")
big5_item_c_z1_idx = c("persnr","value","big5_item_c_z1")
big5_item_e_z1_idx = c("persnr","value","big5_item_e_z1")
big5_item_a_z1_idx = c("persnr","value","big5_item_a_z1")
big5_item_n_z1_idx = c("persnr","value","big5_item_n_z1")


idx_list = c("dist_matrix","neighbor_matrix","east_matrix",
             "price_idx","area_idx","patents_idx","auslander_idx",
             "sun_idx","gdp_idx","catholics_idx","catholics_idx",
             "pop_idx","unemployed_idx", "age_idx",	"bilzeit_idx",
             "married_idx",	"kids_idx",	"owner_idx",	"zrisk_idx",	
             "zLOC_idx",	"zimpat_idx",	"big5_item_o_z1_idx",	
             "big5_item_c_z1_idx",	"big5_item_e_z1_idx",	"big5_item_a_z1_idx",
             "big5_item_n_z1_idx")


#create indep vars matrix
#dist = mig_full_dyad[,dist_matrix]
#dist = reshape(data = dist, idvar = "persnr", timevar = "value", direction = "wide")
#dist = dist[idx_sna,]
#dist$persnr = NULL
#dist = as.matrix(dist)

#neighbor = mig_full_dyad[,neighbor_matrix]
#neighbor = reshape(data = neighbor, idvar = "persnr", timevar = "value", direction = "wide")
#neighbor = neighbor[idx_sna,]
#neighbor$persnr = NULL
#neighbor = as.matrix(neighbor)

#east = mig_full_dyad[,east_matrix]
#east = reshape(data = east, idvar = "persnr", timevar = "value", direction = "wide")
#east = east[idx_sna,]
#east$persnr = NULL
#east = as.matrix(east)

matrix_maker = function(x, idx_x){
  x = mig_full_dyad[,idx_x]
  x = reshape(data = x, idvar = "persnr", timevar = "value", direction = "wide")
  x = x[idx_sna,]
  x$persnr = NULL
  x = as.matrix(x)
}

dist = matrix_maker(x = dist, idx_x = dist_matrix)
east = matrix_maker(x = east, idx_x = east_matrix)
neighbor = matrix_maker(x = neighbor, idx_x = neighbor_matrix)
area = matrix_maker(x = area, idx_x = area_idx)
patents = matrix_maker(x = patents, idx_x = patents_idx)  
auslander = matrix_maker(x = auslander, idx_x = auslander_idx)
sun = matrix_maker(x = sun, idx_x = sun_idx)
gdp = matrix_maker(x = gdp, idx_x = gdp_idx)
catholics = matrix_maker(x = catholics, idx_x = catholics_idx)
pop = matrix_maker(x = pop, idx_x = pop_idx)
unemployed = matrix_maker(x = unemployed, idx_x = unemployed_idx)
age = matrix_maker(x = age, idx_x = age_idx)
bilzeit = matrix_maker(x = bilzeit, idx_x = bilzeit_idx)
married = matrix_maker(x = married, idx_x = married_idx)
kids = matrix_maker(x = kids, idx_x = kids_idx)
owner = matrix_maker(x = owner, idx_x = owner_idx)
zrisk = matrix_maker(x = zrisk, idx_x = zrisk_idx)
zLOC = matrix_maker(x = zLOC, idx_x = zLOC_idx)
zimpat = matrix_maker(x = zimpat, idx_x = zimpat_idx)
big5_item_o_z1 = matrix_maker(x = big5_item_o_z1, idx_x = big5_item_o_z1_idx)
big5_item_c_z1 = matrix_maker(x = big5_item_c_z1, idx_x = big5_item_c_z1_idx)
big5_item_e_z1 = matrix_maker(x = big5_item_e_z1, idx_x = big5_item_e_z1_idx)
big5_item_a_z1 = matrix_maker(x = big5_item_a_z1, idx_x = big5_item_a_z1_idx)
big5_item_n_z1 = matrix_maker(x = big5_item_n_z1, idx_x = big5_item_n_z1_idx)

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
#library(sna)

## try creating a 16*16 random sample...



dyadic_regression = netlm(y = move, x = list(dist,	east,	neighbor,	area,	patents,	auslander,	sun,	gdp,	catholics,	pop,	unemployed,	age,	bilzeit,	married,	kids,	owner,	zrisk,	zLOC,	zimpat,	big5_item_o_z1,	big5_item_c_z1,	big5_item_e_z1,	big5_item_a_z1,	big5_item_n_z1), nullhyp = "qap")
summary(dyadic_regression)

std_coef <- function (model) {
  sigma2 <- sum(model$residuals ^ 2) / model$df.residual
  Rinv <- backsolve(model$qr$qr, diag(model$rank))
  sqrt(rowSums(Rinv ^ 2) * sigma2)
}


std_coef(dyadic_regression)

dyadic_regression$coefficients





