rm(list = ls())
migration = readRDS("migration_v3.rds")

#1.0 Split the dyadic data so there's only one obs per person
#2.0 Create interactions / new variables
#3.0 Run Lasso + OLS and select most important variables
#4.0 Run Lasso + Logistic Regression and select variables
#5.0 Run Elastic Net


#1.0
mig_lasso = subset(migration, !duplicated(persnr))
rm(migration)

#1.1 Cleanup
mig_lasso$regunempl = factor(mig_lasso$regunempl)
mig_lasso$regunempl = as.numeric(mig_lasso$regunempl)-1
mig_lasso$regunemployed = ifelse(mig_lasso$regunempl <1,1,0)
mig_lasso$regunempl = NULL
mig_lasso$regunemployed = factor(mig_lasso$regunemployed, labels = c("no","yes"))
mig_lasso$female = factor(mig_lasso$female, labels = c("no","yes"))
mig_lasso$impat_ind = factor(mig_lasso$impat_ind, labels = c("no","yes"))
mig_lasso$LOC_ind = factor(mig_lasso$LOC_ind, labels = c("no","yes"))
mig_lasso$risk_ind = factor(mig_lasso$risk_ind, labels = c("no","yes"))
mig_lasso$kids = factor(mig_lasso$kids,labels =c("no","yes"))
mig_lasso$married = factor(mig_lasso$married,labels = c("no","yes"))
mig_lasso$owner = factor(mig_lasso$owner,labels = c("no","yes"))
mig_lasso$big5_item_o_ind = factor(mig_lasso$big5_item_o_ind, labels = c("no","yes"))
mig_lasso$big5_item_c_ind = factor(mig_lasso$big5_item_c_ind, labels = c("no","yes"))
mig_lasso$big5_item_e_ind = factor(mig_lasso$big5_item_e_ind, labels = c("no","yes"))
mig_lasso$big5_item_a_ind = factor(mig_lasso$big5_item_a_ind, labels = c("no","yes"))
mig_lasso$big5_item_n_ind = factor(mig_lasso$big5_item_n_ind, labels = c("no","yes"))





mig_lasso$famstd = factor(mig_lasso$famstd)
#idx_na = which(is.na(mig_lasso$famstd))
mig_lasso$famstd[is.na(mig_lasso$famstd)] = "[1] married, living togehter"
mig_lasso$famstd = factor(mig_lasso$famstd)



#2.0
mig_lasso$age_sq = (mig_lasso$age)^2
mig_lasso$log_income = log(1+mig_lasso$netinc)
mig_lasso$age_risk = mig_lasso$age*mig_lasso$risk
mig_lasso$east = ifelse(test = as.numeric(mig_lasso$bula) >= 12, 1,0)
#mig_lasso$east = factor(mig_lasso$east, labels = c("no","yes"))

#2.1 Save dataset for micro-level regressions

#library(foreign)
#write.dta(dataframe = mig_lasso, file = "migration_micro.dta")


#3.0 
#3.1all
covariates = c("age",	"age_risk",	"age_sq",	"area_km.y",	
               "auslander.y",	"big5_item_a",	"big5_item_a_ind",	
               "big5_item_c",	"big5_item_c_ind",	"big5_item_e",	
               "big5_item_e_ind",	"big5_item_n",	"big5_item_n_ind",	
               "big5_item_o",	"big5_item_o_ind",	"bilzeit",	"bula",	
               "catholics_share.y",	"female","famstd",	"gdp_billion.y",
               "impat",	"impat_ind",	"jobsat",	"kids",	"LOC",	"LOC_ind",
               "log_income",	"married",	"netincsat",	"owner",	"patents.y",	
               "population_sq_km.y",	"price_index.y",	"regunemployed",	"risk",	
               "risk_ind",	"sun_hours.y",	"unemployed_percent.y",	"east")

#3.2 all individual level
covariates_2 = c("age",	"age_sq",	"age_risk","big5_item_a",	
               "big5_item_c",	"big5_item_e",	"big5_item_n",	
               "big5_item_o",	"bilzeit",	"bula",	"female",
               "famstd","impat",	"jobsat",	"kids",	"LOC",
               "log_income","married","netincsat","owner",	
                "regunemployed",	"risk","east")

#3.3 w/o behaviroal variables
covariates_3 = c("age",	"age_sq",	"bilzeit",	"bula",	"female",
                 "famstd",	"jobsat",	"kids",
                 "log_income","married","netincsat","owner",	
                 "regunemployed","east")






#state level
cov_state = c("gdp_billion.y","patents.y","population_sq_km.y","price_index.y","area_km.y",
              "auslander.y","sun_hours.y",	"unemployed_percent.y","catholics_share.y")

#3.1
#x = mig_lasso[,covariates]
#str(x)
#int = sapply(x, is.integer)
#integers_exclude = c("auslander.y","impat","patents.y","population_sq_km.y","sun_hours.y")
#ints = as.matrix(x[,int])
#x$auslander.y = NULL
#x$impat = NULL
#x$patents.y = NULL
#x$population_sq_km.y = NULL
#x$sun_hours.y = NULL

#ints[,1] = as.numeric(ints[,1])
#ints[,2] = as.numeric(ints[,2])
#ints[,3] = as.numeric(ints[,3])
#ints[,4] = as.numeric(ints[,4])
#ints[,5] = as.numeric(ints[,5])

#ints = as.data.frame(ints)
#x = cbind(x, ints)

#colnames(x)[colSums(is.na(x)) > 0]
#x = model.matrix(~.-1,data = x, contrasts.arg = lapply(x[,sapply(x, is.factor)], contrasts, contrasts = FALSE))

#x = as.data.frame(x)
#str(mig_lasso$move5)


#famstd
#mig_lasso$famstd = factor(mig_lasso$famstd)
#x$famstd = NULL
#x = as.data.frame(x)
#x_1 = as.matrix(x[,1:20])
#x_2 = as.matrix(x[,21:40])
#x_3 = as.matrix(x[,1:48,50:68])

#it was log_income
#summary(x$log_income)

library(glmnet)
#fit = glmnet(x, y = mig_lasso$move5)
#plot(fit, label = TRUE)
#print(fit)


validate = seq(from = 0.1, to = 1, by = 0.1)
#cvfit = cv.glmnet(x = x, y = mig_lasso$move5, alpha = validate, family = "binomial", type.measure = "mae")


#plot(cvfit)
#coef(cvfit, s = "lambda.min")[which(coef(cvfit, s = "lambda.1se") != 0)]
#coef(cvfit, s = "lambda.1se")



#x2- including multiple versions of the same variable creates problems for lasso
x2 = mig_lasso[,covariates_2]
x2 = model.matrix(~.-1,data = x2, contrasts.arg = lapply(x2[,sapply(x2, is.factor)], contrasts, contrasts = FALSE))
#validate = seq(from = 0.1, to = 1, by = 0.1)

###### X2 WITH LASSO

fit.x2.lasso = glmnet(x2, y=mig_lasso$move5)
plot(fit.x2.lasso, label = TRUE)
cvfit.x2.lass = cv.glmnet(x2,y = mig_lasso$move5)
plot(cvfit.x2.lass)
coef(cvfit.x2.lass, s = "lambda.min")

#fit = glmnet(x2, y = mig_lasso$move5,family = "binomial")
#cvfit = cv.glmnet(x = x2, y = mig_lasso$move5, alpha = validate, family = "binomial",type.measure = "deviance")
plot(cvfit)
coef(cvfit, s = "lambda.1se")



###### x2 LASSO ASSUMING BINOMIAL DISTRIBUTION
fit.x2.lasso.bin = glmnet(x2, y=mig_lasso$move5, family = "binomial")
plot(fit.x2.lasso.bin, label = TRUE)
cvfit.x2.lass.bin = cv.glmnet(x2,y = mig_lasso$move5, family = "binomial", type.measure = "mse")
plot(cvfit.x2.lass.bin)
coef(cvfit.x2.lass.bin, s = "lambda.1se")


###### X2: Elastic Net

validate = seq(from = 0.1, to = 1, by = 0.1)
fit.x2.elastic = glmnet(x2, y=mig_lasso$move5, alpha = 0.5)
plot(fit.x2.elastic, label = TRUE)
cvfit.x2.elastic = cv.glmnet(x2,y = mig_lasso$move5, alpha = validate)
plot(cvfit.x2.elastic)
coef(cvfit.x2.elastic, s = "lambda.1se")

###### X2: Elastic Net with Binomial Distribution

fit.x2.elastic.bin = glmnet(x2, y=mig_lasso$move5, alpha = 0.5, family = "binomial")
plot(fit.x2.elastic.bin, label = TRUE)
cvfit.x2.elastic.bin = cv.glmnet(x2,y = mig_lasso$move5, alpha = validate, family = "binomial", type.measure = "mse")
plot(cvfit.x2.elastic.bin)
coef(cvfit.x2.elastic.bin, s = "lambda.1se")



#x3
x3 = mig_lasso[,covariates_3]
x3 = model.matrix(~.-1,data = x3, contrasts.arg = lapply(x3[,sapply(x3, is.factor)], contrasts, contrasts = FALSE))
#validate = seq(from = 0.1, to = 1, by = 0.1)

#x3 LASSO
fit.x3.lasso = glmnet(x3, y=mig_lasso$move5)
plot(fit.x3.lasso, label = TRUE)
cvfit.x3.lass = cv.glmnet(x3,y = mig_lasso$move5)
plot(cvfit.x3.lass)
coef(cvfit.x3.lass, s = "lambda.1se")

#x3 Elastic with Binomial
fit.x3.elastic.bin = glmnet(x3, y = mig_lasso$move5,family = "binomial", alpha = 0.5)
plot(fit.x3.elastic.bin, label = TRUE)
foldid=sample(1:10,size=length(mig_lasso$move5),replace=TRUE)
cvfit.x3.elastic.bin = cv.glmnet(x = x3,foldid = foldid, alpha = validate, family = "binomial", y = mig_lasso$move5,type.measure = "mse")
plot(cvfit.x3.elastic.bin)
coef(cvfit.x3.elastic.bin, s = "lambda.1se")
#print(cvfit)

