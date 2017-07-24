#LASSO

library(glmnet)

y = as.matrix(migration_long$move5) 
x = migration_long[,c(5:21,35:47,67:85)]
x$dest_dyad = NULL
x$zrisk = NULL


migration_long$age = 2008 - migration_long$gebjahr
migration_long$log_income = log(1 + migration_long$netinc)

#covarites already standardized
cleaned_cov = c("female", "married","zimpat",
                "zLOC","zrisk","big5_item_c",
                "big5_item_e","big5_item_a", 
                "big5_item_o", "big5_item_n")

x_cleaned = migration_long[,cleaned_cov]

#covariates that need to be standardized
standarize_covariates = c("bilzeit", "age", "log_income")
x_std = migration_long[,standarize_covariates] 
x_std = scale(x_std)


#covariates need to be one shot coded (converted to dummies)
bula = migration_long$bula
bula_dummy = model.matrix(~bula, bula, contrasts.arg = list(bula=contrasts(bula, contrasts = F)))
bula_dummy = bula_dummy[,-1]


x = cbind(x_cleaned, x_std, bula_dummy)


x = as.matrix(x)
#check structure
str(x)
#check nas
colnames(x)[colSums(is.na(x)) > 0]


fit = glmnet(x, y)
plot(fit, label = TRUE)
print(fit)



coef(fit, s=4.405e-03)



cvfit = cv.glmnet(x, y)
plot(cvfit)
cvfit$lambda.min
coef(cvfit, s = "lambda.1se")
print(cvfit)

