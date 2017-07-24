


#descriptive stats

#a) Movers
movers = data.frame(table(migration_long_final$move))
colnames(movers) <- c("move", "count")
movers$percent <- round((movers$count/sum(movers$count))*100, digits=2)

#add columns by movers

female = tapply(migration_long_final$female, migration_long_final$move, mean)
female = data.frame(female)

income = tapply(migration_long_final$netinc, migration_long_final$move, mean)
income = data.frame(income)


age = tapply(migration_long_final$age, migration_long_final$move, mean)
age = data.frame(age)

educ = tapply(migration_long_final$bilzeit, migration_long_final$move, mean)
educ = data.frame(educ)

married = tapply(migration_long_final$married, migration_long_final$move, mean)
married = data.frame(married)


# Add it to the dataframe generated earlier with cbind(), which means bind by columns
movers = cbind(movers, female)
movers = cbind(movers, income)
movers = cbind(movers, age)
movers = cbind(movers, educ)
movers = cbind(movers, married)



#b) states

states = data.frame(table(migration_long_movers$bula))
colnames(states) = c("home_state", "count")
states$percent = round((states$count/sum(states$count))*100, digits=2)

move = tapply(migration_long_movers$move, migration_long_movers$bula, sum)
move = data.frame(move)

#idx_move = which(migration_long_movers$move == 1)
#mig = migration_long_movers[idx_move,]
#mig = write.csv(x = mig, file = "mig.csv")


female = tapply(migration_long_movers$female, migration_long_movers$bula, mean)
female = data.frame(female)

income = tapply(migration_long_movers$netinc, migration_long_movers$bula, mean)
income = data.frame(income)


age = tapply(migration_long_movers$age, migration_long_movers$bula, mean)
age = data.frame(age)

educ = tapply(migration_long_movers$bilzeit, migration_long_movers$bula, mean)
educ = data.frame(educ)

married = tapply(migration_long_movers$married, migration_long_movers$bula, mean)
married = data.frame(married)

risk = tapply(migration_long_movers$risk, migration_long_movers$bula, mean)
risk = data.frame(risk)

impat = tapply(migration_long_movers$impat, migration_long_movers$bula, mean)
impat = data.frame(impat)

LOC = tapply(migration_long_movers$LOC, migration_long_movers$bula, mean)
LOC = data.frame(LOC)


# Add it to the dataframe generated earlier with cbind(), which means bind by columns
states = cbind(states, move)
states = cbind(states, female)
states = cbind(states, income)
states = cbind(states, age)
states = cbind(states, educ)
states = cbind(states, married)
states = cbind(states, risk)
states = cbind(states, impat)
states = cbind(states, LOC)

write.csv(x = states, file = "desc_states_movers.csv")


#states[,10] = NULL


# 4. Merge with individual specific characteristics
# 5. Modeling

#ols
#colnames(migration_long)

# ols = lm(move ~ big5_itema1 +	big5_itema2 +	big5_itema3 +	big5_itemc1 +	big5_itemc2 +	
#           big5_itemc3 +	big5_iteme1 +	big5_iteme2 +	big5_iteme3 +	big5_itemn1 +	
#           big5_itemn2 +	big5_itemn3 +	big5_itemo1 +	big5_itemo2 +	big5_itemo3 +	
#           bilzeit +	female +	gebjahr +	impat +	jobsat +	kids +	married +	
#           netincsat +	owner +	regunempl +	risk +	zimpat +	zLOC +	zrisk +
#           (Area.x-Area.y) + (Auslander.x-Auslander.y) + (GDP.x-GDP.y) +
#           (Patents.x-Patents.y) + (Price.Index.x-Price.Index.y) +
#           (Unemployed.Total.x-Unemployed.Total.y) +
#           (Unemployment.Rate.x-Unemployment.Rate.y), data = migration_long)


#summary(migration_long$sumkids)
#summary(migration_long$zimpat)


migration_long$any_move = NULL
migration_long$big5_itema1 = NULL
migration_long$big5_itema2 = NULL
migration_long$big5_itema3 = NULL
migration_long$big5_itemc1 = NULL
migration_long$big5_itemc2 = NULL
migration_long$big5_itemc3 = NULL
migration_long$big5_iteme1 = NULL
migration_long$big5_iteme2 = NULL
migration_long$big5_iteme3 = NULL
migration_long$big5_itemn1 = NULL
migration_long$big5_itemn2 = NULL
migration_long$big5_itemn3 = NULL
migration_long$big5_itemo1 = NULL
migration_long$big5_itemo2 = NULL
migration_long$big5_itemo3 = NULL
migration_long$big5_missing = NULL
migration_long$bilzeit_missing = NULL
migration_long$bula.2008 = NULL
migration_long$bula.2009 = NULL
migration_long$bula.2010 = NULL
migration_long$bula.2011 = NULL
migration_long$bula.2012 = NULL
migration_long$bula.2013 = NULL
migration_long$dest_dyad = NULL
migration_long$destination = NULL
migration_long$gebjahr = NULL
migration_long$gebmonat = NULL
migration_long$jobsat_mean = NULL
migration_long$jobsat_missing = NULL
migration_long$kids_missing = NULL
migration_long$loc1989 = NULL
migration_long$loc1989_c = NULL
migration_long$loc1989_missing = NULL
migration_long$loc89 = NULL
migration_long$married_missing = NULL
#migration_long$move5 = NULL
migration_long$netinc_missing = NULL
migration_long$netincsat_mean = NULL
migration_long$netincsat_missing = NULL
migration_long$ownership_rent = NULL
migration_long$sex = NULL
migration_long$sumkids = NULL
migration_long$summoves_5 = NULL
migration_long$unempl_missing = NULL
migration_long$willtomove_ = NULL
migration_long$wtm = NULL
migration_long$year = NULL
migration_long$hhnrunique = NULL
migration_long$unempl = NULL

apply(migration_long, 2, function(x) any(is.na(x)))
#zrisk
zrisk = scale(migration_long$risk)
migration_long$zrisk = as.vector(zrisk)
migration_long$zimpat = as.vector(scale(migration_long$impat))
migration_long$zLOC = as.vector(scale(migration_long$LOC))


#famstd
migration_long$famstd = factor(migration_long$famstd)
migration_long$famstd = addNA(migration_long$famstd)

#regunempl
migration_long$regunempl = factor(migration_long$regunempl)


### save dataset

saveRDS(object = migration_long, file = "migration_v3.rds")
#saveRDS(object = migration, file = "migration_v3.rds")

migration = readRDS("migration_v3.rds")

#correct/add indicators

#correct
migration$LOC_ind = ifelse(migration$LOC >= 4.89,1,0)
migration$risk_ind = ifelse(migration$risk >= 4.538,1,0)
migration$impat_ind = ifelse(migration$impat >= 6.059,1,0)

#add
migration$big5_item_o_ind = ifelse(migration$big5_item_o >= 4.566,1,0)
migration$big5_item_c_ind = ifelse(migration$big5_item_c >= 5.909,1,0)
migration$big5_item_e_ind = ifelse(migration$big5_item_e >= 4.874,1,0)
migration$big5_item_a_ind = ifelse(migration$big5_item_a >= 5.427,1,0)
migration$big5_item_n_ind = ifelse(migration$big5_item_n >= 3.943,1,0)


#correct standardized values


migration$zrisk = as.vector(scale(migration$risk))
migration$zimpat = as.vector(scale(migration$impat))
migration$zLOC = as.vector(scale(migration$LOC))
migration$big5_item_o_z1 = as.vector(scale(migration$big5_item_o))
migration$big5_item_c_z1 = as.vector(scale(migration$big5_item_c))
migration$big5_item_e_z1 = as.vector(scale(migration$big5_item_e))
migration$big5_item_a_z1 = as.vector(scale(migration$big5_item_a))
migration$big5_item_n_z1 = as.vector(scale(migration$big5_item_n))




saveRDS(object = migration, file = "migration_v3.rds")

library(foreign)
write.dta(dataframe = migration, file = "migration.dta")
write.csv(x = migration, file = "mig.csv")


