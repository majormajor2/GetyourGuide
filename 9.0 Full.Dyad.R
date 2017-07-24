rm(list = ls())


library(readstata13)


migration = readstata13::read.dta13("cleaned_cov_1.dta")
idx_2008 = which(migration$year == "2008")
move = c("persnr","year", "bula" )
move = migration[,move]

idx_bad_years = which(move$year<2008)
move = move[-idx_bad_years,]
migration = migration[idx_2008,]


move = reshape(move, idvar = "persnr", timevar = "year", direction = "wide")

#how many are missing
sum(is.na(move$bula.2008)) #5732
sum(is.na(move$bula.2009)) #0
sum(is.na(move$bula.2010)) #0
sum(is.na(move$bula.2011)) #0
sum(is.na(move$bula.2012)) #0
sum(is.na(move$bula.2013)) #0

#imput using 2009 values
move$bula.2008[is.na(move$bula.2008)] = as.character(move$bula.2009[is.na(move$bula.2008)])

#how many are missing
sum(is.na(move$bula.2008))#0

#clean non-responses
move$bula.2008 = factor(move$bula.2008)
move$bula.2009 = factor(move$bula.2009)
move$bula.2010 = factor(move$bula.2010)
move$bula.2011 = factor(move$bula.2011)
move$bula.2012 = factor(move$bula.2012)
move$bula.2013 = factor(move$bula.2013)
migration$bula = factor(migration$bula)

#any move between 2008 and 2013
move$any_move = ifelse(move$bula.2008 == move$bula.2013, 0, 1)
move$destination = ifelse(move$any_move >0, as.character(move$bula.2013), "no_move")
#move$home_state = move$bula.2008



#merge back with migration
migration = merge(x = migration, y = move, by = "persnr")

#use wide to long formulation to set-up dyads

#set-up columns for each state

migration$dest_01 = "[1] Schleswig-Holstein"
migration$dest_02 = "[2] Hamburg"
migration$dest_03 = "[3] Niedersachsen"
migration$dest_04 = "[4] Bremen"
migration$dest_05 = "[5] Nordrhein-Westfalen"
migration$dest_06 = "[6] Hessen" 
migration$dest_07 = "[7] Rheinland-Pfalz"
migration$dest_08 = "[8] Baden-Wuerttemberg"
migration$dest_09 = "[9] Bayern"
migration$dest_10 = "[10] Saarland"
migration$dest_11 = "[11] Berlin"
migration$dest_12 = "[12] Brandenburg"
migration$dest_13 = "[13] Mecklenburg-Vorpommern"
migration$dest_14 = "[14] Sachsen"
migration$dest_15 = "[15] Sachsen-Anhalt"
migration$dest_16 = "[16] Thueringen"



#factorize
migration$dest_01 = as.factor(migration$dest_01)
migration$dest_02 = as.factor(migration$dest_02)
migration$dest_03 = as.factor(migration$dest_03)
migration$dest_04 = as.factor(migration$dest_04)
migration$dest_05 = as.factor(migration$dest_05)
migration$dest_06 = as.factor(migration$dest_06) 
migration$dest_07 = as.factor(migration$dest_07)
migration$dest_08 = as.factor(migration$dest_08)
migration$dest_09 = as.factor(migration$dest_09)
migration$dest_10 = as.factor(migration$dest_10)
migration$dest_11 = as.factor(migration$dest_11)
migration$dest_12 = as.factor(migration$dest_12)
migration$dest_13 = as.factor(migration$dest_13)
migration$dest_14 = as.factor(migration$dest_14)
migration$dest_15 = as.factor(migration$dest_15)
migration$dest_16 = as.factor(migration$dest_16)

#wide to long
#install.packages("tidyr")
#library(tidyr)
#migration_long = gather(data = migration,"dyad", dest_01:dest_16, factor_key = TRUE)
library(reshape2)

id_vars = c("persnr", "any_move",	"anymove_5_2005",	"anymove_5_2006",	"anymove_5_2007",	
            "anymove_5_2008",	"anymove_5_2009",	"anymove_5_2010",	"anymove_5_2011",	
            "anymove_5_2012",	"anymove_5_2013",	"anymove_5_2014",	"big5_itema1",	
            "big5_itema2",	"big5_itema3",	"big5_itemc1",	"big5_itemc2",	
            "big5_itemc3",	"big5_iteme1",	"big5_iteme2",	"big5_iteme3",	
            "big5_itemn1",	"big5_itemn2",	"big5_itemn3",	"big5_itemo1",	
            "big5_itemo2",	"big5_itemo3",	"big5_missing",	"bilzeit",	
            "bilzeit_female",	"bilzeit_male",	"bilzeit_missing",	"bula",	"bula.2008",	
            "bula.2009",	"bula.2010",	"bula.2011",	"bula.2012",	"bula.2013",	
            "destination",	"famstd",	"female",	"gebjahr",	"gebmonat",	"hhnr_",	
            "hhnrunique",	"hrf",	"impat",	"impat_ind",	"impat_ind2",	"impat_mean",	
            "impat_mode",	"impat_xtile",	"jobsat",	"jobsat_mean",	"jobsat_missing",	
            "kids",	"kids_missing",	"LOC",	"LOC_ind",	"LOC_ind2",	"loc_item1",	
            "loc_item10",	"loc_item2",	"loc_item3",	"loc_item5",	"loc_item6",	
            "loc_item7",	"loc_item8",	"LOC_mean",	"LOC_mode",	"LOC_xtile",	
            "loc1989",	"loc1989_missing",	"loc89",	"married",	
            "married_missing",	"move",	"move5",	"netinc",	"netinc_female",	
            "netinc_male",	"netinc_missing",	"netincsat",	
            "netto_",	"owner",	"ownership_rent",	
            "pop_",	"prgroup",	"psample",	"regunempl",	"risk",	"risk_ind",	
            "risk_ind2",	"risk_mean",	"risk_mode",	"risk_xtile",	"sex",	
            "sum_5_2005",	"sum_5_2006",	"sum_5_2007",	"sum_5_2008",	"sum_5_2009",	
            "sum_5_2010",	"sum_5_2011",	"sum_5_2012",	"sum_5_2013",	"sum_5_2014",	
            "sumkids",	"summoves_5",	"unempl",	"unempl_missing",	"willtomove_",	
            "year",	"zimpat",	"zLOC",	"zrisk")

#id variables missing: loc1989_c, netincsat_mean, netincsat_missing, wtm

migration_long = melt(data = migration, 
                      id.vars = id_vars, 
                      measure.vars = c("dest_01",	"dest_02",	"dest_03",
                                       "dest_04",	"dest_05",	"dest_06",	
                                       "dest_07",	"dest_08",	"dest_09",	
                                       "dest_10",	"dest_11",	"dest_12",	
                                       "dest_13",	"dest_14",	"dest_15",	
                                       "dest_16"),
                      variable.name = "dest_dyad")



#index obs where destination and home state are the same

#library(plyr)
#migration_long$bula = revalue(c("1"="[1] Schleswig-Holstein",	"2"="[2] Hamburg",	"3"="[3] Niedersachsen",	"4"="[4] Bremen",	"5"="[5] Nordrhein-Westfalen",	"6"="[6] Hessen",	"7"="[7] Rheinland-Pfalz",	"8"="[8] Baden-Wuerttemberg",	"9"="[9] Bayern",	"10"="[10] Saarland",	"11"="[11] Berlin",	"12"="[12] Brandenburg",	"13"="[13] Mecklenburg-Vorpommern",	"14"="[14] Sachsen",	"15"="[15] Sachsen-Anhalt",	"16"="[16] Thueringen"))


idx_redundant_dyad = which(migration_long$bula == migration_long$value)
#exclude indexed items
migration_long = migration_long[idx_redundant_dyad,]
#treat move variable to take the value 1 if the individual moved to that state and 0 otherwise
migration_long$move = ifelse(migration_long$destination == migration_long$value, 1,0)


migration_long$psample=NULL
migration_long$netto_=NULL
migration_long$hhnr_=NULL
migration_long$pop_=NULL
migration_long$prgroup=NULL
migration_long$hrf=NULL
migration_long$anymove_5_2005=NULL
migration_long$anymove_5_2006=NULL
migration_long$anymove_5_2007=NULL
migration_long$anymove_5_2008=NULL
migration_long$anymove_5_2009=NULL
migration_long$anymove_5_2010=NULL
migration_long$anymove_5_2011=NULL
migration_long$anymove_5_2012=NULL
migration_long$anymove_5_2013=NULL
migration_long$anymove_5_2014=NULL
migration_long$sum_5_2005=NULL
migration_long$sum_5_2006=NULL
migration_long$sum_5_2007=NULL
migration_long$sum_5_2008=NULL
migration_long$sum_5_2009=NULL
migration_long$sum_5_2010=NULL
migration_long$sum_5_2011=NULL
migration_long$sum_5_2012=NULL
migration_long$sum_5_2013=NULL
migration_long$sum_5_2014=NULL
migration_long$LOC_mode=NULL
#migration_long$LOC_ind=NULL
migration_long$LOC_mean=NULL
migration_long$LOC_ind2=NULL
migration_long$LOC_xtile=NULL
migration_long$risk_mode=NULL
#migration_long$risk_ind=NULL
migration_long$risk_mean=NULL
migration_long$risk_ind2=NULL
migration_long$risk_xtile=NULL
migration_long$impat_mode=NULL
#migration_long$impat_ind=NULL
migration_long$impat_mean=NULL
migration_long$impat_ind2=NULL
migration_long$impat_xtile=NULL
migration_long$bilzeit_male=NULL
migration_long$bilzeit_female=NULL
migration_long$loc_item8=NULL
migration_long$loc_item7=NULL
migration_long$loc_item6=NULL
migration_long$loc_item5=NULL
migration_long$loc_item3=NULL
migration_long$loc_item2=NULL
migration_long$loc_item10=NULL
migration_long$loc_item1=NULL
migration_long$netinc_female=NULL
migration_long$netinc_male=NULL

migration_long$value = as.factor(migration_long$value)

state_data = read.csv(file = "state_data.csv")
state_data$Number = NULL
state_data$Bundesland = NULL
state_data$X = NULL

#merge destination level state data
migration_long = merge(x = migration_long, y = state_data, by = "value")
#merge home state data
colnames(state_data)[1] = "bula"
migration_long = merge(x = migration_long, y = state_data, by = "bula")

dist = read.csv(file = "Dist.csv")
dist$Home_State = NULL
dist$Destination_State = NULL
migration_long = merge(x = migration_long, y = dist, by = c("bula", "value"), all.x = TRUE)


migration_long$age = 2008 - migration_long$gebjahr
migration_long$risk = ifelse(is.na(migration_long$risk), 5, migration_long$risk)


#delete covariates not used

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




#create new variables that are missing

migration_long$big5_item_o = NA
migration_long$big5_item_c = NA
migration_long$big5_item_e = NA
migration_long$big5_item_a = NA
migration_long$big5_item_n = NA

migration_long$big5_item_o_z1 = NA
migration_long$big5_item_c_z1 = NA
migration_long$big5_item_e_z1 = NA
migration_long$big5_item_a_z1 = NA
migration_long$big5_item_n_z1 = NA

migration_long$big5_item_o_ind = NA
migration_long$big5_item_c_ind = NA
migration_long$big5_item_e_ind = NA
migration_long$big5_item_a_ind = NA
migration_long$big5_item_n_ind = NA


rm(migration)
rm(dist)
rm(move)
rm(state_data)


migration = readRDS("migration_v3.rds")

setdiff(migration$persnr, migration_long$persnr)
#2923802

missing = which(migration$persnr == 2923802)
migration = migration[-missing,]

mig_full_dyad = rbind(migration, migration_long)


saveRDS(object = mig_full_dyad, file = "mig_full_dyad.rds")
library(foreign)
write.dta(dataframe = mig_full_dyad, file = "mig_full_dyad.dta")
mig = readstata13::read.dta13("migration.dta")


#### adjust NA's 


mig_full_dyad = readRDS("mig_full_dyad.rds")
idx_redundant_dyad = which(mig_full_dyad$bula == mig_full_dyad$value)

mig_na = mig_full_dyad[idx_redundant_dyad,]
mig_full_dyad = mig_full_dyad[-idx_redundant_dyad,]

save_ids = c("persnr", "bula","value","move5")

mig_na_ids = mig_na[,save_ids]
mig_na_na = mig_na[,c(4:13,15:61)]
mig_na_na[!is.na(mig_na_na)] = NA

mig_na = cbind(mig_na_ids, mig_na_na)

mig_full_dyad = rbind(mig_full_dyad, mig_na)
saveRDS(object = mig_full_dyad, file = "mig_full_dyad.rds")
library(foreign)
#v2 includes non-NA move5 so we know which dyad-iD has an individual moving
write.dta(dataframe = mig_full_dyad, file = "mig_full_dyad_v2.dta")

rm(mig_na)
rm(mig_na_ids)
rm(mig_na_na)
rm(migration_long)
rm(migration)
rm(mig_check)


#mig_check = migration[migration$persnr==8029101,]
#mig_check_2 = migration[migration$persnr==8029101,]


