
migration = readRDS(file = "migration_v2.RDS")

migration = migration[,c(1:83,102)]
#state data
state_data = read.csv(file = "state_data.csv")
state_data$Number = NULL
state_data$Bundesland = NULL
state_data$X = NULL

#merge destination level state data
migration = merge(x = migration, y = state_data, by = "value")
#merge home state data
colnames(state_data)[1] = "bula"
migration = merge(x = migration, y = state_data, by = "bula")

#migration_movers = merge(x = migration_movers, y = gdp, by = "bula")
#migration_movers$percent_gdp = migration_movers$bula.gdp / migration_movers$GDP.EUR.Billions

dist = read.csv(file = "Dist.csv")
dist$Home_State = NULL
dist$Destination_State = NULL
migration = merge(x = migration, y = dist, by = c("bula", "value"))
#migration = merge(x = migration, y = big5, by = "persnr")

#clean dataset
colnames(migration)

migration$any_move = NULL
migration$big5_itema1 = NULL
migration$big5_itema2 = NULL
migration$big5_itema3 = NULL
migration$big5_itemc1 = NULL
migration$big5_itemc2 = NULL
migration$big5_itemc3 = NULL
migration$big5_iteme1 = NULL
migration$big5_iteme2 = NULL
migration$big5_iteme3 = NULL
migration$big5_itemn1 = NULL
migration$big5_itemn2 = NULL
migration$big5_itemn3 = NULL
migration$big5_itemo1 = NULL
migration$big5_itemo2 = NULL
migration$big5_itemo3 = NULL
migration$big5_missing = NULL
migration$bilzeit_missing = NULL
migration$bula.2008 = NULL
migration$bula.2009 = NULL
migration$bula.2010 = NULL
migration$bula.2011 = NULL
migration$bula.2012 = NULL
migration$bula.2013 = NULL
migration$dest_dyad = NULL
migration$destination = NULL
migration$gebjahr = NULL
migration$gebmonat = NULL
migration$jobsat_mean = NULL
migration$jobsat_missing = NULL
migration$kids_missing = NULL
migration$loc1989 = NULL
migration$loc1989_c = NULL
migration$loc1989_missing = NULL
migration$loc89 = NULL
migration$married_missing = NULL
#migration$move5 = NULL
migration$netinc_missing = NULL
migration$netincsat_mean = NULL
migration$netincsat_missing = NULL
migration$ownership_rent = NULL
migration$sex = NULL
migration$sumkids = NULL
migration$summoves_5 = NULL
migration$unempl_missing = NULL
migration$willtomove_ = NULL
migration$wtm = NULL
migration$year = NULL
migration$hhnrunique = NULL
migration$unempl = NULL
