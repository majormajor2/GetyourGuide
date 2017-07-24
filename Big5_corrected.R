
migration = readstata13::read.dta13("cleaned_cov.dta")
idx_2008 = which(migration$year == "2008")
migration = migration[idx_2008,]


#select big five + persnr

big5 =  c(1,118:127)

migration = migration[,big5]


#merge with dataset
migration_long = readRDS(file = "migration_v1.RDS")


migration_long = merge(x = migration_long, y = migration, by = "persnr")
