
## Move variable is calculated by comparing home-state and destination state
## in 2013, however in some cases individuals that moved, moved back to their
## home-state. Correction done using Excel.

#orignial checks
#checks = c("persnr","move5","move","bula","bula.2009","bula.2010","bula.2011","bula.2012","bula.2013","value","destination")

#new 7.22

checks = c("persnr","bula","value", "move","move5")

check = migration[,checks]
idx_moves = which(check$move5 == 1)

#check in excel
#write.csv(x = check, file = "check.csv")


check_1 = check[-idx_moves,]
sum(check_1$move5) #0
sum(check_1$move) #342 wtf..
check_1$move = 0
sum(check_1$move) #0 :)

check_2 = read.csv(file = "check.csv")
sum(check_2$move5) #5490
sum(check_2$move) #366 :)

check_2 = check_2[,checks]

check_corrected = rbind(check_1, check_2)


sum(check$move) #332: missing migrants that moved back to initial home-state
sum(check_corrected$move) #336: correct


migration$move = NULL
check_corrected$move5 = NULL

migration = merge(x = migration,y = check_corrected,by = c("persnr","bula","value"))


summary(migration$move5) #0.04094
summary(migration$move) #0.00273



rm(check)
rm(check_1)
rm(check_2)
rm(check_corrected)


