

######################## INTRODUCTION #################################
# This is the file that runs the main code.                           #
# It is segmented into logical steps.                                 #
# All functions are stored in other scripts, of which                 #
# the most important one is helper.R.                                 #
#######################################################################

source("1.0 Helper.R")

#Load Datasets
train = read.csv("train.csv")
#prediction = read.csv("~/R/GetyourGuide/prediction.csv")

#Data Setup
train = data_setup(train)
#prediction = data_setup(prediction)

#Time Features
train = time_features(train)
#prediction = time_features(prediction)

#Data Exploration
par(mar=c(1,1,1,1)) # to make sure the plot works on a small screen
# Barplots 
# Corrplots


#Apply WOE to factors with high number of levels
#train = calculate_woe(train)
#train = apply_woe(train)

#file is too big for my computer

#check if there are new levels in Prediction and set them to 0
#train_factors = train[,c(2:7)]
#prediction.new.levels = check_new_levels(train = train_factors, prediction = prediction)

#26189 new levels in Keywords
#17995 new levels in Ad_Groups
#33 new levels in Campaigns

#key_missing = as.vector(prediction.new.levels$Keyword_ID)
#ad_missig = as.vector(prediction.new.levels$Ad_group_ID)
#campaign_missng = as.vector(prediction.new.levels$Campaign_ID)

#index new levels
#new_levels_index = which(prediction$Keyword_ID %in% key_missing 
                         #| prediction$Ad_group_ID %in% ad_missig 
                         #| prediction$Campaign_ID %in% campaign_missng)

#set new levels to zero
#prediction.levels.zero = prediction[new_levels_index,]
#prediction.levels.zero$Keyword_ID = 0
#prediction.levels.zero$Ad_group_ID = 0
#prediction.levels.zero$Campaign_ID = 0

#remove indexed rows from prediction
#prediction.no.new.levels = prediction[-new_levels_index,]
#remove new factor levels
#prediction.no.new.levels = remove_new_levels(prediction.no.new.levels)
#check new levels
#check_new_levels(train = train_factors, prediction = prediction.no.new.levels) #no new levels


#merge the two dfs to create a new prediction dataframe
#prediction_woe = rbind(prediction.levels.zero, prediction.no.new.levels)
#saveRDS(object = prediction_woe, file = "prediction_woe.rds")


#Apply WOE to factors with high number of levels i.e. Keyword, Ad Group and Campaign
#woe_columns = c("Keyword_ID","Ad_group_ID","Campaign_ID", "booking")
#train_woe = train_subset[,woe_columns]
#train = calculate_woe(train = train_woe)
#prediction = apply_woe(prediction_woe)

#Predicted Features
#revenue

#split dataset
set.seed(124)
idx_2 = createDataPartition(y = train$rpc, p = 0.7, list = FALSE)
trainset = train[idx_2,]
testset = train[-idx_2,]

#clean trainset
trainset$rpc = NULL
trainset$booking = NULL
trainset$Clicks = NULL
trainset$Conversions = NULL


#Enable parallel processing
#cl <- makeCluster(detectCores()-1)
#registerDoParallel(cl)
#time.start <- Sys.time()
#print(paste0("Model started at: ", time.start))

#ols = caret::train(Revenue~., method = "lm", data = trainset)

#time.end <- Sys.time()
#dur <- time.end-time.start
#print( dur )
#stopCluster(cl)

#Create feature for testset and Prediction

#testset$Revenue_feature = predict(ols, newdata = testset)
#prediction$Revenue_feature = predict(ols, newdata = prediction)


#K-Nearest Neighbors
#testset_dist = testset[,-c(17:19)]

#prediction$dist = get_dist(trainSet = testset, predSet = prediction, k = 5)
#testset$dist = get_dist(trainSet = testset_dist, predSet = testset_dist, k = 5)

#Feature Importance

#remove unwanted variables
#testset$Revenue = NULL
#testset$Clicks = NULL
#testset$rpc = NULL
#testset$Conversions = NULL
#testset$booking = NULL
#testset$dist = NULL

#Run Linear Model
#cl <- makeCluster(detectCores()-1)
#registerDoParallel(cl)
#time.start <- Sys.time()
#print(paste0("Model started at: ", time.start))

#ols = caret::train(log.rpc~., method = "lm", data = testset)

#time.end <- Sys.time()
#dur <- time.end-time.start
#print( dur )
#stopCluster(cl)



#Which variables are important?
#library(caret)
#plot(varImp(ols, scale = FALSE))



#Which can we drop?
# calculate correlation matrix
# exclude main variables
#testset_temp = testset
# find variables that are strongly correlated
#strongly_correlated_variables = strongly_correlated(dataset = testset_temp, threshold = 0.75)
# exclude strongly correlated variables
#trainset_cor <- trainset
#trainset_cor[,strongly_correlated_variables] <- NULL
#rm(ls = trainset_temp)
#rm(ls = strongly_correlated_variables)
#rm(ls = strongly_correlated)
