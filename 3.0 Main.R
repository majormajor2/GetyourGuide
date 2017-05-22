

######################## INTRODUCTION #################################
# This is the file that runs the main code.                           #
# It is segmented into logical steps.                                 #
# All functions are stored in other scripts, of which                 #
# the most important one is helper.R.                                 #
#######################################################################

source("1.0 Helper.R")

#Load Datasets
train = read_csv("~/R/GetyourGuide/train.csv")
prediction = read_csv("~/R/GetyourGuide/prediction.csv")

#Data Setup
train = data_setup(train)
prediction = data_setup(prediction)

#Time Features
train = time_features(train)
prediction = time_features(train)

#Data Exploration
par(mar=c(1,1,1,1)) # to make sure the plot works on a small screen
# Barplots 
# Corrplots


#Apply WOE to factors with high number of levels
train = calculate_woe(train)
train = apply_woe(train)

#check if there are new levels in Prediction and set them to 0
train_factors = train_subset[,c(2:7)]
prediction.new.levels = check_new_levels(train = train_factors, prediction = prediction)

#26189 new levels in Keywords
#17995 new levels in Ad_Groups
#33 new levels in Campaigns

key_missing = as.vector(prediction.new.levels$Keyword_ID)
ad_missig = as.vector(prediction.new.levels$Ad_group_ID)
campaign_missng = as.vector(prediction.new.levels$Campaign_ID)

#index new levels
new_levels_index = which(prediction$Keyword_ID %in% key_missing 
                         | prediction$Ad_group_ID %in% ad_missig 
                         | prediction$Campaign_ID %in% campaign_missng)

#set new levels to zero
prediction.levels.zero = prediction[new_levels_index,]
prediction.levels.zero$Keyword_ID = 0
prediction.levels.zero$Ad_group_ID = 0
prediction.levels.zero$Campaign_ID = 0

#remove indexed rows from prediction
prediction.no.new.levels = prediction[-new_levels_index,]
#remove new factor levels
prediction.no.new.levels = remove_new_levels(prediction.no.new.levels)
#check new levels
check_new_levels(train = train_factors, prediction = prediction.no.new.levels) #no new levels


#merge the two dfs to create a new prediction dataframe
prediction_woe = rbind(prediction.levels.zero, prediction.no.new.levels)
saveRDS(object = prediction_woe, file = "prediction_woe.rds")


#Apply WOE to factors with high number of levels i.e. Keyword, Ad Group and Campaign
woe_columns = c("Keyword_ID","Ad_group_ID","Campaign_ID", "booking")
train_woe = train_subset[,woe_columns]
train = calculate_woe(train = train_woe)
prediction = apply_woe(prediction_woe)

#Predicted Features
train = predicted_features(train)
prediction = predicted_features(prediction)

#K-Nearest Neighbors
prediction$dist = get_dist(trainSet = train, predSet = prediction, k = 5)
train$dist = get_dist(trainSet = train, predSet = train, k = 5)

#Feature Importance











