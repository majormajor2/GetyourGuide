
##Feature Engineering
# Basic Features 
# Start with factorizing Accounts, Devices and Matches
# Interaction terms:Account-Device, Account-Match, Match-Device, Account-Match-Device
# Replace: Keywords, Ad_group and Campaign with 1) WOE 2) Prob of success (variations by time frame)
# Seasonality Analysis: Month, Week, Day of the Week, Day of the Month
# Predicted Features: Click, Conversion, Revenue, Booking



#0.1 Preprocessing
#Transform Target variable
train$log.rpc = log(1 + train$rpc)
#can be detransformed using: exp(train$log.rpc)-1

#0.2 Basic Features

# lineID

train$lineID = row.names(train)


# Price Paid
# Assumption: Each Keyword - Account - Device etc combnation refers to a unique product and so 
# dividing revenue by conversions gives us average price of a unique product for that day

train$price.paid = 0
train$price.paid = ifelse(train$Revenue == 0, 0, train$Revenue/train$Conversions)

#Booking 
train$booking = ifelse(train$Revenue>0, 1,0)
train$booking = as.factor(train$booking)



#1.0 Factorizing Accounts, Devices & Matches etc
train$Account_ID = as.factor(train$Account_ID)
train$Device_ID = as.factor(train$Device_ID)
train$Match_type_ID = as.factor(train$Match_type_ID)
train$Keyword_ID = as.factor(train$Keyword_ID)
train$Ad_group_ID = as.factor(train$Ad_group_ID)
train$Campaign_ID = as.factor(train$Campaign_ID)

prediction$Account_ID = as.factor(prediction$Account_ID)
prediction$Device_ID = as.factor(prediction$Device_ID)
prediction$Match_type_ID = as.factor(prediction$Match_type_ID)
prediction$Keyword_ID = as.factor(prediction$Keyword_ID)
prediction$Ad_group_ID = as.factor(prediction$Ad_group_ID)
prediction$Campaign_ID = as.factor(prediction$Campaign_ID)

#2.0 Interaction Terms
#Create interaction terms
train$Account_Device = paste(train$Account_ID, train$Device_ID)
train$Account_Match = paste(train$Account_ID, train$Match_type_ID)
train$Match_Device = paste(train$Match_type_ID, train$Device_ID)
train$Account_Match_Device = paste(train$Account_ID, train$Match_type_ID, train$Device_ID)


#Factorize
train$Account_Device = as.factor(train$Account_Device)
train$Account_Match = as.factor(train$Account_Match)
train$Match_Device = as.factor(train$Match_Device)
train$Account_Match_Device = as.factor(train$Account_Match_Device)


#3.0 Replace high factor levels

#Check if there are any new factor levels in the Prediction set

train_factors = train[,c(2:7)]
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
check_new_levels(train = train_factors, prediction = prediction.no.new.levels)


#merge the two dfs to create a new prediction dataframe
prediction_woe = rbind(prediction.levels.zero, prediction.no.new.levels)
saveRDS(object = prediction_woe, file = "prediction_woe.rds")


#Apply WOE to factors with high number of levels i.e. Keyword, Ad Group and Campaign
train = calculate_woe(train = train)


#4.0 Time Features

#Dates
train$Date = as.Date(train$Date)

#Dates: Month
train$month = month(x = train$Date)
train$month = as.factor(train$month)

#Year-Month
train$year.month = format(as.Date(train$Date), "%Y-%m")
train$year.month = as.factor(train$year.month)

#Day of the Week
train$weekday = wday(train$Date)
train$weekday = as.factor(train$weekday)

#Day of the Month
train$day.of.the.month = day(x = train$Date)
train$day.of.the.month = as.factor(train$day.of.the.month)


#5.0 Predicted Features

#booking
#Clicks
#Revenue
#Conversions



#6.0 Distance




