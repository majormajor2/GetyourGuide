
##Feature Engineering
# Basic Features 
# Start with factorizing Accounts, Devices and Matches
# Interaction terms:Account-Device, Account-Match, Match-Device, Account-Match-Device
# Replace: Keywords, Ad_group and Campaign with 1) WOE 2) Prob of success (variations by time frame)
# Seasonality Analysis: Month, Week, Day of the Week, Day of the Month
# Predicted Features: Click, Conversion, Revenue, Booking


#0. Basic Features

# Price Paid
# Assumption: Each Keyword - Account - Device etc combnation refers to a unique product and so 
# dividing revenue by conversions gives us average price of a unique product for that day

train$price.paid = 0
train$price.paid = ifelse(train$Revenue == 0, 0, train$Revenue/train$Conversions)

#Order 
train$booking = ifelse(train$Revenue>0, 1,0)

#Dates
train$Date = as.Date(train$Date)

#Dates: Month
train$month = month(x = train$Date)
train$month = as.factor(train$month)

#Year-Month
train$year.month = as.yearmon(train$Date, format = "%y%m%d")
train$year.month = as.factor(train$year.month)

#Day of the Week
train$weekday = wday(train$Date)
train$weekday = as.factor(train$weekday)

#Day of the Month
train$day.of.the.month = day(x = train$Date)
train$day.of.the.month = as.factor(train$day.of.the.month)

#1.0 Factorizing Accounts, Devices & Matches etc
train$Account_ID = as.factor(train$Account_ID)
train$Device_ID = as.factor(train$Device_ID)
train$Match_type_ID = as.factor(train$Match_type_ID)
train$Keyword_ID = as.factor(train$Keyword_ID)
train$Ad_group_ID = as.factor(train$Ad_group_ID)
train$Campaign_ID = as.factor(train$Campaign_ID)


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

#4.0 Seasonal/Time Features

#5.0 Predicted Features







