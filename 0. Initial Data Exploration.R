rm(list = ls())

### 0 - Initial Data Exploration


#Load Data
train = read.csv("~/GetyourGuide/train.csv")
View(train)

#Load required Packages
library(caret)
library(doParallel)
library(stringr)
library(dplyr)
if(!require("lubridate")) install.packages("lubridate"); library("lubridate") 
library(zoo)


#Data Structure
str(train)
summary(train$Revenue)
summary(train$Clicks)

#Revenue per Click i.e. RPC
train$rpc = train$Revenue/train$Clicks
summary(train_subset$rpc)


#Take a stratified sample of the data to create a subset
set.seed(123)
idx_1 = createDataPartition(y = train$rpc, p = 0.012, list = FALSE)
train = train[idx_1,]
#rm(train)
#rm(idx_1)

#Split into training and test sets to check variable importance
set.seed(124)
idx_2 = createDataPartition(y = train$rpc, p = 0.7, list = FALSE)
trainset = train[idx_2,]
testset = train[-idx_2,]


#Pre-Model clean trainset

trainset$Revenue = NULL
trainset$Conversions = NULL
trainset$Clicks = NULL

#Model using Parallel Processing
cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)

time.start <- Sys.time()
print(paste0("Model started at: ", time.start))

ols = caret::train(rpc~., method = "lm", data = trainset)

time.end <- Sys.time()
dur <- time.end-time.start
print( dur )

stopCluster(cl)

#Predictions
pred.ols = predict(ols, newdata = testset)
#Evaluate Prediction using the defnined error criterion
rpce(preds = pred.ols, obs = testset$Revenue, clicks = testset$Clicks)
#Variable Importance
varImp(ols, scale = FALSE)


## Data Exploration: ID Columns

trainset$Keyword_ID_100 = trainset$Keyword_ID/1000
trainset$Keyword_ID_last5 = str_sub(trainset$Keyword_ID, start = -6, end = -1)


#check levels (on 100k subset + 8.3M training set)
length(unique(train$Keyword_ID)) #47363, 487981
length(unique(train$Ad_group_ID)) #35323, 269480
length(unique(train$Campaign_ID)) #1517, 2927
length(unique(train$Account_ID)) #16, 16
length(unique(train$Device_ID)) #3,3
length(unique(train$Match_type_ID)) #3,3


sort(table(train$Date))
sum(train[train$year.month == "Jan 2015","Clicks"]) 
sum(train[train$Date == "2014-12-23","revenue"]) 
str(train)
date = unique(train$Date)


## Data Exploration: Dates

trainset$month = month(x = trainset$Date)
trainset$month = as.factor(trainset$month)
table(trainset$month, sum(trainset$rpc))
x = tbl_df(summarise(group_by(train, day.of.the.month), rpc = length(unique(train$Ad_group_ID))))
x

length(unique(train[train$Date == "2014-12-14", "Ad_group_ID"]))

### Graphs:

## 1.0 Revenue
#Revenue per Month (sum)
ggplot(data = train, 
       aes(year.month, Revenue)) + 
        stat_summary(fun.y = sum, geom = "bar")

#Revenue per Month (mean)
ggplot(data = train, 
       aes(year.month, Revenue)) + 
        stat_summary(fun.y = mean, geom = "bar") + labs(x = "Month", y = "Revenue (mean)")

#Revenue per DOTM (mean)
ggplot(data = train, 
       aes(day.of.the.month, Revenue)) + 
  stat_summary(fun.y = mean, geom = "bar") + labs(x = "DOTM", y = "Revenue (mean)")

#Revenue per DOTW (mean)
ggplot(data = train, 
       aes(weekday, Revenue)) + 
  stat_summary(fun.y = mean, geom = "bar") + labs(x = "Weekday", y = "Revenue (mean)")

#Revenue per Account (mean)
ggplot(data = train, 
       aes(Account_ID, Revenue)) + 
  stat_summary(fun.y = mean, geom = "bar") + labs(x = "Account", y = "Revenue (mean)")

#Revenue per Device (mean)
ggplot(data = train, 
       aes(Device_ID, Revenue)) + 
  stat_summary(fun.y = mean, geom = "bar") + labs(x = "Device", y = "Revenue (mean)")

#Revenue per Match (mean)
ggplot(data = train, 
       aes(Match_type_ID, Revenue)) + 
  stat_summary(fun.y = mean, geom = "bar") + labs(x = "Match ID", y = "Revenue (mean)")


##2.0 Clicks
#Clicks per Month (sum)
ggplot(data = train, 
        aes(year.month, Clicks)) + 
          stat_summary(fun.y = sum, geom = "bar")
#Clicks per Month (mean)  
ggplot(data = train, 
       aes(year.month, Clicks)) + 
  stat_summary(fun.y = mean, geom = "bar")

##3.0 RPC
#RPC per Month(sum)
#RPC per Month(mean)


##4.0 Bookings
ggplot(data = train, 
       aes(year.month, booking)) + 
  stat_summary(fun.y = mean, geom = "bar") + labs(x = "Month", y = "Number of Bookings (mean)")


##5.0 Conversions


ggplot(train[which(train$Revenue>0),], aes(x=Revenue)) + geom_histogram()
ggplot(train, aes(x=log.rpc)) + geom_histogram()














