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


length(unique(trainset$Keyword_ID)) #47363
length(unique(trainset$Ad_group_ID)) #35323
length(unique(trainset$Campaign_ID)) #1517
length(unique(trainset$Account_ID)) #16
length(unique(trainset$Device_ID)) #3
length(unique(trainset$Match_type_ID)) #3


sort(table(train$Date))
sum(train[train$year.month == "Jan 2015","Clicks"]) 
sum(train[train$Date == "2014-12-23","revenue"]) 
str(train)
date = unique(train$Date)


## Data Exploration: Dates

trainset$month = month(x = trainset$Date)
trainset$month = as.factor(trainset$month)
table(trainset$month, sum(trainset$rpc))
x = tbl_df(summarise(group_by(trainset, month), rpc = mean(Clicks))) 
x



### Graphs:

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



#Clicks per Month (sum)
ggplot(data = train, 
        aes(year.month, Clicks)) + 
          stat_summary(fun.y = sum, geom = "bar")
#Clicks per Month (mean)  
ggplot(data = train, 
       aes(year.month, Clicks)) + 
  stat_summary(fun.y = mean, geom = "bar")


#RPC per Month(sum)
#RPC per Month(mean)








scale_x_date(
    labels = date_format("%Y-%m"),
    breaks = "1 month") # custom x-axis labels


## Data Prep
# Outlier treatment for revenue













