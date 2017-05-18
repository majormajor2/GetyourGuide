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


#Data Structure
str(train)
summary(train$Revenue)
summary(train$Clicks)

#Revenue per Click i.e. RPC
train_$rpc = train$Revenue/train$Clicks
summary(train_subset$rpc)


#Take a stratified sample of the data to create a subset
set.seed(123)
idx_1 = createDataPartition(y = train$rpc, p = 0.012, list = FALSE)
train_subset = train[idx_1,]
#rm(train)
#rm(idx_1)

#Split into training and test sets to check variable importance
set.seed(124)
idx_2 = createDataPartition(y = train_subset$rpc, p = 0.7, list = FALSE)
trainset = train_subset[idx_2,]
testset = train_subset[-idx_2,]


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
sum(train[train$Date == "2014-12-23","Clicks"]) 
sum(train[train$Date == "2014-12-23","revenue"]) 
str(train)
date = unique(train$Date)


## Data Exploration: Dates

trainset$month = month(x = trainset$Date)
trainset$month = as.factor(trainset$month)
table(trainset$month, sum(trainset$rpc))
x = tbl_df(summarise(group_by(trainset, month), rpc = mean(Clicks))) 
x

## Data Prep
# Outlier treatment for revenue
##FE
# Start with factorizing Accounts, Devices and Matches
# Interaction terms:Account-Device, Account-Match, Match-Device, Account-Match-Device
# Replace: Keywords, Ad_group and Campaign with 1) WOE 2) Prob of success (variations by time frame)
# Seasonality Analysis: Month, Week, Day of the Week, Day of the Month
# Predicted Features: Click, Conversion, Revenue













