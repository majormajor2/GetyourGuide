library(caret)
library(doParallel)
library(stringr)
library(dplyr)
if(!require("lubridate")) install.packages("lubridate"); library("lubridate") 
library(zoo)
if(!require("tikzDevice")) install.packages("tikzDevice"); library("tikzDevice") 
require(tikzDevice)

#Define performance measure
performance_measure = function (data, lev = NULL, model =NULL){
  
  click_sq_error= data$Clicks*(data$obs - data$pred)^2
  click_sum = sum(data$Clicks)
  rpce = sum(click_sq_error, na.rm= TRUE)/click_sum
  
  out = c(rpce)
  names(out) = c("RPCE")
  out
}

#simpler error function
rpce = function(preds, obs, clicks) {
  rpce = sum(clicks*((obs - preds)^2))/sum(clicks)
  return(rpce)
}



## This function calculates and returns the distance of every row in the data set to the k closest cases that were bookings ##
## Gives additional information on the similarity of every case with other successful (booking) cases ##
## Three return values currently implemented (can be expanded easily), to be specified in "type" argument:
##      1. "average" : average of all distances to k specified neighbors
##      2. "sum"     : sum of the distances to k specified neighbors
##      3. "all"     : returns k columns with every single distance to k closest order cases

## Should be used after all preparation and static feature engineering is finished to incorporate as much info as possible
## 1.) Apply on train set itself (train = predSet) to create the feature
## 2.) Apply on prediction set from train to create the features on final Prediction set 
## 3.) Try different values of k

if(!require("FNN")) install.packages("FNN"); library("FNN")

get_dist = function(trainSet, predSet, k, type = "average"){
  
  
  trainSet_idx_numeric = sapply(trainSet, is.numeric)  # filter numerical variables
  trainSet_numeric = trainSet[,trainSet_idx_numeric]
  trainSet_numeric = as.data.frame(lapply(trainSet_numeric, standardize1))
  trainSet_numeric$booking = trainSet$booking            # attach booking variable (since dependent)
  
  trainSet_numeric = trainSet_numeric[,-which(colnames(trainSet_numeric) == c("rpc","log.rpc","Clicks","Conversions","Revenue"))]  # take out since not in Prediction
  
  if(!identical(trainSet, predSet)){
    predSet_idx_numeric = sapply(predSet, is.numeric)  # filter numerical variables
    predSet_numeric = predSet[,predSet_idx_numeric]
    predSet_numeric = as.data.frame(lapply(predSet_numeric, standardize1))
    
    if(any(colnames(predSet) == "booking")){
      predSet_numeric$booking = predSet$booking  
    }
    
    if(any(colnames(predSet) == "rpc")){
      predSet_numeric = predSet_numeric[,-which(colnames(predSet_numeric) == "rpc")]
    }
    if(any(colnames(predSet) == "log.rpc")){
      predSet_numeric = predSet_numeric[,-which(colnames(predSet_numeric) == "log.rpc")]
    }
    if(any(colnames(predSet) == "Revenue")){
      predSet_numeric = predSet_numeric[,-which(colnames(predSet_numeric) == "Revenue")]
    }
    if(any(colnames(predSet) == "Clicks")){
      predSet_numeric = predSet_numeric[,-which(colnames(predSet_numeric) == "Clicks")]
    }
    if(any(colnames(predSet) == "Conversions")){
      predSet_numeric = predSet_numeric[,-which(colnames(predSet_numeric) == "Conversions")]
    }
    
  }else{
    predSet_numeric = trainSet_numeric
  }
  
  par.nn = k
  
  print("start modelling")
  
  nn.list = get.knnx(data = trainSet_numeric[which(trainSet_numeric$booking == 1),-c(which(colnames(trainSet_numeric) == "booking"))], query = predSet_numeric[,-c(which(colnames(predSet_numeric) == "booking"))], k = par.nn)
  
  ### depending on "type" calculate the required distance(s) and return
  
  if(type == "average"){
    avgDistance = apply(as.data.frame(nn.list[2]), 1, function(x){mean(x)})
    return(avgDistance)
  }
  
  if(type == "all"){
    distances = as.data.frame(nn.list[2])
    return(distances)
  }
  
  if(type == "sum"){
    sumDistance = apply(as.data.frame(nn.list[2]), 1, function(x){sum(x)})
    return(sumDistance)
  }
  
}

### Standardization function used above ###

standardize1 <- function(x){
  
  my <- mean(x)
  std <- sd(x)
  result <- (x-my)/std
  return(result)
}


### Checks to see if there are new factor levels in the Prediction set
check_new_levels = function(train, prediction, target = "booking")
{
  new_levels = list() # create an empty list that will hold the new levels
  
  for(column in colnames(train)) # loops over all columns
  {
    if(is.factor(train[,column]) && !column == target) # checks if the column is a factor and if it is not the target variable
    {
      if(length(setdiff(levels(prediction[,column]),levels(train[,column]))) != 0) # checks if there are new factor levels
      {
        # before the 2nd loop: create a temporary vector to hold new levels in the column
        temp = vector()
        for(level in setdiff(levels(prediction[,column]),levels(train[,column]))) # loops through new factor levels
        {
          if(level %in% levels(prediction[,column]))
          {
            #print(column)
            #print(level) 
            temp = append(temp, level) # append the level to the temporary vector
          }
        }
        # after the 2nd loop: add the filled temporary vector to the list of new levels
        new_levels[column] = list(temp) 
      }
    }
  }
  return(new_levels)
}

## Weight of Evidence
calculate_woe = function(train, target = "booking", columns_to_replace = c("Keyword_ID","Ad_group_ID","Campaign_ID"))
{
  woe_object = woe(as.formula(paste(target, paste(columns_to_replace, collapse="+"), sep = "~")), data = train, zeroadj = 0.5)
  return(woe_object)
}

# Apply WoE & return data set with columns replaced by woe
apply_woe = function(dataset, woeobject = woe_object, doReplace = TRUE)
{
  # Columns to replace are the columns that are in WoE-object
  columns_to_replace = names(woe_object$woe)
  # Predict WoE in the new dataset with the WoE-object and replace factor levels with their WoE
  dataset_woe = predict(woe_object, newdata = dataset, replace = doReplace)
  # Change names (get rid of the woe.-prefix)
  colnames(dataset_woe) = gsub(pattern = "woe.", replacement = "", x = colnames(dataset_woe))
  # Return the dataset with replaced columns
  return(dataset_woe)
}

#Remove redundant factor levels
remove_new_levels  = function(prediction)
  {
  prediction$Keyword_ID = factor(prediction$Keyword_ID)
  prediction$Ad_group_ID = factor(prediction$Ad_group_ID)
  prediction$Campaign_ID = factor(prediction$Campaign_ID)
  return(prediction)
}

data_setup = function(dataset)
  {
  #1.0 Factorizing Accounts, Devices & Matches etc
  dataset$Account_ID = as.factor(dataset$Account_ID)
  dataset$Device_ID = as.factor(dataset$Device_ID)
  dataset$Match_type_ID = as.factor(dataset$Match_type_ID)
  dataset$Keyword_ID = as.factor(dataset$Keyword_ID)
  dataset$Ad_group_ID = as.factor(dataset$Ad_group_ID)
  dataset$Campaign_ID = as.factor(dataset$Campaign_ID)
  #2.0 Interaction Terms
  #Create interaction terms
  dataset$Account_Device = paste(dataset$Account_ID, dataset$Device_ID)
  dataset$Account_Match = paste(dataset$Account_ID, dataset$Match_type_ID)
  dataset$Match_Device = paste(dataset$Match_type_ID, dataset$Device_ID)
  #Factorize
  dataset$Account_Match_Device = paste(dataset$Account_ID, dataset$Match_type_ID, dataset$Device_ID)
  dataset$Account_Device = as.factor(dataset$Account_Device)
  dataset$Account_Match = as.factor(dataset$Account_Match)
  dataset$Match_Device = as.factor(dataset$Match_Device)
  dataset$Account_Match_Device = as.factor(dataset$Account_Match_Device)
  if("Revenue" %in% colnames(dataset)){
  #Create Target Variable
  dataset$rpc = dataset$Revenue/dataset$Clicks
  #Create Transformed Version
  dataset$log.rpc = log(1 + dataset$rpc)
  # Price Paid
  # Assumption: Each Keyword - Account - Device etc combnation refers to a unique product and so 
  # dividing revenue by conversions gives us average price of a unique product for that day
  dataset$price.paid = 0
  dataset$price.paid = ifelse(dataset$Revenue == 0, 0, dataset$Revenue/dataset$Conversions)
  
  #Booking 
  dataset$booking = ifelse(dataset$Revenue>0, 1,0)
  dataset$booking = as.factor(dataset$booking)
  }
  return(dataset)
}


time_features = function(dataset) {
  if(!require("lubridate")) install.packages("lubridate"); library("lubridate") 
  if(!require("zoo")) install.packages("zoo"); library("zoo") 
  #Dates
  dataset$Date = as.Date(dataset$Date)
  
  #Dates: Month
  dataset$month = month(x = dataset$Date)
  dataset$month = as.factor(dataset$month)
  
  #Year-Month
  dataset$year.month = format(as.Date(dataset$Date), "%Y-%m")
  dataset$year.month = as.factor(dataset$year.month)
  
  #Day of the Week
  dataset$weekday = wday(dataset$Date)
  dataset$weekday = as.factor(dataset$weekday)
  
  #Day of the Month
  dataset$day.of.the.month = day(x = dataset$Date)
  dataset$day.of.the.month = as.factor(dataset$day.of.the.month)
  
  return(dataset)
}



strongly_correlated = function(dataset, threshold = 0.7)
  
{
  
  correlation_matrix = cor(dataset[, sapply(dataset, is.numeric)]) # calculate matrix only for numeric columns
  
  listed_variables = vector() # vector of listed variables to prevent duplication
  
  dropped_variables = vector()# vector of variables that will be dropped
  
  
  
  for(column in colnames(correlation_matrix))
    
  {
    
    listed_variables = append(listed_variables, column) # add column to listed variables
    
    # loop only over variables for which we have not calculated the correlation yet
    
    for(row in setdiff(row.names(correlation_matrix),listed_variables))
      
    {
      
      if(abs(correlation_matrix[row,column]) > threshold)
        
      {
        
        if(mean(abs(correlation_matrix[row,])) > mean(abs(correlation_matrix[,column])))
          
        {
          
          dropped_variables = append(dropped_variables, row)
          
        }
        
        else{dropped_variables = append(dropped_variables, column)}
        
      }
      
    }
    
  }
  
  # return unique variable names
  
  return(unique(dropped_variables))
  
}





