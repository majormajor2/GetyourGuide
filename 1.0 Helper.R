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



lastObservation <- function(dat.fr){
  #Computes the days since the last observation for each Ad_group
  #
  #Args:
  #   dat.fr: a data frame with Ad_group column (train,prediction)
  #   
  #
  #Returns:
  #   A data table with lineID and days since last observation
  
  if (!require("doSNOW")) install.packages("doSNOW")
  if (!require("doParallel")) install.packages("doParallel")
  if (!require("tcltk")) install.packages("tcltk")
  if (!require("data.table")) install.packages("data.table")
  
  library(data.table)
  library(doSNOW)
  library(tcltk)
  library(doParallel)
  
  dat.fr <- data.table(dat.fr)
  dat.fr <- dat.fr[order(Ad_group_ID, lineID),]
  dat.fr[,count:=1]
  help <- dat.fr[,sum(count),by=Ad_group_ID]
  nr.Ad_group_ID <- dim(help)[1]
  
  cores <- detectCores()-1
  cl <- makeSOCKcluster(cores)
  registerDoSNOW(cl)
  
  pb <- txtProgressBar(max=nr.Ad_group_ID, style=3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  
  x <- foreach(i=1:nr.Ad_group_ID,.options.snow=opts,.combine = 'c') %dopar% {
    
    end <- sum(help$V1[c(1:i)])
    len <- help$V1[i]
    vec <- rep(0,len)
    
    cnt <- 1  
    
    last <- 0
    for(j in c((end-len+1):end)){
      if(j==(end-len+1)){
        vec[cnt] <- 0
      } else {
        vec[cnt] <- (dat.fr$day[j]-last)
      }
      cnt <- cnt+1
      last <- dat.fr$day[j]
    }
    vec
  }
  stopCluster(cl)
  output <- data.table(cbind(dat.fr$lineID,x))
  names(output) <- c("lineID","lastObservation")
  return(output)
}



## This function calculates and returns the distance of every row in the data set to the k closest cases THAT WERE ORDERS ##
## Might give additional information of the similarity of every case with other successful (order) cases ##
## Three return values currently implemented (can be expanded easily), to be specified in "type" argument:
##      1. "average" : average of all distances to k specified neighbors
##      2. "sum"     : sum of the distances to k specified neighbors
##      3. "all"     : returns k columns with every single distance to k closest order cases

## Should be used after all preparation and static feature engineering is finished to incorporate as much info as possible
## 1.) Apply on train set itself (train = predSet) to create the feature
## 2.) Apply on prediction set from train to create the features on final Prediction set 
## 3.) Try different values of k

if(!require("FNN")) install.packages("FNN"); library("FNN")

get_dist <- function(trainSet, predSet, k, type = "average"){
  
  
  trainSet_idx_numeric <- sapply(trainSet, is.numeric)  # filter numerical variables
  trainSet_numeric <- trainSet[,trainSet_idx_numeric]
  trainSet_numeric <- as.data.frame(lapply(trainSet_numeric, standardize1))
  trainSet_numeric$order <- trainSet$order            # attach order variable (since dependent)
  
  trainSet_numeric <- trainSet_numeric[,-which(colnames(trainSet_numeric) == "rpc")]  # take out revenue since not in class set
  
  if(!identical(trainSet, predSet)){
    predSet_idx_numeric <- sapply(predSet, is.numeric)  # filter numerical variables
    predSet_numeric <- predSet[,predSet_idx_numeric]
    predSet_numeric <- as.data.frame(lapply(predSet_numeric, standardize1))
    
    if(any(colnames(predSet) == "order")){
      predSet_numeric$order <- predSet$order  
    }
    
    if(any(colnames(predSet) == "rpc")){
      predSet_numeric <- predSet_numeric[,-which(colnames(predSet_numeric) == "revenue")]
    }
    
  }else{
    predSet_numeric <- trainSet_numeric
  }
  
  par.nn <- k
  
  print("start modelling")
  
  nn.list <- get.knnx(data = trainSet_numeric[which(trainSet_numeric$order == 1),-c(which(colnames(trainSet_numeric) == "order"), which(colnames(trainSet_numeric) == "lineID"))], query = predSet_numeric[,-c(which(colnames(predSet_numeric) == "order"), which(colnames(predSet_numeric) == "lineID"))], k = par.nn)
  
  ### depending on "type" calculate the required distance(s) and return
  
  if(type == "average"){
    avgDistance <- apply(as.data.frame(nn.list[2]), 1, function(x){mean(x)})
    return(avgDistance)
  }
  
  if(type == "all"){
    distances <- as.data.frame(nn.list[2])
    return(distances)
  }
  
  if(type == "sum"){
    sumDistance <- apply(as.data.frame(nn.list[2]), 1, function(x){sum(x)})
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
calculate_woe = function(train, target = "log.rpc", columns_to_replace = c("Keyword_ID","Ad_group_ID","Campaign_ID"))
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


remove_new_levels  = function(prediction)
  {
  prediction$Keyword_ID = factor(prediction$Keyword_ID)
  prediction$Ad_group_ID = factor(prediction$Ad_group_ID)
  prediction$Campaign_ID = factor(prediction$Campaign_ID)
  return(prediction)
}






