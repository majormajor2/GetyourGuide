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
