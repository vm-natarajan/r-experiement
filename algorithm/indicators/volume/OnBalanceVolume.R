calculateOnBalanceVolumeTrend <- function(dataset = dataset,days = 12,limit = 100){
  
  if(nrow(dataset) > limit)
    dataset <- dataset[c(1:(limit+days)),];
  
  resultset <- data.frame(matrix(ncol = 3, nrow = 0));
  colnames(resultset) <- c("security","date","obv_trend");
  obv <- dataset[(nrow(dataset)-days),]$No.of.Shares;
  
  temp_set <- cbind(security = as.character(dataset[(nrow(dataset)-days),"Security"]),date = as.character(dataset[(nrow(dataset)-days),"Date"]),obv_trend = obv);
  resultset <- rbind(resultset,temp_set);
  
  for(row in (nrow(dataset)-days-1):1){
    current_price <- dataset[row,]$Close.Price;
    current_volume <- dataset[row,]$No.of.Shares;
    previous_price <- dataset[(row+1),]$Close.Price;
    if(current_price > previous_price){
      obv <- obv + current_volume;
    }else if(current_price < previous_price){
      obv <- (obv - current_volume);
    }else{
      obv <- obv;
    }
    temp_set <- cbind(security = as.character(dataset[row,"Security"]),date = as.character(dataset[row,"Date"]),obv_trend = obv);
    resultset <- rbind(resultset,temp_set);
  }
  
  #resultset <- resultset[c(nrow(resultset):1),];
  return(resultset);
  
}

runOBV <- function(masterset = dataset,days = 12,limit = 100){
  
  securities <- unique(masterset$Security);
  
  #Initalization of data frame that holds resultant dataset
  obv_set <- data.frame(matrix(ncol = 3, nrow = 0));
 
  for(security in securities){
    
    data <- masterset[masterset$Security == security,];
    obv <- calculateOnBalanceVolumeTrend(dataset = data,days = days,limit = limit);
    obv_set <- rbind(obv_set,obv);
    
  }
  return(obv_set);
}
