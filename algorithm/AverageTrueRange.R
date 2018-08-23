#REFERENCE : https://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:average_true_range_atr

calculateTrueRange <- function(dataset = dataset,days = 14 ,limit = 100){
  
  true_range_set <- data.frame(matrix(ncol = 3, nrow = 0))
  col_names <- c("security", "date", "true_range")
  colnames(true_range_set) <- col_names
  
  for(row in 1:(nrow(dataset))){
    
    current_day_dataset <- dataset[c(row),];
    current_high_price <- current_day_dataset$High.Price;
    current_low_price <- current_day_dataset$Low.Price;
    
    high_low_spread <- current_high_price - current_low_price;
    
    if(row == nrow(dataset)){
      
      previous_day_dataset <- dataset[c(row),];
      high_previous_close_spread <- 0;
      low_previous_close_spread <- 0;
      
    }else{
      
      previous_day_dataset <- dataset[c(row+1),];
      previous_close_price <- previous_day_dataset$Close.Price;
      high_previous_close_spread <- abs(current_high_price - previous_close_price);
      low_previous_close_spread <- abs(current_low_price - previous_close_price);
      
    }
    
    true_range <- max(high_low_spread,high_previous_close_spread,low_previous_close_spread);
    
    new_row <- data.frame(security = dataset$Security[row],date = dataset$Date[row],true_range = true_range);
    
    true_range_set <- rbind(true_range_set,new_row);
    
  }
  
  atr <- calculateAverageTrueRange(dataset = true_range_set ,days = days,limit = limit);
  return(atr);
  #return(true_range_set);
}

calculateAverageTrueRange <- function(dataset = dataset, days = 14 ,limit = 100){
  
  if(nrow(dataset) > limit)
    dataset <- dataset[c(1:(limit+days)),];
  
  average_true_range_set <- data.frame(matrix(ncol = 3, nrow = 0))
  col_names <- c("security", "date", "atr")
  colnames(average_true_range_set) <- col_names
  
  for(row in 1:nrow(dataset)){
    if(row <= (nrow(dataset)-days)){
      previous_days_dataset <- dataset[c((row+1):(row+days)),];
      average_atr <- round(mean(previous_days_dataset$true_range),digits = 3);
      new_row <- data.frame(security = dataset$security[row],date = dataset$date[row],atr=average_atr);
      average_true_range_set <- rbind(average_true_range_set,new_row);
    }
  }
  
  return(average_true_range_set);
  
}

runATR <- function(masterset = dataset,days = 14,limit = 100){
  
  securities <- unique(masterset$Security);
  atr_set <- data.frame(matrix(ncol = 3, nrow = 0));
  col_names <- c("security", "date", "atr");
  colnames(atr_set) <- col_names;
  
  for(security in securities){
    
    data <- masterset[masterset$Security == security,];
    atr <- calculateTrueRange(dataset = data,days = days,limit = limit);
    atr_set <- rbind(atr_set,atr);
    
  }
  
  return(atr_set);
  
}