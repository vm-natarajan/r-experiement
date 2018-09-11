calculateRelativeStrengthIndex <- function(dataset = dataset,days = 14,limit = 100){
  
  if(nrow(dataset) > limit)
    dataset <- dataset[c(1:(limit+days)),];
  
  rsi <- data.frame(matrix(ncol = 3, nrow = 0))
  col_names <- c("security", "date", "rsi")
  colnames(rsi) <- col_names
  
  for(row in (nrow(dataset)-days):1){
      
      current_days_dataset <- dataset[c((row):(row+days-1)),];
      previous_days_dataset <- dataset[c((row+1):(row+days)),];
      
      #Calculate the point difference from previous day close
      point_difference <- current_days_dataset$Close.Price - previous_days_dataset$Close.Price;
      
      #Calculate the sum and average of points gained for days given
      average_points_gained <- sum(point_difference[point_difference>0])/days;
      
      #Calculate the sum and average of points lost for days given
      average_points_lost <- abs(sum(point_difference[point_difference<0])/days);
      
      #Calculate RS factor and index
      relative_strenth_factor <- average_points_gained/average_points_lost;
      relative_strenth_index <- 100 - (100/(1+relative_strenth_factor));
      
      ifelse(is.nan(average_points_gained),yes = relative_strenth_index <- 0,no = "");
      ifelse(is.nan(average_points_lost),yes = relative_strenth_index <- 100,no = "");
      
      average <- mean(previous_days_dataset$Close.Price);
      new_row <- data.frame(security = dataset$Security[row],date = dataset$Date[row],rsi=relative_strenth_index);
      rsi <- rbind(rsi,new_row);
  }
  rsi$rsi <- as.numeric(rsi$rsi);
  rsi$date <- as.Date(rsi$date);
  return(rsi);
  
}


runRSI <- function(masterset = dataset,days = 14,limit = 100){
  
  securities <- unique(masterset$Security);
  rsi_set <- data.frame(matrix(ncol = 3, nrow = 0))
  for(security in securities){
    data <- masterset[masterset$Security == security,]
    rsi <- calculateRelativeStrengthIndex(dataset = data,days = days,limit = limit);
    rsi_set <- rbind(rsi_set,rsi);
  }
  
  return(rsi_set);
  
}