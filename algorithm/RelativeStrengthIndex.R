# R function to calculate the relative strenth index of the security
calculateRelativeStrengthIndex <- function(dataset = dataset,days = 12){
  
  #Select number of records based on days passed to consider for calculating RSI
  dataset <- dataset[c(1:days),];
  
  #Retrieve previous days record to calculate RSI
  previous_days_dataset <- dataset[-1,];
  
  dataset <- dataset[-nrow(dataset),]
  
  #Calculate the point difference from previous day close
  point_difference <- dataset$Close.Price - previous_days_dataset$Close.Price;
  
  #Calculate the sum and average of points gained for days given
  points_gained <- sum(point_difference[point_difference>0]);
  average_points_gained <- mean(point_difference[point_difference>0]);
  
  #Calculate the sum and average of points lost for days given
  points_lost <- sum(point_difference[point_difference<0]);
  average_points_lost <- -mean(point_difference[point_difference<0]);
 
  #Calculate RS factor and index
  relative_strenth_factor <- average_points_gained/average_points_lost;
  relative_strenth_index <- 100 - (100/(1+relative_strenth_factor));
  
  ifelse(is.nan(average_points_gained),yes = relative_strenth_index <- 0,no = "");
  ifelse(is.nan(average_points_lost),yes = relative_strenth_index <- 100,no = "");
  
  return(relative_strenth_index);
}

runRelativeStrenthIndex <- function(masterset = dataset,days = 12){
  
  securities <- unique(masterset$Security);
  rsi_set <- data.frame(matrix(ncol = 2, nrow = 0))
  col_names <- c("security", "rsi")
  colnames(rsi_set) <- col_names
  for(security in securities){
    data <- masterset[masterset$Security == security,]
    rsi <- calculateRelativeStrengthIndex(dataset = data,days = days);
    new_row <- data.frame(security = security,rsi = rsi);
    rsi_set <- rbind(rsi_set,new_row);
  }
  return(rsi_set);
  
}
