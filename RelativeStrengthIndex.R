# R function to calculate the relative strenth index of the security
calculateRelativeStrengthIndex <- function(dataset = dataset,days = 5){
  
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
  return(relative_strenth_index);
}
