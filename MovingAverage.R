calculateSimpleMovingAverage <- function(dataset = dataset,days = 3){
  simple_moving_average <- data.frame(matrix(ncol = 3, nrow = 0))
  col_names <- c("security", "date", "simple_moving_average")
  colnames(simple_moving_average) <- col_names
  for(row in 1:nrow(dataset)){
    if(row <= (nrow(dataset)-days)){
      previous_days_dataset <- dataset[c((row+1):(row+days)),];
      average <- mean(previous_days_dataset$Close.Price);
      new_row <- data.frame(security = dataset$Security[row],date = dataset$Date[row],simple_moving_average=average);
      simple_moving_average <- rbind(simple_moving_average,new_row);
    }
  }
  return(simple_moving_average);
}


calculateExponentialMovingAverage <- function(dataset = dataset,days = 10){
  
  smoothing_constant <- 2/(days+1);
  initial_ema_dataset <- dataset[nrow(dataset):(nrow(dataset)-days+1),];
  ema <- as.numeric(mean(initial_ema_dataset$Close.Price));
  resultset <- data.frame(matrix(ncol = 3, nrow = 0));
  colnames(resultset) <- c("security","date","ema");
  resultset <- cbind(security = as.character(dataset[(nrow(dataset)-days+1),"Security"]),date = as.character(dataset[(nrow(dataset)-days+1),"Date"]),ema = ema);
  for(row in (nrow(dataset)-days):1){
      close_price <- dataset[row,]$Close.Price;
      ema <- ((close_price - ema)*smoothing_constant) + ema;
      temp_set <- cbind(security = as.character(dataset[row,"Security"]),date = as.character(dataset[row,"Date"]),ema = ema);
      resultset <- rbind(resultset,temp_set);
   }
  return(resultset);
}
