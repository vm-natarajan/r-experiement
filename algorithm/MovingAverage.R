calculateSimpleMovingAverage <- function(dataset = dataset,days = 12,limit = 100){
  
  if(nrow(dataset) > limit)
    dataset <- dataset[c(1:(limit+days)),];
  
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


runSMA <- function(masterset = dataset,days = 12,limit = 100){
  
  securities <- unique(masterset$Security);
  sma_set <- data.frame(matrix(ncol = 3, nrow = 0))
  col_names <- c("security", "date","sma")
  colnames(sma_set) <- col_names
  for(security in securities){
    data <- masterset[masterset$Security == security,]
    sma <- calculateSimpleMovingAverage(dataset = data,days = days,limit = limit);
    sma_set <- rbind(sma_set,sma);
  }
  
  return(sma_set);
  
}

calculateExponentialMovingAverage <- function(dataset = dataset,days = 12,limit = 100){
  
  if(nrow(dataset) > limit)
    dataset <- dataset[c(1:(limit+days)),];
  
  smoothing_constant <- 2/(days+1);
  initial_ema_dataset <- dataset[nrow(dataset):(nrow(dataset)-days+1),];
  ema <- as.numeric(mean(initial_ema_dataset$Close.Price));
  resultset <- data.frame(matrix(ncol = 3, nrow = 0));
  colnames(resultset) <- c("security","date","ema");
  for(row in (nrow(dataset)-days):1){
    close_price <- dataset[row,]$Close.Price;
    ema <- ((close_price - ema)*smoothing_constant) + ema;
    temp_set <- cbind(security = as.character(dataset[row,"Security"]),date = as.character(dataset[row,"Date"]),ema = round(ema,3));
    resultset <- rbind(resultset,temp_set);
  }
  resultset <- resultset[c(nrow(resultset):1),]
  return(resultset);
}


runEMA <- function(masterset = dataset,days = 12,limit = 100){
  
  securities <- unique(masterset$Security);
  ema_set <- data.frame(matrix(ncol = 3, nrow = 0))
  col_names <- c("security", "date","ema")
  colnames(ema_set) <- col_names
  
  for(security in securities){
    data <- masterset[masterset$Security == security,]
    ema <- calculateExponentialMovingAverage(dataset = data,days = days,limit = limit);
    ema_set <- rbind(ema_set,ema);
  }
  
  return(ema_set);
  
}