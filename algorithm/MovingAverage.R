calculateSimpleMovingAverage <- function(dataset = dataset,days = 12,limit = 100){
  
  if(nrow(dataset) > limit)
    dataset <- dataset[c(1:(limit+days)),];
  
  sma <- data.frame(matrix(ncol = 3, nrow = 0))
  col_names <- c("security", "date", "sma")
  colnames(sma) <- col_names
  
  for(row in 1:nrow(dataset)){
    if(row <= (nrow(dataset)-days)){
      previous_days_dataset <- dataset[c((row+1):(row+days)),];
      average <- mean(previous_days_dataset$Close.Price);
      new_row <- data.frame(security = dataset$Security[row],date = dataset$Date[row],sma=average);
      sma <- rbind(sma,new_row);
    }
  }
  
  return(sma);
  
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

#REFERENCE : https://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:moving_average_envelopes

runSMAEnvelope <- function(masterset = dataset,days = 12,limit = 100){
  
  securities <- unique(masterset$Security);
  sma_set <- data.frame(matrix(ncol = 3, nrow = 0))
  col_names <- c("security", "date","sma")
  colnames(sma_set) <- col_names
  for(security in securities){
    data <- masterset[masterset$Security == security,]
    sma <- calculateSimpleMovingAverage(dataset = data,days = days,limit = limit);
    sma_set <- rbind(sma_set,sma);
  }
  
  sma_numeric <- as.numeric(as.character(sma_set[,"sma"]));
  lower_envelope = sma_numeric - (0.025* sma_numeric);
  upper_envelope = sma_numeric + (0.025* sma_numeric);
  sma_set <-cbind(sma_set,lower_envelope = lower_envelope,upper_envelope = upper_envelope);
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

#REFERENCE : https://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:moving_average_envelopes

runEMAEnvelope <- function(masterset = dataset,days = 12,limit = 100){
  
  securities <- unique(masterset$Security);
  ema_set <- data.frame(matrix(ncol = 3, nrow = 0))
  col_names <- c("security", "date","ema")
  colnames(ema_set) <- col_names
  
  for(security in securities){
    data <- masterset[masterset$Security == security,]
    ema <- calculateExponentialMovingAverage(dataset = data,days = days,limit = limit);
    ema_set <- rbind(ema_set,ema);
  }
  ema_numeric <- as.numeric(as.character(ema_set[,"ema"]));
  lower_envelope = ema_numeric - (0.025* ema_numeric);
  upper_envelope = ema_numeric + (0.025* ema_numeric);
  ema_set <-cbind(ema_set,lower_envelope = lower_envelope,upper_envelope = upper_envelope);
  return(ema_set);
  
}

calculateMovingAverageConvergenceDivergence <- function(dataset = dataset,days1 = 26,days2 = 12,limit = 100){
  days1_set <- runEMA(dataset,days1,limit);
  print(nrow(days1_set));
  days2_set <- runEMA(dataset,days2,limit);
  days2_set <- days2_set[c(1:nrow(days1_set)),]
  print(nrow(days2_set));
  macd <- as.numeric(as.character(days2_set$ema)) - as.numeric(as.character(days1_set$ema));
  resultset <- cbind(security = days1_set$Security,date = days1_set$Date,days1_ema = days1_set$ema,days2_ema = days2_set$ema,macd = macd);
  return(resultset);
}