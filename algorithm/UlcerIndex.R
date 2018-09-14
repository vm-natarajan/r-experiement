calculateUlcerIndex <- function(dataset = dataset,days = 14,limit = 100){
  
  if(nrow(dataset) > limit)
    dataset <- dataset[c(1:(limit+days+2)),];
  
  pdd <- data.frame(matrix(ncol = 4, nrow = 0))
  col_names <- c("security", "date", "pdd","pdds")
  colnames(pdd) <- col_names
  
  for(row in 1:(nrow(dataset) - days)){

      periods_dataset <- dataset[c((row):(row+days-1)),];
      periods_high_close <- max(periods_dataset$Close.Price);
      current_close_price <- dataset$Close.Price[row];
      pdd_calc <- ((current_close_price - periods_high_close)/periods_high_close) * 100;
      pdd_squared_calc <- pdd_calc^2;
      new_row <- data.frame(security = dataset$Security[row],date = dataset$Date[row],pdd=pdd_calc,pdds = pdd_squared_calc);
      pdd <- rbind(pdd,new_row);
    
  }
  
  pdd$date <- as.Date(pdd$date);
  pdd$ROC <- as.numeric(pdd$ROC);
  pdds$ROC <- as.numeric(pdds$ROC);
  pdd <- pdd[c(nrow(so):1),];
  rownames(pdd) <- c(1:nrow(pdd));
  
  ui <- data.frame(matrix(ncol = 4, nrow = 0))
  col_names <- c("security", "date", "pdd","pdds","ui")
  colnames(ui) <- col_names
  
  for(srow in (days+1):nrow(pdd)){
    previous_days_dataset <- pdd[c((srow-days+1):srow),];
    tp_sma_calc <- (previous_days_dataset$tp);
    tp_sd_calc <- sd(previous_days_dataset$tp);
    cci_calc <- round((tp$tp[srow] - tp_sma_calc)/(0.015*tp_sd_calc),digits = 3);
    new_row <- data.frame(security = tp$security[srow],date = tp$date[srow],tp=tp$tp[srow],tp_sma = tp_sma_calc,tp_deviation = tp_sd_calc,cci = cci_calc);
    cci <- rbind(cci,new_row);
  }
  
  
   
  return(so_sma);
  
}


runSO <- function(masterset = dataset,days = 14,limit = 100){
  
  options(digits = 2);
  securities <- unique(masterset$Security);
  so_set <- data.frame(matrix(ncol = 3, nrow = 0))
  for(security in securities){
    data <- masterset[masterset$Security == security,]
    so <- calculateStochasticOscillator(dataset = data,days = days,limit = limit);
    so_set <- rbind(so_set,so);
  }
  
  return(so_set);
  
}
