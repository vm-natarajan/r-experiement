calculateStochasticOscillator <- function(dataset = dataset,days = 14,limit = 100){
  
  if(nrow(dataset) > limit)
    dataset <- dataset[c(1:(limit+days)),];
  
  so <- data.frame(matrix(ncol = 3, nrow = 0))
  col_names <- c("security", "date", "so")
  colnames(sma) <- col_names
  
  for(row in 1:nrow(dataset)){
    
    if(row <= (nrow(dataset)-days)){
      periods_dataset <- dataset[c((row):(row+days-1)),];
      periods_high <- max(periods_dataset$High.Price);
      periods_low <- min(periods_dataset$Low.Price);
      so_calc <- ((dataset$Close.Price[row] - periods_low)/(periods_high - periods_low))*100;
      new_row <- data.frame(security = dataset$Security[row],date = dataset$Date[row],so=so_calc);
      so <- rbind(so,new_row);
    }
    
  }
  
  so$date <- as.Date(so$date);
  so <- so[c(nrow(so):1),];
  rownames(so) <- c(1:nrow(so));
  
  so_sma <- data.frame(matrix(ncol = 4, nrow = 0))
  col_names <- c("security", "date", "so","so_sma")
  colnames(so_sma) <- col_names
  
  for(srow in 3:nrow(so)){
      previous_days_dataset <- so[c((srow-2):srow),];
      so_sma_calc <- mean(previous_days_dataset$so);
      new_row <- data.frame(security = so$security[srow],date = so$date[srow],so=so$so[srow],so_sma = so_sma_calc);
     # new_row <- data.frame(so[srow,],so_sma=so_sma);
      so_sma <- rbind(so_sma,new_row);
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
