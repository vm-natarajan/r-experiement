calculateBollingerBand <- function(dataset = dataset,days = 12,limit = 100){
  
  if(nrow(dataset) > limit)
    dataset <- dataset[c(1:(limit+days)),];
  
  bollinger_band <- data.frame(matrix(ncol = 5, nrow = 0))
  col_names <- c("security", "date", "middleband","upperband","lowerband")
  colnames(bollinger_band) <- col_names
  sd_correction_factor <- ((days-1)/days)^(0.5);
  
  for(row in 1:nrow(dataset)){
    
    if(row <= (nrow(dataset)-days)){
      previous_days_dataset <- dataset[c((row+1):(row+days)),];
      middleband <- mean(previous_days_dataset$Close.Price);
      upperband <- middleband + (2*sd(previous_days_dataset$Close.Price)*sd_correction_factor);
      lowerband <- middleband - (2*sd(previous_days_dataset$Close.Price)*sd_correction_factor);
      new_row <- data.frame(security = dataset$Security[row],date = dataset$Date[row],middleband = middleband,upperband = upperband,lowerband = lowerband);
      bollinger_band <- rbind(bollinger_band,new_row);
    }
    
  }
  
  return(bollinger_band);
  
}


runBollingerBand <- function(masterset = dataset,days = 12,limit = 100){
  
  securities <- unique(masterset$Security);
  bollinger_band_set <- data.frame(matrix(ncol = 5, nrow = 0))
  col_names <- c("security", "date", "middleband","upperband","lowerband")
  colnames(bollinger_band_set) <- col_names
  
  for(security in securities){
    
    data <- masterset[masterset$Security == security,]
    bollinger_band <- calculateBollingerBand(dataset = data,days = days,limit = limit);
    bollinger_band_set <- rbind(bollinger_band_set,bollinger_band);
    
  }
  
  return(bollinger_band_set);
  
}