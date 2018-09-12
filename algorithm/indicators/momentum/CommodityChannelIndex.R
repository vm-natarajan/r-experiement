calculateCommodityChannelIndex <- function(dataset = dataset,days = 20,limit = 100){
  options(scipen = 999)
  if(nrow(dataset) > limit)
    dataset <- dataset[c(1:(limit+days)),];
  
  tp <- data.frame(matrix(ncol = 3, nrow = 0))
  col_names <- c("security", "date", "tp")
  colnames(tp) <- col_names
  tp_calc <- round((dataset$High.Price + dataset$Low.Price + dataset$Close.Price)/3,digits = 3);
  tp <- as.data.frame(cbind(security = dataset$Security,date = dataset$Date,tp = tp_calc));
  tp$tp <- as.numeric(as.character(tp$tp));
  tp$date <- as.Date(as.character(tp$date));
  tp <- tp[c(nrow(tp):1),];
  rownames(tp) <- c(1:nrow(tp));
  
  cci <- data.frame(matrix(ncol = 6, nrow = 0))
  col_names <- c("security", "date", "tp","tp_sma","tp_deviation","cci")
  colnames(cci) <- col_names
  
  for(srow in (days+1):nrow(tp)){
      previous_days_dataset <- tp[c((srow-days+1):srow),];
      tp_sma_calc <- mean(previous_days_dataset$tp);
      tp_sd_calc <- sd(previous_days_dataset$tp);
      cci_calc <- round((tp$tp[srow] - tp_sma_calc)/(0.015*tp_sd_calc),digits = 3);
      new_row <- data.frame(security = tp$security[srow],date = tp$date[srow],tp=tp$tp[srow],tp_sma = tp_sma_calc,tp_deviation = tp_sd_calc,cci = cci_calc);
      cci <- rbind(cci,new_row);
  }

  return(cci);
  
}

runCCI <- function(masterset = dataset,days = 20,limit = 100){
  
  options(digits = 2);
  securities <- unique(masterset$Security);
  cci_set <- data.frame(matrix(ncol = 3, nrow = 0))
  for(security in securities){
    data <- masterset[masterset$Security == security,]
    cci <- calculateCommodityChannelIndex(dataset = data,days = days,limit = limit);
    cci_set <- rbind(cci_set,cci);
  }
  
  return(cci_set);
  
}
