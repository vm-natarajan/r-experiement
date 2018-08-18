findPatternUsingVolume <- function(dataset,days = 5){
  
  volume <- data.frame(matrix(ncol = 3, nrow = 0))
  col_names <- c("security", "date", "trend")
  colnames(volume) <- col_names
  for(row in 1:nrow(dataset)){
    if(row <= (nrow(dataset)-days)){

      previous_days_dataset <- dataset[c((row+1):(row+days)),];
      wap_average <- mean(previous_days_dataset$WAP);
      volume_average <- mean(previous_days_dataset$No.of.Shares);
      
      if((dataset$WAP[row] > wap_average) && (dataset$No.of.Shares[row] > volume_average)){
        new_row <- data.frame(security = dataset$Security[row],date = dataset$Date[row],trend="II");
      }else if((dataset$WAP[row] < wap_average) && (dataset$No.of.Shares[row] > volume_average)){
        new_row <- data.frame(security = dataset$Security[row],date = dataset$Date[row],trend="DI");
      }else if((dataset$WAP[row] < wap_average) && (dataset$No.of.Shares[row] < volume_average)){
        new_row <- data.frame(security = dataset$Security[row],date = dataset$Date[row],trend="DD");
      }else if ((dataset$WAP[row] > wap_average) && (dataset$No.of.Shares[row] < volume_average)){
        new_row <- data.frame(security = dataset$Security[row],date = dataset$Date[row],trend="ID");
      }else{
        new_row <- data.frame(security = dataset$Security[row],date = dataset$Date[row],trend="NC");
      }
      
      volume <- rbind(volume,new_row);
    }
  }
  return(volume);
  
}

calculatePriceVolumeTrend <- function(dataset = dataset,days = 10){
  
  resultset <- data.frame(matrix(ncol = 3, nrow = 0));
  colnames(resultset) <- c("security","date","pv_trend");
  pvt <- dataset[(nrow(dataset)-days+1),]$No.of.Shares;
  for(row in (nrow(dataset)-days):1){
    
    previous_close <-  dataset[(row+1),]$Close.Price;
    close_price <- dataset[row,]$Close.Price;
    close_price_cent_change <- ((close_price - previous_close)/previous_close);
    volume_cent_change <- close_price_cent_change*dataset[row,]$No.of.Shares;
    pvt <- pvt + volume_cent_change;
    temp_set <- cbind(security = as.character(dataset[row,"Security"]),date = as.character(dataset[row,"Date"]),pv_trend = pvt);
    resultset <- rbind(resultset,temp_set);
  }
  return(resultset);
}


