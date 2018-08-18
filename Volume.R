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