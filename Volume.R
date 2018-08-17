findPatternUsingVolume <- function(dataset,days = 4){
  for(row in 1:nrow(dataset)){
    if(row < days){
      previous_days_dataset <- dataset[c((row+1):(row+days)),];
      volume_average <- mean(previous_days_dataset$No.of.Shares);
      wap_average <- mean(previous_days_dataset$WAP);
      #,"WAP"
      #,"No.of.Shares"
      print(dataset$No.of.Shares[row]);
      print(volume_average);
      print(dataset$WAP[row]);
      print(wap_average);
      print((dataset$No.of.Shares[row] > volume_average) && (dataset$WAP[row] > wap_average));
      if(dataset$No.of.Shares[row] > volume_average && dataset$WAP[row] > wap_average){
        result <- as.data.frame(result,cbind(dataset[row,]))
        print(result);
      }
    }
  }
}