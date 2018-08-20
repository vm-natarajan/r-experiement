mergeDailyData = function(){
  
  setwd(dir = "C:\\Users\\VNatarajan\\R\\");
  zip_files <- list.files(path = ".",pattern = "*\\.ZIP");
  dataset <- data.frame(matrix(ncol = 14, nrow = 0));
  
  for(zip_file in zip_files){
    unzip(zip_file,exdir = ".");
  }
  
  data_files <- list.files(path = ".",pattern = "*\\.CSV");
  
  for(data_file in data_files){
    
    day <- substr(data_file,start = 3,stop = 4);
    month <- substr(data_file,start = 5,stop = 6);
    year <- substr(data_file,start = 7,stop = 8);
    date <- paste(year,month,day,sep = "-")
    mon <- format(as.Date(date),"%b")
    date <- paste(day,mon,year,sep = "-")
    day_wise <- read.csv(file = data_file);
    day_wise <- cbind(Date = date,day_wise,DQ = "NA",XDQTOTQ = "NA",SHL = "NA",SCO = "NA");
    dataset <- rbind(dataset,day_wise);
    
  }
  
  dataset[c(3,4,5,11,15)] <- NULL;
  colnames(dataset) <- c("Date","Security","Open.Price","High.Price","Low.Price","Close.Price","WAP","No..of.Trades","No.of.Shares","Total.Turnover..Rs..","Deliverable.Quantity","X..Deli..Qty.to.Traded.Qty","Spread.High.Low","Spread.Close.Open")
  dataset$WAP <- NA;
  dataset$Spread.High.Low <- dataset$High.Price - dataset$Low.Price;
  dataset$Spread.Close.Open <- dataset$Close.Price - dataset$Open.Price;
  write.csv(x = dataset,file = "daily_combined.csv",row.names = FALSE);
  
}