calculateRateOfChange <- function(dataset = dataset,days = 12,limit = 100){
  
  if(nrow(dataset) > limit)
    dataset <- dataset[c(1:(limit+days)),];
  
  roc <- data.frame(matrix(ncol = 3, nrow = 0))
  col_names <- c("security", "date", "roc")
  colnames(roc) <- col_names
  
  for(row in 1:nrow(dataset)){
    
    if(row <= (nrow(dataset)-days)){
      
      previous_day_dataset <- dataset[c(row+days),];
      previous_close_price <- previous_day_dataset$Close.Price;
      
      current_day_dataset <- dataset[c(row),];
      current_close_price <- current_day_dataset$Close.Price;
      
      roc_calc <- ((current_close_price - previous_close_price)/previous_close_price)*100;
      
      new_row <- data.frame(security = dataset$Security[row],date = dataset$Date[row],roc = roc_calc);
      
      roc <- rbind(roc,new_row);
      
    }
    
  }
  
  return(roc);
  
}


runROC <- function(masterset = dataset,days = 12,limit = 100){
  
  securities <- unique(masterset$Security);
  roc_set <- data.frame(matrix(ncol = 3, nrow = 0));
  col_names <- c("security", "date", "roc");
  colnames(roc_set) <- col_names;
  
  for(security in securities){
    
    data <- masterset[masterset$Security == security,];
    roc <- calculateRateOfChange(dataset = data,days = days,limit = limit);
    roc_set <- rbind(roc_set,roc);
    
  }
  
  return(roc_set);
  
}