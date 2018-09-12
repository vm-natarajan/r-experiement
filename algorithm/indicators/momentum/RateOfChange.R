#REFERENCE : https://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:rate_of_change_roc_and_momentum

# R function to calculate rate of change of stocks for the dataset which is passed,considering the days

calculateRateOfChange <- function(dataset = dataset,days = 12,limit = 100){
  
  #Filter the records from passed dataset as equal as sum of limit and number of days
  
  if(nrow(dataset) > limit)
    dataset <- dataset[c(1:(limit+days)),];
  
  #Initalization of data frame that holds resultant dataset
  
  roc <- data.frame(matrix(ncol = 3, nrow = 0))
  col_names <- c("Security", "Date", "ROC")
  colnames(roc) <- col_names
  
  for(row in 1:nrow(dataset)){
    
    if(row <= (nrow(dataset)-days)){
      
      #Get nth previous record where n is equal to day passed as parameter
      
      previous_day_dataset <- dataset[c(row+days),];
      previous_close_price <- previous_day_dataset$Close.Price;
      
      #Get recent or current record
      
      current_day_dataset <- dataset[c(row),];
      current_close_price <- current_day_dataset$Close.Price;
      
      #Calculate Rate Of Change
      roc_calc <- format(((current_close_price - previous_close_price)/previous_close_price)*100,digits = 2);
      
      #Create new data set that columns- security, date and roc
      new_row <- data.frame(Security = dataset$Security[row],Date = dataset$Date[row],ROC = roc_calc,stringsAsFactors = FALSE);
      
      #Merge the new calculated roc with the resultant data set
      roc <- rbind(roc,new_row);
      
    }
    
  }
  
  roc$Security <- as.factor(roc$Security);
  roc$ROC <- as.numeric(roc$ROC);
  roc$Date <- as.Date(roc$Date);
  roc <- roc[c(nrow(roc):1),];
  rownames(roc) <- c(1:nrow(roc));
  return(roc);
  
}

# R function that acts an engine to calculate ROC for all securities
runROC <- function(masterset = dataset,days = 12,limit = 100){
  
  securities <- unique(masterset$Security);
  
  #Initalization of data frame that holds resultant dataset
  roc_set <- data.frame(matrix(ncol = 3, nrow = 0));
  
  for(security in securities){
    
    data <- masterset[masterset$Security == security,];
    roc <- calculateRateOfChange(dataset = data,days = days,limit = limit);
    roc_set <- rbind(roc_set,roc);
    
  }
  
  return(roc_set);
  
}