
#1.1 To find occurence of bullish Marubozu by passed dataset,along with lengths of body,lowershadow and upper
#shadow

#Notes : - When this pattern appears,the next few sessions are expected to be bullish

findBullishMarubozu <- function (dataset,body=80,ls=25,us=25){
  
  #if(dataset$Close.Price>dataset$Open.Price)
  
    dataset <- dataset[dataset$Close.Price>dataset$Open.Price,]
    
    #calculate body length of the candle stick
    candlestickbody <- dataset$Close.Price - dataset$Open.Price;
    
    #calculate lower shadow of the candle stick
    lowershodow <- dataset$Open.Price - dataset$Low.Price
    
    #calculate upper shodow of the candle stick
    uppershadow <- dataset$High.Price - dataset$Close.Price;
    
    #set the sum of above values as benchmark
    benchmark <- candlestickbody + lowershodow + uppershadow;
    
    #Calculate the percentage of parts against benchmark
    l_body <- round((candlestickbody/benchmark),digits = 3)*100;
    l_lowershadow <- round((lowershodow/benchmark),digits = 3)*100;
    l_uppershadow <- round((uppershadow/benchmark),digits = 3)*100;
    
    #Dataframe to keep hold of part elements in percentage
    i_result <- as.data.frame(cbind(l_body,l_lowershadow,l_uppershadow));
    
    #The final dataframe that returns bullish data by applying the given filter parameters
    dataset <- dataset[i_result$l_body>=body & i_result$l_lowershadow<=ls & i_result$l_uppershadow<=us,];
    
    dataset <- as.data.frame(cbind(Security = dataset$Security,Date = dataset$Date,Open.Price = dataset$Open.Price,Low.Price = dataset$Low.Price,High.Price = dataset$High.Price,Close.Price = dataset$Close.Price))
    return(dataset);
}    


#1.2 To find occurence of Bearish Marubozu by passed dataset,along with lengths of body,lowershadow and upper
#shadow

#Notes : - When this pattern appears,the next few sessions are expected to be bearish

findBearishMarubozu <- function (dataset,body=80,ls=25,us=25){
  
  #if(dataset$Close.Price>dataset$Open.Price)
  
  dataset <- dataset[dataset$Close.Price<dataset$Open.Price,]
  
  #calculate body length of the candle stick
  candlestickbody <- dataset$Open.Price - dataset$Close.Price;
  
  #calculate lower shadow of the candle stick
  lowershodow <- dataset$Close.Price - dataset$Low.Price
  
  #calculate upper shodow of the candle stick
  uppershadow <- dataset$High.Price - dataset$Open.Price;
  
  #set the sum of above values as benchmark
  benchmark <- candlestickbody + lowershodow + uppershadow;
  
  #Calculate the percentage of parts against benchmark
  l_body <- round((candlestickbody/benchmark),digits = 3)*100;
  l_lowershadow <- round((lowershodow/benchmark),digits = 3)*100;
  l_uppershadow <- round((uppershadow/benchmark),digits = 3)*100;
  
  #Dataframe to keep hold of part elements in percentage
  i_result <- as.data.frame(cbind(l_body,l_lowershadow,l_uppershadow));
  
  #The final dataframe that returns bullish data by applying the given filter parameters
  dataset <- dataset[i_result$l_body>=body & i_result$l_lowershadow<=ls & i_result$l_uppershadow<=us,];
  
  return(dataset);
}    

# R function that acts an engine to calculate ROC for all securities
runBullishMarubozu <- function(masterset = dataset,body=80,ls=25,us=25){

  securities <- unique(masterset$Security);
  
  #Initalization of data frame that holds resultant dataset
  m_bull_set <- data.frame(matrix(ncol = 6, nrow = 0));
 # col_names <- c("Security", "Date", "ROC")
  #colnames(roc_set) <- col_names;
  
  for(security in securities){
    
    data <- masterset[masterset$Security == security,];
    m_bull <- findBullishMarubozu(dataset = data,body = body,ls = ls,us = us);
    m_bull_set <- rbind(m_bull_set,m_bull);
    
  }
  
  return(m_bull_set);
  
}
