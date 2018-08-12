findBullishMarubozu <- function (dataset,body=40,ls=30,us=40){
  
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
    
    return(dataset);

}    

