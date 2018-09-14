generateKNNInput <- function(dataset = dataset,limit = 500){
  
  file.sources = list.files(path = "algorithm/indicators/momentum/",pattern="*.R",full.names = TRUE);
  sapply(file.sources,source,.GlobalEnv);
  CCI <- runCCI(masterset = dataset,limit = limit);
  CMF <- runCMF(masterset = dataset,limit = limit);
  RSI <- runRSI(masterset = dataset,limit = limit);
  WRP <- runWRP(masterset = dataset,limit = limit);
  current_close_dataset <- dataset$Close.Price[c(1:(limit-1))];
  previous_close_dataset <- dataset$Close.Price[c(2:limit)];
  diff <- current_close_dataset - previous_close_dataset;
  diff_set <- ifelse(diff >= 0 ,yes = "GAIN" ,no = "LOSS");
  diff_set <- diff_set[c(length(diff_set):1)];
  result_set <- cbind(CCI,CMF,RSI,RSI);
  print(paste(nrow(CCI),nrow(CMF),nrow(RSI),nrow(RSI)));
  result_set <- result_set[-nrow(result_set),];
  result_set <- result_set[,c(1,2,6,11,14,17)]
  colnames(result_set) <- c("Security", "Date" , "CCI" ,"CMF" ,"RSI" , "WRP");
  result_set <- cbind(result_set ,Change = diff_set);
  result_set$CCI <- round(result_set$CCI,digits = 2);
  result_set$CMF <- round(result_set$CMF,digits = 2);
  return(result_set);
  
}



getKNNInput <- function(masterset = dataset,limit = 500){
  
  securities <- unique(masterset$Security);
  knndata_set <- data.frame(matrix(ncol = 3, nrow = 0))
  
  for(security in securities){
    print(security);
    data <- masterset[masterset$Security == security,]
    if( nrow(data) - (0.25 * nrow(data) ) > limit ){
      knndata <- generateKNNInput(dataset = data,limit = limit);
      knndata_set <- rbind(knndata_set,knndata);
    }
    
  }
  
  return(knndata_set);
  
}