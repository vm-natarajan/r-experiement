#REFERENCE : https://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:chaikin_money_flow_cmf
# O/P Range : Between -1 and +1
# Buy Signal:  > 0.05
# Sell Signal: < -0.05

calculateChaikinMoneyFlow <- function(dataset = dataset,days = 20,limit = 100){
  options(scipen = 999,digits = 3)
  if(nrow(dataset) > limit)
    dataset <- dataset[c(1:(limit+days)),];
  
  mfv <- data.frame(matrix(ncol = 4, nrow = 0));
  col_names <- c("security", "date","volume", "mfv");
  colnames(mfv) <- col_names;
  mfm_calc <- ( (dataset$Close.Price - dataset$Low.Price) - (dataset$High.Price - dataset$Close.Price) ) / (dataset$High.Price - dataset$Low.Price);
  mfv_calc <- dataset$No.of.Shares * mfm_calc;
  mfv <- as.data.frame(cbind(security = dataset$Security,date = dataset$Date,volume = dataset$No.of.Shares,mfv = mfv_calc));
  mfv$mfv <- round(as.numeric(as.character(mfv$mfv)));
  mfv$volume <- as.numeric(as.character(mfv$volume));
  mfv$date <- as.Date(as.character(mfv$date));
  mfv <- mfv[c(nrow(mfv):1),];
  rownames(mfv) <- c(1:nrow(mfv));
  
  cmf <- data.frame(matrix(ncol = 5, nrow = 0));
  col_names <- c("security", "date","volume" ,"mfv","cmf");
  colnames(cmf) <- col_names;
  
  for(srow in (days+1):nrow(mfv)){
      previous_days_dataset <- mfv[c((srow-days+1):srow),];
      volume_sma_calc <- mean(previous_days_dataset$volume);
      mfv_sma_calc <- mean(previous_days_dataset$mfv);
      cmf_calc <- round(mfv_sma_calc/volume_sma_calc,digits = 3);
      new_row <- data.frame(security = mfv$security[srow],date = mfv$date[srow],volume=mfv$volume[srow],mfv=mfv$mfv[srow],cmf = cmf_calc);
      cmf <- rbind(cmf,new_row);
  }

  return(cmf);
  
}


runCMF <- function(masterset = dataset,days = 20,limit = 100){
  
  options(digits = 2);
  securities <- unique(masterset$Security);
  cmf_set <- data.frame(matrix(ncol = 3, nrow = 0))
  for(security in securities){
    data <- masterset[masterset$Security == security,]
    cmf <- calculateChaikinMoneyFlow(dataset = data,days = days,limit = limit);
    cmf_set <- rbind(cmf_set,cmf);
  }
  
  return(cmf_set);
  
}
