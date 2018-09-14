#REFERENCE : https://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:williams_r
# O/P Range : Between 0 and -100
# Buy Signal: <-80
# Sell Signal: >-20

calculateWilliamRPercent <- function(dataset = dataset,days = 14,limit = 100){
  
  if(nrow(dataset) > limit)
    dataset <- dataset[c(1:(limit+days)),];
  
  wrp <- data.frame(matrix(ncol = 3, nrow = 0))
  col_names <- c("security", "date", "rpercent")
  colnames(wrp) <- col_names
  
  for(row in 1:nrow(dataset)){
    if(row <= (nrow(dataset)-days)){
      periods_dataset <- dataset[c((row):(row+days-1)),];
      periods_high <- max(periods_dataset$High.Price);
      periods_low <- min(periods_dataset$Low.Price);
      wrp_calc <- ((periods_high - dataset$Close.Price[row])/(periods_high - periods_low))*(-100);
      new_row <- data.frame(security = dataset$Security[row],date = dataset$Date[row],rpercent=wrp_calc);
      wrp <- rbind(wrp,new_row);
    }
    
  }
  
  wrp$date <- as.Date(wrp$date);
  wrp <- wrp[c(nrow(wrp):1),];
  rownames(wrp) <- c(1:nrow(wrp));
  return(wrp);
  
}


runWRP <- function(masterset = dataset,days = 14,limit = 100){
  
  options(digits = 2);
  securities <- unique(masterset$Security);
  wrp_set <- data.frame(matrix(ncol = 3, nrow = 0))
  for(security in securities){
    data <- masterset[masterset$Security == security,]
    wrp <- calculateWilliamRPercent(dataset = data,days = days,limit = limit);
    wrp_set <- rbind(wrp_set,wrp);
  }
  
  return(wrp_set);
  
}
