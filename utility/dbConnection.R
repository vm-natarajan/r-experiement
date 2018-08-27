db <- function(tableName){
  library(RMySQL);
  userName <- 'root';
  passWord <- 'Testtest1';
  dbName <- 'bse';
  hostName <- 'localhost';
  db = dbConnect(MySQL(), user=userName, password=passWord, dbname=dbName, host=hostName);
  rs <- dbGetQuery(db,paste("select * from",tableName,"order by Date DESC;"));
  return(rs);
}