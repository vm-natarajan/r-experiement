db <- function(tableName){
  library(RMySQL);
  db = dbConnect(MySQL(), user='root', password='Testtest1', dbname='bse', host='localhost');
  rs <- dbGetQuery(db,paste("select * from",tableName ));
  return(rs);
}
