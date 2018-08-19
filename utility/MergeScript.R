setwd("/Users/veera/d-science/bse/src/")
if(exists("dataset")){
  rm(dataset)
}

files <- list.files(path = "/Users/veera/d-science/bse/src/",pattern = "*.csv");

for (file in files) {
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.table(file, header=TRUE, sep=",")
    dataset<-cbind(dataset, Security = gsub("\\.csv","",file))
  }
  
  # if the merged dataset does exist, append to it
  else{
    temp_dataset <-read.table(file, header=TRUE, sep=",")
    temp_dataset <- cbind(temp_dataset,Security = gsub("\\.csv","",file))
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
  
}

write.table(dataset,file = "/Users/veera/d-science/bse/dest/combined.csv",col.names = TRUE,sep = ",",row.names = FALSE)
