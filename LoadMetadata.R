LoadMetadataTable <- function(){
  
#### This function serves to load the metadata table from GoogleDocs as it stands in Jan 2020.
#### File is newly downloaded, refreshed, and loaded into the R workspace.
#### 
#### 

library(googledrive)

  drive_download("WiscSIMS_metadata", type = "csv", overwrite = TRUE)
  
  DataInput<-read.csv("WiscSIMS_metadata.csv")
  

return(DataInput)  
  
}
