SIMS_data_extractor <- function(FileDirectory==NA){
#### Function to extract SIMS data from excel spreadsheets
  # and to associate those data with google forms generated metadata
  # only user input is the filepath to the folder containing all SIMS datafiles
  # these are likely the raw file inputs from the SIMS data repository, so
  # currently potentially unreliable standard analyses will be included.
  
  #Load required packages
  library(readxl)
  library(ggplot2)
  library(doBy)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(zoo)
  library(rstudioapi)
  library(jsonlite)
  source('~/Documents/WiscSIMSDataExtractor/LoadMetadata.R')
  source('~/Documents/WiscSIMSDataExtractor/ReadSIMSd18OFiles.R')
  
  
  if(is.na(FileDirectory)){
    FileDirectory <- file.choose()
    FileDirectory <- dirname(FileDirectory)
  }
  
  Metadata <- LoadMetadataTable()
  
  FileList <- as.vector(list.files(path = FileDirectory, full.names = T, pattern = ".xls?", recursive = TRUE))
  FullPath <- FileList
  FileList <- basename(FileList)
  FileList <- as.data.frame(FileList)
  ExcelFileData <- FileList %>% separate(FileList, c("Date", "Isotope", "User"), "_")
  
  ExcelFileData$User <- str_remove(ExcelFileData$User, ".xls[x]?")
  ExcelFileData <- cbind(FullPath, FileList, ExcelFileData)
  
for(i in 1:nrow(Metadata)){
  
  OpenFile <- Metadata$SIMS.Session.datafiles[i]
  
  PlugNum <- Metadata$Number.of.SIMS.mounts[i]
  
  InputFile <- as.character(ExcelFileData$FullPath[ExcelFileData$FileList==OpenFile])
  
  SingleSIMSFile <- d18O10SIMSimport(InputFile = InputFile, PlugNum = PlugNum)
  
  ProjectData <- Metadata[i,]
  
  #SingleSIMSFile <- SingleSIMSFile %>%  group_by(GUESS.SAMP) %>%  nest()
  
  SparrowOutput <- cbind(SingleSIMSFile, ProjectData)
  
NestedFile <- SparrowOutput %>%  group_by(Project.Title, GUESS.SAMP) %>% nest()
  
}
  SparrowOutput2 <- ProjectData[,list(SingleSIMSFile)]
  
  write_json(NestedFile,'/Users/macrostrat/Documents/WiscSIMSDataExtractor/NestExamp', pretty = TRUE)
}