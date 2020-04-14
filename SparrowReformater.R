DatumNesting <- function(InputFile, PlugNum = NA){
  
  #### Args:
  #           InputFile:
  #           Isotope method:
  #           Project:
  
  #### Returns:
  #           UploadStatus:
  #This might be where we can change things to do different sample mounts?
  library(plyr)
  library(readxl)
  library(httr)
  library(jsonlite)
  
  source("GeneralSIMSImporter.R")
  
  BaseFile <- basename(InputFile)
  analysis <- list()
  
  Output <- GeneralSIMSImporter(InputFile = InputFile, PlugNum = PlugNum)
  
  url1 <-
    "https://github.com/EarthCubeGeochron/Sparrow-WiscSIMS/blob/master/Test-Data/WiscSIMSColumnDictionary.xlsx?raw=true"
  GET(url1, write_disk(tf <- tempfile(fileext = ".xlsx")))
  
  LookupDF <- read_excel(tf)
  
  #Make nested list for JSON format and Sparrow uploading
  m<-0
  for(k in 1:length(levels(Output$GUESS.SAMP))){
  
    SampleList <- list()

  for(j in 1:nrow(Output[Output$GUESS.SAMP==levels(Output$GUESS.SAMP)[k]&!is.na(Output$GUESS.SAMP==levels(Output$GUESS.SAMP)[k]),])){
    
    SampleAnalyses <- which(Output$GUESS.SAMP==levels(Output$GUESS.SAMP)[k])
    l <- SampleAnalyses[k]
    
  DatumList <- list()
  for(i in 1:ncol(Output)){
    
    DatumList <- append(DatumList,
           list(list(value = Output[l,i],
                type = list(parameter = colnames(Output)[i],
                            unit = LookupDF$unit[LookupDF$ColNames==colnames(Output)[i]]))))
    m <- m+1
    print(m)
  }
  
  analysis <- append(analysis, list(list(analysis_name = IsotopeMethod,
                   datum = DatumList,
                   session_index = j)))
  
  }
  
  SampleList <- append(SampleList,
                       list(list(
                         name = BaseFile,
                         sample = list(name = levels(Output$GUESS.SAMP)[k]),
                         date = min(Output$DATETIME, na.rm = TRUE),
                         analysis = analysis,
                                             date = Output$DATETIME[1])))
  
  #PUT(url="http://backend:5000/api/v1/import-data/session", body=SampleList, encode = "json")
  
  }
  
  return(print(paste("Sparrow uploaded", BaseFile)))

}