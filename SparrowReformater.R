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
  Output <- Output[!is.na(Output$File),]
  
  IsotopeMethod <- ""
  
  if(grepl("d18O", InputFile)){
    
    IsotopeMethod <- "d18O10"
    
  }
  
  if(grepl("d13C", InputFile)){
    
    IsotopeMethod <- "d13C7"
    
  }
  
  url1 <-
    "https://github.com/EarthCubeGeochron/Sparrow-WiscSIMS/blob/master/Test-Data/WiscSIMSColumnDictionary.xlsx?raw=true"
  GET(url1, write_disk(tf <- tempfile(fileext = ".xlsx")))
  
  LookupDF <- read_excel(tf)
  
  #Make nested list for JSON format and Sparrow uploading
  m<-0
  
  SampleList <- list()
  
  for(k in 1:length(levels(Output$GUESS.SAMP))){
    
    SampleList <- list()
    
    for(j in 1:nrow(Output[Output$GUESS.SAMP==levels(Output$GUESS.SAMP)[k],])){
      
      SampleAnalyses <- which(Output$GUESS.SAMP==levels(Output$GUESS.SAMP)[k])
      l <- SampleAnalyses[k]
      
      DatumList <- list()
      for(i in 1:ncol(Output)){
        
        DatumList <- append(DatumList,
                            list(list(value = Output[l,i],
                                      type = list(parameter = colnames(Output)[i],
                                                  unit = LookupDF$unit[LookupDF$ColNames==colnames(Output)[i]]))))
        #m <- m+1
        #print(m)
      }
      
      analysis <- append(analysis, list(list(analysis_name = IsotopeMethod,
                                             datum = DatumList,
                                             session_index = j)))
      
    }
    
    SampleList <- list(
      name = BaseFile,
      date = gsub(" ", "T", min(Output$DATETIME, na.rm = TRUE)),
      sample = list(name = levels(Output$GUESS.SAMP)[k]),
      analysis = analysis)
    
    # SampleList <- append(SampleList,
    #                      list(list(
    #                        name = BaseFile,
    #                        sample = list(name = levels(Output$GUESS.SAMP)[k]),
    #                        date = min(Output$DATETIME, na.rm = TRUE),
    #                        analysis = analysis,
    #                                            date = Output$DATETIME[1])))
    
    request <- list(
      filename=NA,
      data=SampleList
    )
    
    PUT(url="http://backend:5000/api/v1/import-data/session", body=request, encode = "json")
    
  }
  
  #PUT(url="http://backend:5000/api/v1/import-data/session", body=SampleList, encode = "json")
  
  return(paste("Sparrow uploaded", BaseFile))
  #return(SampleList)
}
