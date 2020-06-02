DatumNesting <- function(InputFile, PlugNum = NA){
  
  #### Args:
  #           InputFile:
  #           Isotope method:
  #           Project:
  
  #### Returns:
  #           UploadStatus:
  #This might be where we can change things to do different sample mounts?
  # Currently trying to recommit this to github...
  library(plyr)
  library(readxl)
  library(httr)
  library(jsonlite)
  
  source("GeneralSIMSImporter.R")
  
  BaseFile <- basename(InputFile)
  analysis <- list()
  
  Output <- GeneralSIMSImporter(InputFile = InputFile, PlugNum = PlugNum)
  Output <- Output[!is.na(Output$File),]
  
  print(colnames(Output))
  
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
      l <- SampleAnalyses[j]
      
      #Should only upload one sample worth of data at a time
      
      DatumList <- list()
      m <- 1
      for(i in 1:ncol(Output)){
        
        
        if(LookupDF$Type[LookupDF$ColNames==colnames(Output)[i]]=="Numeric"){
          
          if(!is.na(Output[l,i])){
            value <- Output[l,i]
          }else{
            value <- -1042
          }
          
          DatumList [[m]] <- list(value = value,
                                  type = list(parameter = colnames(Output)[i],
                                              unit = LookupDF$unit[LookupDF$ColNames==colnames(Output)[i]]))
          
          m <- m+1
        }
        
        
        #print(m)
      }
      
      analysis[[j]] <- list(analysis_name = IsotopeMethod,
                            datum = DatumList,
                            session_index = j
      )
      
    }
    
    Session <- list(
      name = paste(BaseFile, levels(Output$GUESS.SAMP)[k], sep = "_"),
      sample = list(name = levels(Output$GUESS.SAMP)[k]),
      date = gsub(" ", "T", min(Output$DATETIME, na.rm = TRUE)),
      analysis = analysis)
    
    # SampleList <- append(SampleList,
    #                      list(list(
    #                        name = BaseFile,
    #                        sample = list(name = levels(Output$GUESS.SAMP)[k]),
    #                        date = min(Output$DATETIME, na.rm = TRUE),
    #                        analysis = analysis,
    #                                            date = Output$DATETIME[1])))
    
    request <- list(
      filename=paste(BaseFile, k),
      data=Session
    )
    
    response <- PUT(url="http://backend:5000/api/v1/import-data/session", body=request, encode = "json")
    
    errors<-content(response)
    
    print(errors)
    
    
  }
  
  #PUT(url="http://backend:5000/api/v1/import-data/session", body=SampleList, encode = "json")
  
  return(print("Sparrow upload", BaseFile))
  #return(SampleList)
}
