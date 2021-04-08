DatumNesting <- function(InputFile,
                         PlugNum = NA,
                         Upload = TRUE) {
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
  #InputFile <- fp
  #PlugNum = NA
  #Upload = FALSE
  source("GeneralSIMSImporter.R")
  
  BaseFile <- basename(InputFile)
  analysis <- list()
  
  Output <-
    GeneralSIMSImporter(InputFile = InputFile, PlugNum = PlugNum)
  Output <- Output[!is.na(Output$File), ]
  Output$GUESS.SAMP <- as.factor(Output$GUESS.SAMP)
  Output$GUESS.SAMP <- droplevels(Output$GUESS.SAMP)
  Output <- Output[!is.na(Output$File)&!is.na(Output$DTFAX),]
  
  print(colnames(Output))
  
  IsotopeMethod <- ""
  
  if (grepl("d18O", InputFile)) {
    IsotopeMethod <- "d18O10"
    
  }
  
  if (grepl("d13C", InputFile)) {
    IsotopeMethod <- "d13C7"
    
  }
  
  url1 <-
    "https://github.com/EarthCubeGeochron/Sparrow-WiscSIMS/blob/master/Test-Data/WiscSIMSColumnDictionary.xlsx?raw=true"
  GET(url1, write_disk(tf <- tempfile(fileext = ".xlsx")))
  
  LookupDF <- read_excel(tf)
  
  
  #Make nested list for JSON format and Sparrow uploading
  m <- 0
  n <- 0
  
  SampleList <- list()
  
  for (k in 1:length(levels(Output$GUESS.SAMP))) {
    SampleList <- list()
    print(levels(Output$GUESS.SAMP)[k])
    SmallTable <-
      Output[Output$GUESS.SAMP == levels(Output$GUESS.SAMP)[k], ]
    #print(SmallTable$GUESS.SAMP)
    Session <- list()
    request <- list()
    print(nrow(SmallTable))
    analysis <- list()
    #DatumList <- list()
    #AttributeList <- list()
    
    for (j in 1:nrow(SmallTable)) {
      #analysis <- list()
      DatumList <- list()
      AttributeList <- list()
      m <- 1
      n <- 1
      for (i in 1:ncol(SmallTable)) {
        if (LookupDF$Type[LookupDF$ColNames == colnames(SmallTable)[i]] == "Numeric") {
          #print(c(l,i))
          if (!is.na(SmallTable[j, i]) & is.numeric(SmallTable[j, i])) {
            value <- SmallTable[j, i]
          } else{
            value <- -1042
          }
          
          DatumList [[m]] <- list(
            value = value,
            type = list(
              parameter = colnames(SmallTable)[i],
              unit = LookupDF$unit[LookupDF$ColNames ==
                                     colnames(SmallTable)[i]]
            )
          )
          
          m <- m + 1
        }
        
        if (LookupDF$Type[LookupDF$ColNames == colnames(SmallTable)[i]] ==
            "Text") {
          if (!is.na(SmallTable[j, i]) & !is.numeric(SmallTable[j, i])) {
            note <- SmallTable[j, i]
          } else{
            note <- "Empty"
          }
          
          AttributeList[[n]] <-
            list(parameter = colnames(SmallTable)[i],
                 value = note)
          
          n <- n + 1
        }
        
        
        #print(m)
      }
      
      d <- try(as.Date(SmallTable$DATETIME[i], format="%Y-%m-%d %H:%M:%S"))
      if("try-error" %in% class(d) || is.na(d)) {

        if (SmallTable$MATERIAL[j] == "STD") {
          
          analysis[[j]] <- list(
            analysis_name = SmallTable$Comment[j],
            analysis_type = IsotopeMethod,
            is_standard = SmallTable$MATERIAL[j] == "STD",
            datum = DatumList,
            attribute = AttributeList,
            material = SmallTable$MATERIAL[j],
            session_index = j,
            standard_sample = list(name = SmallTable$RegexSTD[j])
          )
          
        } else{
          
          analysis[[j]] <- list(
            analysis_name = SmallTable$Comment[j],
            analysis_type = IsotopeMethod,
            is_standard = SmallTable$MATERIAL[j] == "STD",
            datum = DatumList,
            attribute = AttributeList,
            material = SmallTable$MATERIAL[j],
            session_index = j
          )
          
        }
        
      } else {
        
        date <- gsub(" ", "T", SmallTable$DATETIME[j])
        
        if (SmallTable$MATERIAL[j] == "STD") {
          
          analysis[[j]] <- list(
            analysis_name = SmallTable$Comment[j],
            analysis_type = IsotopeMethod,
            date = date,
            is_standard = SmallTable$MATERIAL[j] == "STD",
            datum = DatumList,
            attribute = AttributeList,
            material = SmallTable$MATERIAL[j],
            session_index = j,
            standard_sample = list(name = SmallTable$RegexSTD[j])
          )
          
        } else{
          
          analysis[[j]] <- list(
            analysis_name = SmallTable$Comment[j],
            analysis_type = IsotopeMethod,
            date = date,
            is_standard = SmallTable$MATERIAL[j] == "STD",
            datum = DatumList,
            attribute = AttributeList,
            material = SmallTable$MATERIAL[j],
            session_index = j
          )
          
        }
      }
      
      #print(j)
      #print(nrow(SmallTable))
      
      
    }
    
    Session <- list(
      name = paste(BaseFile, levels(Output$GUESS.SAMP)[k], sep = "_"),
      sample = list(name = levels(Output$GUESS.SAMP)[k]),
      project = list(name = BaseFile),
      date = gsub(" ", "T", min(Output$DATETIME, na.rm = TRUE)),
      analysis = analysis
    )
    
    # SampleList <- append(SampleList,
    #                      list(list(
    #                        name = BaseFile,
    #                        sample = list(name = levels(Output$GUESS.SAMP)[k]),
    #                        date = min(Output$DATETIME, na.rm = TRUE),
    #                        analysis = analysis,
    #                                            date = Output$DATETIME[1])))
    
    request <- list(filename = paste(BaseFile, k),
                    data = Session)
    
    if (Upload) {
      response <-
        PUT(url = "http://backend:5000/api/v1/import-data/session",
            body = request,
            encode = "json") #PUT(url="http://backend:5000/api/v1/import-data/session", body=request, encode = "json")
      
      errors <- content(response)
      
      print(errors)
      
    } else{
      print("Make JSON")
      NewFile <- gsub("\\..*", "", BaseFile)
      write_json(
        request,
        paste(
          "~/Documents/SparrowJSONTest/",
          k,
          "_",
          NewFile,
          ".json",
          sep = ""
        ),
        auto_unbox = TRUE,
        pretty = TRUE
      )
      
    }
    
    
    
    
  }
  
  #PUT(url="http://backend:5000/api/v1/import-data/session", body=SampleList, encode = "json")
  
  return(print("DONE"))#print("Sparrow upload", BaseFile))
  #return(SampleList)
}
