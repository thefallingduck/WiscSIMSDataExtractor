GeneralSIMSImporter <- function(InputFile, PlugNum=NA){
  
  ####Opens up WiscSIMS datafiles that contain d18O or d13C in the
  #file name and parses it to a data table with the same column names
  #returns error message if any columns are missing or have not been
  #identified in the initial data set. This makes use of several other functions including
  #ColumnRename.R and StandardID.R which have been made to be flexible using the lookup tables that can be edited.
  #Eventually these tables will be housed on an instance of Sparrow as part of the database, however
  #I have not implimented that yet.
  
  #Arguments:
  #   InputFile:  Input excel file chosen by user with WiscSIMS naming convention
  
  #Returns:
  #   ParsedFile: Output dataframe with R friendly column names, bracket ID, Sample vs Standard ID
  #   ErrorList:  Output character string listing errors encountered with column names
  
  #### For troubleshooting...
  #InputFile <- file.choose()
  #InputFile <- "/Users/macrostrat/Dropbox/SIMS batch data import/SIMS batch data import/SIMS data files/20191021_d18O_Helser.xlsx"
  #PlugNum <- NA
  
  ####Test to see if input file is a proper Excel file with d18O or d13C in name ####
  #InputFile <- fp #input.file <- "/Users/macrostrat/Projects/EarthCube-Geochron/Sparrow-instances/Sparrow-WiscSIMS/Test-Data/20130917_d13C_Ammonites.xls"
  #InputFile <- file.choose()
  if(grepl("d18O|d13C|CaO|_Ca", InputFile)==FALSE|grepl(".xls[x]?", InputFile)==FALSE){
    
    stop('Not valid Excel file')
    
  }
  
  if(grepl("d18O", InputFile)){
    
    IsotopeMethod <- "d18O10"
    
  }
  
  if(grepl("d13C", InputFile)){
    
    IsotopeMethod <- "d13C7"
    
  }
  
  if(grepl("CaO", InputFile)){
    
    IsotopeMethod <- "CaO"
    
  }
  
  if(grepl("_Ca_", InputFile)){
    
    IsotopeMethod <- "Ca"
    
  }
  
  #### Determine if required packages are loaded ####
  
  library(readxl)
  library(plyr)
  
  Input <- as.data.frame(read_excel(InputFile, guess_max = 1000, range = cell_cols("A:Z")))
  
  source("ColumnRename.R")
  source("StandardID.R")
  source("MaxStringRepeated.R")
  source("BlockID.R")
  source("AddBracketStructure.R")
  source("BracketRecalc.R")
  source("CatchExcelBrackets.R")
  ####Replace column names using non-exhaustive list of column names for d18O files based on Spring 2014 file observation####
  
  Output<-tryCatch({
    Output <- ColumnRename(Input, IsotopeMethod = IsotopeMethod)
    Output
  },
  warning=function(cond) {
    message("Error on ColumnRename")
    message("Here's the original warning message:")
    print("\n")
    message(cond)
    print("\n")
    # Choose a return value in case of warning
    return(NULL)
  })
  
  Output <- Output[order(Output$INDEX),]
  
  Output <- tryCatch({
    Output <- AddBracketStructure(Output)
    Output
    },
                     warning=function(cond) {
                       message("Error on AddBracketStructure")
                       message("Here's the original warning message:")
                       print("\n")
                       message(cond)
                       print("\n")
                       # Choose a return value in case of warning
                       return(NULL)
                     })
  
  Output <- tryCatch({
    Output <- BracketRecalc(Output, IsotopeMethod = IsotopeMethod)
    Output
    },
                     warning=function(cond) {
                       message("Error on BracketRecalc")
                       message("Here's the original warning message:")
                       print("\n")
                       message(cond)
                       print("\n")
                       # Choose a return value in case of warning
                       return(Output)
                     })
  
  Output <- tryCatch({
    Output <- CatchExcelBrackets(Output)
    Output
  },
  error=function(cond) {
    message("Error on CatchExcelBrackets.")
    message("Here's the original warning message:")
    message(cond)
    return(Output)
  },
  warning=function(cond) {
    message("Warning on CatchExcelBrackets")
    message("Here's the original warning message:")
    message(cond)
    # Choose a return value in case of warning
    return(Output)
  })
  
  Output <- Output[order(Output$INDEX),]
  
  return(Output)
  
}
