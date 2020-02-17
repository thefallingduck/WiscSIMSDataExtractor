GetUniqueColumns <- function(Directory=NA, Isotope=NA){
  
  ####Function to get all unique column names in
  ####a chosen directory from .xls? files.
  ####Useful for writing batch import scripts
  ####to easily combine data with non-uniform column names.

  #Arguments:
  #   Directory - File directory encompassing subdirectories of interest.
  #   Isotope - grep for isotope system
  # 
  # Returns:
  #   UniqueColumns - vector of unique column names sorted alphabetically
  #   
  #   
  #   
  #   

  library(readxl)
  
  if(is.na(Directory)){
    
    Directory <- choose.dir()
    
  }
  
  FileList <- list.files(path = Directory, pattern = ".xls[x]?", recursive = TRUE, full.names = TRUE)
  
    if(is.na(Isotope)==FALSE){
      
    FileList <- grep(Isotope, FileList, value = TRUE)
    
    }
  
  FileList <- grep("\\~\\$|@|tuning", FileList, value = TRUE, invert = TRUE)
  
  InputColumns <- vector()
  
  for(i in 1:length(FileList)){
    
    InputColumns<-append(InputColumns, colnames(read_excel(FileList[i])))
    
  }
  
  UniqueColumns <- unique(InputColumns)
  UniqueColumns <- sort(UniqueColumns)
  
  return(UniqueColumns)
}
