StandardID <- function(InputFile, IsotopeMethod){
  
  #Identifies running standards based on GREP library
  
  #Args:
  #       InputFile: Datafame after column rename
  #       IsotopeMethod: Included to limit standards
  
  #Returns:
  #       Output: Dataframe with standard column including simplified standard names

  Output <- InputFile
  
  Output$RegexSTD <- Output$Comment
  
  url1 <-
    "https://github.com/EarthCubeGeochron/Sparrow-WiscSIMS/blob/master/Test-Data/WiscSIMSrunSTDS.xlsx?raw=true"
  GET(url1, write_disk(tf <- tempfile(fileext = ".xlsx")))
  
  Standards <- read_excel(tf)

  for(i in 1:nrow(Standards)){
    
  Output$RegexSTD[grepl(Standards$REGEX[i], Output$Comment, ignore.case = TRUE)] <- Standards$StdName[i]
  
  }
  
  return(Output)
  
}
