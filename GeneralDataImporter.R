WiscSIMSImport <- function(InputFile){
  #### This function opens any WiscSIMS file by calling parsers that
  #are chosen based on the file name.
  #Currently choses between d18O large spot and
  #d13C. Can also tie files to their metadata.
  
  source('~/Documents/WiscSIMSDataExtractor/ReadSIMSd18OFiles.R')
  source('~/Documents/WiscSIMSDataExtractor/ReadSIMSd13CFiles.R')
  source('~/Documents/WiscSIMSDataExtractor/LoadMetadata.R')
  
  MetaData <- LoadMetadataTable()
  
  FileName <- basename(InputFile)
  
  PlugNum <- MetaData$Number.of.SIMS.mounts[grep(FileName, Metadata$SIMS.Session.datafiles)]
  
  if(grepl(".xls[x]?", InputFile)==FALSE){
    
    stop('Not valid Excel file')
    
  }
  
  if(grepl("d18O", FileName)==TRUE){
    
    Output <- d18O10SIMSimport(InputFile, PlugNum = PlugNum)
    
  }
  
  if(grepl("d13C", FileName)==TRUE){
    
    Output <- d13CSIMSimport(InputFile, PlugNum = PlugNum)
    
  }
  
  return(Output)
  
}