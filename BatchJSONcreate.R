#### Batch produce JSON files to test Sparrow uploads ####
library(purrr)
source('SparrowReformater.R')
possibly_DatumNesting <- possibly(DatumNesting, otherwise = paste(fp, "failed to load", sep = " "))
FileDirectory <- "/Users/macrostrat/Dropbox/SIMS batch data import/SIMS batch data import/SIMS data files"

FileList <- as.vector(list.files(path = FileDirectory, pattern = "\\.xl?", recursive = TRUE))

d18OFileList <- FileList[grepl("d18O", FileList)]
d18OFileList <- d18OFileList[grepl(".xls?$", d18OFileList)]
d18OFileList


# loop through the file list to read in data and clean it up

for (file in d18OFileList) {tryCatch({
  
  fp <- paste(FileDirectory, file, sep="/")
  
  ExampleFilename <- fp
  BaseFile <- basename(file)
  print(BaseFile)
  
  if(grepl("_d18O_", BaseFile)){
  DatumNesting(fp, Upload = FALSE)
  }
})
}

for (file in d18OFileList) {tryCatch({
  
  fp <- paste(FileDirectory, file, sep="/")
  
  ExampleFilename <- fp
  BaseFile <- basename(file)
  print(BaseFile)
  
  if(grepl("_d18O_", BaseFile)){
    possibly_DatumNesting(fp, Upload = FALSE)
  }
})
}



library(purrr)

possibly_DatumNesting = possibly(DatumNesting, otherwise = "skipped file")
result = map(AllFiles, possibly_DatumNesting)

