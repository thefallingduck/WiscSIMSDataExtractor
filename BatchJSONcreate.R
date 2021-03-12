#### Batch produce JSON files to test Sparrow uploads ####
library(purrr)
source('SparrowReformater.R')
source('GeneralSIMSImporter.R')
possibly_DatumNesting <- possibly(DatumNesting, otherwise = "failed to load")
FileDirectory <- "/Users/macrostrat/Desktop/SIMSdatafiles"

FileList <- as.vector(list.files(path = FileDirectory, pattern = "\\.xl?", recursive = TRUE))

d18OFileList <- FileList[grepl("d18O", FileList)]
d18OFileList <- d18OFileList[grepl(".xlsx?$", d18OFileList)]
d18OFileList

d13CFileList <- FileList[grepl("d13C", FileList)]
d13C_Col_list <- vector()

for(file in d13CFileList){
  
  fp <- paste(FileDirectory, file, sep="/")
  Names <- colnames(read_excel(fp))
  d13C_Col_list <- append(d13C_Col_list, Names)
  
}

# loop through the file list to read in data and clean it up



for (file in FileList) {tryCatch({
  
  fp <- paste(FileDirectory, file, sep="/")
  
  ExampleFilename <- fp
  BaseFile <- basename(file)
  print(BaseFile)
  #Aehhh <- GeneralSIMSImporter(fp)
  
  #if(grepl("_d18O_", BaseFile)){
    possibly_DatumNesting(fp, Upload = FALSE)
  #}
})
}



library(purrr)

possibly_DatumNesting = possibly(DatumNesting, otherwise = "skipped file")
result = map(AllFiles, possibly_DatumNesting)

