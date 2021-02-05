####Loop through historical SIMS sessions####

library(knitr)
library(readr)
library(dplyr)
library(rstudioapi)

FileDirectory <- "/Users/macrostrat/Dropbox/SIMS batch data import/SIMS batch data import/SIMS data files"

FileList <- as.vector(list.files(path = FileDirectory, pattern = ".xl?", recursive = TRUE))

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
  rmarkdown::render(input = "SessionSummary.Rmd", 
                    output_format = "html_document",
                    output_file = paste0(gsub(".xls","", BaseFile), ".html"),
                    output_dir = "/Users/macrostrat/Documents/reports")
  }
  })
}

#AdamsPOS <- read_excel(ExampleFilename)
#colnames(AdamsPOS)
#DATETIME <- paste(AdamsPOS$date, format(AdamsPOS$time, "%H:%M"))
