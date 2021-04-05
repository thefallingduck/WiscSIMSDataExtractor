#### Testing parser on multiple files...

source("GeneralSIMSImporter.R")

TestFile <- file.choose()

GeneralSIMSImporter(TestFile)

TestFile2 <- file.choose()

ReadTest<-read_excel(TestFile2)

library(ggplot2)
library(jsonlite)

AllData<-fromJSON("http://localhost:5002/api/v1/analysis?all=true", flatten = TRUE)
/api/v1/datum?parameter=DTFAX