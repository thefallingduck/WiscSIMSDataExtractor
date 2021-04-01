ColumnRename <- function(InputFile, IsotopeMethod) {
  ####Function to rename SIMS file columns using a lookup table that
  #is currently manually generated, but could eventually use something from Sparrow API?
  
  ####Args:
  #     InputFile: Dataframe ment to be parsed with column names
  #     IsotopeMethod: variable to specify what isotope method colnames chosen from
  
  ####Returns:
  #     OutputFile: Dataframe with uniform column names
  library(readxl)
  library(httr)
  
  
  #IsotopeMethod <- "d13C7"
  #InputFile <- as.data.frame(read_excel(input.file))
  
  ColumnNames <- colnames(InputFile)
  
  #LookupTable <- file.choose()
  
  #LookupDF <- read_excel(LookupTable)
  
  #### Use a version of this file on Github
  # Will likely need to change this in the future
  # especially when used in Sparrow infrastructure
  
  url1 <-
    "https://github.com/EarthCubeGeochron/Sparrow-WiscSIMS/blob/master/Test-Data/WiscSIMSColumnDictionary.xlsx?raw=true"
  GET(url1, write_disk(tf <- tempfile(fileext = ".xlsx")))
  
  LookupDF <- read_excel(tf)
  
  LookupDF$Dictionary <-
    as.list(strsplit(
      as.character(LookupDF$DictionaryColNames),
      split = "; ",
      useBytes = TRUE
    ))
  
  SmallLookUp <-
    LookupDF[grep(IsotopeMethod, LookupDF$SIMSmethods),]
  
  StandardColNames <- vector(length = length(ColumnNames))
  
  
  for (i in 1:length(ColumnNames)) {
    for (j in 1:nrow(SmallLookUp)) {
      if (ColumnNames[i] %in% SmallLookUp$Dictionary[[j]]) {
        StandardColNames[i] <- as.character(SmallLookUp$ColNames[j])
      }
    }
  }
  
  UniformColumns <- SmallLookUp$ColNames
  
  ExtraColumns <-
    ColumnNames[!StandardColNames %in% UniformColumns]
  
  MissingColumns <-
    UniformColumns[!UniformColumns %in% StandardColNames]
  
  
  tryCatch({
    if (length(MissingColumns) > 0) {
      warning(paste("Missing:", toString(MissingColumns)))
    }
  })
  # }, error = function(e) {
  #   cat("ERROR :", conditionMessage(e), "\n")
  # })
  
  colnames(InputFile) <- StandardColNames
  
  InputFile <- InputFile[, colnames(InputFile) %in% UniformColumns]
  InputFile$INDEX <- 1:nrow(InputFile)
  
  #### Analysis Lengths ####
  Times <- InputFile[, c("Date", "Time", "INDEX")]
  DATETIME <- paste(Times$Date, format(Times$Time, "%H:%M"))
  DATETIME[DATETIME == "NA NA"] <- NA
  InputFile$DATETIME <- as.POSIXct(DATETIME)
  Times$Time <- as.POSIXct(DATETIME)
  Times2 <- Times[is.na(Times$Time) == FALSE, ]
  Times2$AnalysisLength <-
    c(NA, difftime(Times2$Time[-1], Times2$Time[-length(Times2$Time)], units = "mins"))
  
  InputFile <-
    merge(
      InputFile,
      Times2,
      by.x = "INDEX",
      by.y = "INDEX",
      all.x = TRUE,
      sort = FALSE
    )
  
  InputFile <-
    InputFile[, !(colnames(InputFile) %in% c("Date.y", "Time.y"))]
  InputFile <- InputFile[sort(InputFile$INDEX), ]
  
  return(InputFile)
  
  
}
