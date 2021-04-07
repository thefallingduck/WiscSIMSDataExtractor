AddBracketStructure <- function(Input) {
  #Input <- Output # for debugging?
  
  #This material regex string could be modified based on dictionary?
  MaterialRegex <- "UW|-WI|KIM|SC"
  Output <- Input
  
  Output$MATERIAL <-
    ifelse(
      grepl(MaterialRegex, Output$Comment, ignore.case = TRUE),
      'STD',
      ifelse(
        !grepl(MaterialRegex, Output$Comment, ignore.case = TRUE),
        'Sample',
        NA
      )
    )
  
  Output$MATERIAL[is.na(Output$File) == TRUE] <- NA
  Output$MATERIAL[is.na(Output$DTFAX) & is.na(Output$SD2ext)] <- NA
  
  Material <- Output$MATERIAL
  #Output$MATERIAL[!is.na(Output[4])&Output$MATERIAL=='STD']<-'STD?'
  #Dropped the above line to avoid adding more levels in the GUESS.SAMP column later...
  
  #### Identify sample-standard-standard brackets####
  #### This uses Output$MATERIAL column and the spacing of NAs to
  #### determine the cluster of samples and standards
  ####
  RunIndex <- BlockID(Output)
  
  GROUPNUM <- vector(length = length(Material))
  
  for (i in 1:nrow(RunIndex)) {
    GROUPNUM[RunIndex$start[i]:RunIndex$end[i]] <- i
    
  }
  
  GROUPNUM[GROUPNUM == 0] <- NA
  
  
  #### Add GROUPNUM to output dataframe
  
  Output$GROUPNUM <- GROUPNUM
  
  #### Guess at sample names using comment strings ####
  GUESS.SAMP <- vector(length = length(Material))
  
  DurationMountChange <-
    quantile(Output$AnalysisLength, c(.95), na.rm = TRUE)
  
  RunIndex$TrueChange <- FALSE
  
  RunIndex$TrueChange[RunIndex$ChangeDuration > DurationMountChange &
                        RunIndex$MountChange] <- TRUE
  
  MountChangeIndex <-
    c(1, which(RunIndex$TrueChange), nrow(RunIndex))
  
  ### Could add an analysis length filter to this so we have true mount changes even for standard block mounts
  
  for (i in 1:(length(MountChangeIndex) - 1)) {
    SelectRange <-
      range(RunIndex$start[MountChangeIndex[i]]:RunIndex$end[MountChangeIndex[i +
                                                                                1]])
    SelectRange <- SelectRange[1]:SelectRange[2]
    OutputSub <- Output[SelectRange, ]
    MountSubset <- OutputSub$Comment[OutputSub$MATERIAL == "Sample"]
    MountSubset <- MountSubset[!is.na(MountSubset)]
    if (length(MountSubset) > 0) {
      MountName <- MaxStringRepeated(MountSubset)
    } else {
      StandardSubset <- OutputSub$Comment[OutputSub$MATERIAL == "STD"]
      StandardSubset <- StandardSubset[!is.na(StandardSubset)]
      MountName <- MaxStringRepeated(StandardSubset)
    }
    GUESS.SAMP[SelectRange] <- MountName
  }
  #GUESS.SAMP[is.na(Output$GROUPNUM)] <- NA
  Output$GUESS.SAMP <- GUESS.SAMP
  
  
  return(Output)
  
}