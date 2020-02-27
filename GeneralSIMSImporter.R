GeneralSIMSImporter <- function(InputFile, PlugNum=NA){
  
  ####Opens up WiscSIMS datafiles that contain d18O in the
  #file name and parses it to a data table with the same column names
  #returns error message if any columns are missing or have not been
  #identified in the initial data set
  
  #Arguments:
  #   InputFile:  Input excel file chosen by user with WiscSIMS naming convention
  
  #Returns:
  #   ParsedFile: Output dataframe with R friendly column names, bracket ID, Sample vs Standard ID
  #   ErrorList:  Output character string listing errors encountered with column names
  
  #### For troubleshooting...
  InputFile <- file.choose()
  PlugNum <- NA
  ####Test to see if input file is a proper Excel file with d18O in name ####
  
  if(grepl("d18O|d13C", InputFile)==FALSE|grepl(".xls[x]?", InputFile)==FALSE){
    
    stop('Not valid Excel file')
    
  }
  
  if(grepl("d18O", InputFile)){
    
    IsotopeMethod <- "d18O10"
    
  }
  
  if(grepl("d13C", InputFile)){
    
    IsotopeMethod <- "d13C7"
    
  }
  #### Determine if required packages are loaded ####
  
  library(readxl)
  library(plyr)
  
  Input <- as.data.frame(read_excel(InputFile))
  
  source("/Users/macrostrat/Documents/WiscSIMSDataExtractor/ColumnRename.R")
  ####Replace column names using non-exhaustive list of column names for d18O files based on Spring 2014 file observation####
  
  Output <- ColumnRename(Input, IsotopeMethod = IsotopeMethod)
  
  Output$INDEX <- 1:nrow(Output)
  
  #### Identify samples and standards using grepl####
  #### This is a course identification of samples and standards
  #### based on presence of the following character strings
  #### UW WI KIM SC for both general running standards and
  #### session specific standard matrix matching
  
  Output$MATERIAL <- ifelse(grepl("UW|WI|KIM|SC", Output$Comment, ignore.case = TRUE), 'STD', ifelse(!grepl("UW|WI|KIM|SC", Output$Comment, ignore.case = TRUE), 'Sample', NA))
  
  Output$MATERIAL[is.na(Output$File)==TRUE]<-NA
  
  Output$MATERIAL[is.na(Output$d18OVSMOW)==FALSE&Output$MATERIAL=='STD']<-'STD?'
  
  #### Identify sample-standard-standard brackets####
  #### This uses Output$MATERIAL column and the spacing of NAs to
  #### determine the cluster of samples and standards
  #### 
  
  Material<-Output$MATERIAL
  
  # Step one create run of values
  
  RunIndex <- with(rle(Material), data.frame(number = values,
                                             start = cumsum(lengths) - lengths + 1,
                                             end = cumsum(lengths))[order(values),])
  
  RunIndex <- RunIndex[is.na(RunIndex$number)==FALSE,]
  RunIndex <- RunIndex[order(RunIndex$start),]
  
  GROUPNUM <- vector(length = length(Material))
  
  for(i in 1:nrow(RunIndex)){
    
    GROUPNUM[RunIndex$start[i]:RunIndex$end[i]]<- i
    
  }
  
  GROUPNUM[GROUPNUM==0] <- NA
  
  
  #### Add GROUPNUM to output dataframe
  
  Output <- cbind(Output, GROUPNUM)
  
  #### Guess at sample mounts ####
  #### Based on there being a comment
  #### with no d18Omeas value
  #### This is actually really bad...
  #### In the future maybe we can make some standardization here?
  
  MountStart <- Output$INDEX[is.na(Output$Comment)==FALSE&is.na(Output$d18Omeas)]
  
  MOUNTNUM <- vector(length = nrow(Output))
  
  for(i in 2:length(MountStart)){
    
    MOUNTNUM[MountStart[i-1]:MountStart[i]] <- i
    
  }
  
  Output <- cbind(Output, MOUNTNUM)
  
  Output$MOUNTNUM <- as.factor(Output$MOUNTNUM)
  
  levels(Output$MOUNTNUM) <- Output$Comment[MountStart]
  
  #### Analysis Lengths ####
  Times <- Output[,c("Date","Time","INDEX")]
  DATETIME <- paste(Times$Date, format(Times$Time, "%H:%M"))
  DATETIME[DATETIME=="NA NA"] <- NA
  Output$DATETIME <- as.POSIXct(DATETIME)
  Times$Time <- as.POSIXct(DATETIME)
  Times2 <- Times[is.na(Times$Time)==FALSE,]
  Times2$AnalysisLength <- c(NA, difftime(Times2$Time[-1], Times2$Time[-length(Times2$Time)], units = "mins"))
  
  Output <- merge(Output, Times2, by.x="INDEX", by.y = "INDEX", all.x = TRUE, sort = FALSE)
  
  #### Guess at different samples ####
  #### Based on Levenshtein Distance of comment string
  #### Number of clusters IDed manually or set
  
  str <- Output$Comment[is.na(Output$File)==FALSE&Output$MATERIAL=='Sample']
  
  Comment <- str
  
  str <- sub(" Cs.*", "", str)
  
  d  <- adist(str)
  
  rownames(d) <- Comment
  
  hc <- hclust(as.dist(d))
  
  if(is.numeric(PlugNum)){
    
    df <- data.frame(Comment, GUESS.SAMP = as.factor(cutree(hc,k=PlugNum)))
    
    levels(df$GUESS.SAMP) <- df$Comment[match(levels(df$GUESS.SAMP), df$GUESS.SAMP)]
    
  }else{
    
    GuessPlugs <- length(c(which(diff(Output$GROUPNUM[Output$MATERIAL=="Sample"&is.na(Output$MATERIAL)==FALSE]%%2)!=0)))
    
    df <- data.frame(Comment, GUESS.SAMP = as.factor(cutree(hc,k=GuessPlugs)))
    
    levels(df$GUESS.SAMP) <- df$Comment[match(levels(df$GUESS.SAMP), df$GUESS.SAMP)]
    
    
  }
  
  #### Bind GUESS.SAMP to Output ####
  
  Output <- merge(Output, df, by.x="Comment", by.y = "Comment", all.x = TRUE, sort = FALSE)
  
  Output$GUESS.SAMP[is.na(Output$GUESS.SAMP)==TRUE] <- Output$MATERIAL[is.na(Output$GUESS.SAMP)==TRUE]
  
  #### Relative OHO and Relative Yield ####
  Output$UNIQUEGRP <- as.factor(paste(Output$MATERIAL, Output$GROUPNUM))
  levels(Output$UNIQUEGRP)[levels(Output$UNIQUEGRP)=="NA NA"] <- NA
  Output <- Output[order(Output$GROUPNUM),]
  AllGroups <- unique(as.character(Output$UNIQUEGRP))
  
  Samples <- unique(Output$UNIQUEGRP[Output$MATERIAL=="Sample"& !is.na(Output$MATERIAL)])
  #### Add columns to dataframe
  Output$REL_YIELD <- rep(NA,times=nrow(Output))
  Output$REL_OHO <- rep(NA,times=nrow(Output))
  Output$BRACKET2SD <- rep(NA,times=nrow(Output))
  Output$STDd18O <- rep(NA,times=nrow(Output))
  
  RunStd <- 12.49
  
  for(i in 1:length(Samples)){
    
    StartGroup <- AllGroups[match(Samples[i],AllGroups)-1]
    EndGroup <- AllGroups[match(Samples[i],AllGroups)+1]
    
    if(is.na(EndGroup)){
      
      EndGroup <- "NOPE"
      
    }
    
    ReplaceLogic <- Output$UNIQUEGRP==Samples[i]&is.na(Output$UNIQUEGRP)==FALSE
    
    SelectLogic <- Output$UNIQUEGRP==StartGroup & !is.na(Output$UNIQUEGRP)| Output$UNIQUEGRP==EndGroup & !is.na(Output$UNIQUEGRP)
    
    Output$GUESS.SAMP[SelectLogic] <- unique(Output$GUESS.SAMP[ReplaceLogic])
    
    Output$BRACKET2SD[ReplaceLogic] <- 2*sd(Output$d18Omeas[SelectLogic])
    
    Meand18O <- round(mean(Output$d18Omeas[SelectLogic], na.rm=TRUE), digits = 8)
    BracketBias <- (((1+Meand18O/1000)/(1+RunStd/1000))-1)*1000
    
    Output$STDd18O[ReplaceLogic] <- (((1+Output$d18Omeas[ReplaceLogic]/1000)/(1+BracketBias/1000))-1)*1000
    
    Output$REL_YIELD[ReplaceLogic] <- Output$Yield[ReplaceLogic]/mean(Output$Yield[SelectLogic], na.rm=TRUE)
    
    Output$REL_OHO[ReplaceLogic] <- Output$OHO[ReplaceLogic]-mean(Output$OHO[SelectLogic], na.rm=TRUE)
    
  }
  
  for(i in 1:nrow(Output[Output$Comment == "bracket average and 2SD"&!is.na(Output$Comment),])){
    
    Output$GUESS.SAMP[Output$Comment == "bracket average and 2SD"&!is.na(Output$Comment)][i] <- Output$GUESS.SAMP[Output$INDEX==(Output$INDEX[Output$Comment == "bracket average and 2SD"&!is.na(Output$Comment)][i])-2]
    
  }
  
  for(i in 1:nrow(Output[Output$Comment == "average and 2SD"&!is.na(Output$Comment),])){
    
    Output$GUESS.SAMP[Output$Comment == "average and 2SD"&!is.na(Output$Comment)][i] <- Output$GUESS.SAMP[Output$INDEX==(Output$INDEX[Output$Comment == "average and 2SD"&!is.na(Output$Comment)][i])-1]
    
  }
  
  Output$STDd18Opdb <- (Output$STDd18O-30.91)/1.03091
  
  Output <- Output[order(Output$INDEX),]
  
  plot(Output$INDEX, Output$d18OVSMOW-Output$STDd18O, type = 'o', ylim = c(-.000000001,.000000001))
  
  # Output <- Output[,c("File",
  #                     "Comment",
  #                     "d18OVSMOW",
  #                     "SD2ext",
  #                     "IMF",
  #                     "d18Omeas",
  #                     "SE2int",
  #                     "O16cps",
  #                     "IP(nA)",
  #                     "Yield",
  #                     "DATETIME",
  #                     "AnalysisLength",
  #                     "X",
  #                     "Y",
  #                     "DTFAX",
  #                     "DTFAY",
  #                     "Mass",
  #                     "OHO",
  #                     "MATERIAL",
  #                     "GROUPNUM",
  #                     "GUESS.SAMP",
  #                     "MOUNTNUM",
  #                     "UNIQUEGRP",
  #                     "REL_YIELD",
  #                     "REL_OHO",
  #                     "BRACKET2SD",
  #                     "STDd18O",
  #                     "STDd18Opdb")]
  
  return(Output)
  
}
