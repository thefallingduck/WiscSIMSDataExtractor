GeneralSIMSImporter <- function(InputFile, PlugNum=NA){
  
  ####Opens up WiscSIMS datafiles that contain d18O or d13C in the
  #file name and parses it to a data table with the same column names
  #returns error message if any columns are missing or have not been
  #identified in the initial data set. This makes use of several other functions including
  #ColumnRename.R and StandardID.R which have been made to be flexible using the lookup tables that can be edited.
  #Eventually these tables will be housed on an instance of Sparrow as part of the database, however
  #I have not implimented that yet.
  
  #Arguments:
  #   InputFile:  Input excel file chosen by user with WiscSIMS naming convention
  
  #Returns:
  #   ParsedFile: Output dataframe with R friendly column names, bracket ID, Sample vs Standard ID
  #   ErrorList:  Output character string listing errors encountered with column names
  
  #### For troubleshooting...
  #InputFile <- file.choose()
  #InputFile <- "/Users/macrostrat/Dropbox/SIMS batch data import/SIMS batch data import/SIMS data files/20191021_d18O_Helser.xlsx"
  #PlugNum <- NA
  
  ####Test to see if input file is a proper Excel file with d18O or d13C in name ####
  #InputFile <- fp #input.file <- "/Users/macrostrat/Projects/EarthCube-Geochron/Sparrow-instances/Sparrow-WiscSIMS/Test-Data/20130917_d13C_Ammonites.xls"
  
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
  
  source("ColumnRename.R")
  source("StandardID.R")
  ####Replace column names using non-exhaustive list of column names for d18O files based on Spring 2014 file observation####
  
  Output <- ColumnRename(Input, IsotopeMethod = IsotopeMethod)
  Output <- Output[order(Output$INDEX),]
  
  #### Identify samples and standards using grepl####
  #### This is a course identification of samples and standards
  #### based on presence of the following character strings
  #### UW WI KIM SC for both general running standards and
  #### session specific standard matrix matching
  
  Output$MATERIAL <- ifelse(grepl("UW|WI|KIM|SC", Output$Comment, ignore.case = TRUE), 'STD', ifelse(!grepl("UW|WI|KIM|SC", Output$Comment, ignore.case = TRUE), 'Sample', NA))
  
  Output$MATERIAL[is.na(Output$File)==TRUE]<-NA
  
  Output$MATERIAL[!is.na(Output[4])&Output$MATERIAL=='STD']<-'STD?'
  
  #### Identify sample-standard-standard brackets####
  #### This uses Output$MATERIAL column and the spacing of NAs to
  #### determine the cluster of samples and standards
  #### 
  
  Material<-Output$MATERIAL
  
  # Step one create run of values (i.e. sequences with the same "Material" type)
  
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
  
  Output$GROUPNUM <- GROUPNUM
  
  #### Guess at different samples ####
  #### Based on Levenshtein Distance of comment string
  #### Number of clusters IDed manually or set
  
  str <- Output$Comment[!is.na(Output$File)&Output$MATERIAL=='Sample']
  str <- str[!is.na(str)]

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
    
    if(GuessPlugs>2){
    
    df <- data.frame(Comment, GUESS.SAMP = as.factor(cutree(hc,k=GuessPlugs)))
    
    levels(df$GUESS.SAMP) <- df$Comment[match(levels(df$GUESS.SAMP), df$GUESS.SAMP)]
    
    }else{
      
      df <- data.frame(Comment, GUESS.SAMP = as.factor(rep(Comment[1], times = length(Comment))))
      
      levels(df$GUESS.SAMP) <- df$Comment[match(levels(df$GUESS.SAMP), df$GUESS.SAMP)]
      
    }
    
  }
  
  #### Bind GUESS.SAMP to Output ####
  
  Output <- merge(Output, df, by.x="Comment", by.y = "Comment", all.x = TRUE, sort = FALSE)
  Output$GUESS.SAMP <- as.character(Output$GUESS.SAMP)
  Output$GUESS.SAMP[is.na(Output$GUESS.SAMP)==TRUE] <- Output$MATERIAL[is.na(Output$GUESS.SAMP)==TRUE]
  Output$GUESS.SAMP <- as.factor(Output$GUESS.SAMP)
  
  #### Relative Hydride, Relative Yield, recalc isotope####
  Output$UNIQUEGRP <- as.factor(paste(Output$MATERIAL, Output$GROUPNUM))
  levels(Output$UNIQUEGRP)[levels(Output$UNIQUEGRP)=="NA NA"] <- NA
  Output <- Output[order(Output$GROUPNUM),]
  AllGroups <- unique(as.character(Output$UNIQUEGRP))
  
  Samples <- unique(Output$UNIQUEGRP[Output$MATERIAL=="Sample"& !is.na(Output$MATERIAL)])
  
  #### Add columns to dataframe
  Output$REL_YIELD <- rep(NA,times=nrow(Output))
  Output$REL_Hyd <- rep(NA,times=nrow(Output))
  Output$BRACKET2SD <- rep(NA,times=nrow(Output))
  Output$STDiso <- rep(NA,times=nrow(Output))
  
  Output <- StandardID(Output, IsotopeMethod = IsotopeMethod)
  
  url1 <-
    "https://github.com/EarthCubeGeochron/Sparrow-WiscSIMS/blob/master/Test-Data/WiscSIMSrunSTDS.xlsx?raw=true"
  GET(url1, write_disk(tf <- tempfile(fileext = ".xlsx")))
  
  Standards <- read_excel(tf)
  
  #RunStd <- 12.49
  
  #### Choose isotope column from standard table ####
  if(IsotopeMethod == "d18O10"){
    
    Isotope <- "d18O"
    
  }
  
  if(IsotopeMethod == "d13C7"){
    
    Isotope <- "d13C"
    
  }

  #### Bracket level recalculation of standard, hydride, and yield####
  for(i in 1:length(Samples)){
    
    StartGroup <- AllGroups[match(Samples[i],AllGroups)-1]
    EndGroup <- AllGroups[match(Samples[i],AllGroups)+1]
    
    
    if(is.na(EndGroup)){
      
      EndGroup <- "NOPE"
      
    }
    
    ReplaceLogic <- Output$UNIQUEGRP==Samples[i]&is.na(Output$UNIQUEGRP)==FALSE
    
    SelectLogic <- Output$UNIQUEGRP==StartGroup & !is.na(Output$UNIQUEGRP)| Output$UNIQUEGRP==EndGroup & !is.na(Output$UNIQUEGRP)
    
    if (length(Output$BRACKET2SD[ReplaceLogic])>=1 && length(Output$BRACKET2SD[SelectLogic])>=1){
    
    RunStd <- as.numeric(Standards[Standards$StdName==unique(Output$RegexSTD[SelectLogic]), Isotope])
    
    Output$GUESS.SAMP[SelectLogic] <- unique(Output$GUESS.SAMP[ReplaceLogic])
    
    Output$BRACKET2SD[ReplaceLogic] <- 2*sd(Output[SelectLogic, 7])
    
    MeanIso <- round(mean(Output[SelectLogic, 7], na.rm=TRUE), digits = 8)
    BracketBias <- (((1+MeanIso/1000)/(1+RunStd/1000))-1)*1000
    
    Output$STDiso[ReplaceLogic] <- (((1+Output[ReplaceLogic,7]/1000)/(1+BracketBias/1000))-1)*1000
    
    Output$REL_YIELD[ReplaceLogic] <- Output$Yield[ReplaceLogic]/mean(Output$Yield[SelectLogic], na.rm=TRUE)
    
    Output$REL_Hyd[ReplaceLogic] <- Output[ReplaceLogic, 19]-mean(Output[SelectLogic, 19], na.rm=TRUE)}
    
  }
  
  for(i in 1:nrow(Output[Output$Comment == "bracket average and 2SD"&!is.na(Output$Comment),])){
    
    Output$GUESS.SAMP[Output$Comment == "bracket average and 2SD"&!is.na(Output$Comment)][i] <- Output$GUESS.SAMP[Output$INDEX==(Output$INDEX[Output$Comment == "bracket average and 2SD"&!is.na(Output$Comment)][i])-2]
    
  }
  
  for(i in 1:nrow(Output[Output$Comment == "average and 2SD"&!is.na(Output$Comment),])){
    
    Output$GUESS.SAMP[Output$Comment == "average and 2SD"&!is.na(Output$Comment)][i] <- Output$GUESS.SAMP[Output$INDEX==(Output$INDEX[Output$Comment == "average and 2SD"&!is.na(Output$Comment)][i])-1]
    
  }
  
  Output <- Output[order(Output$INDEX),]
  
  return(Output)
  
}
