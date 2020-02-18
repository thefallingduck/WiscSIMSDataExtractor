d13CSIMSimport <- function(InputFile, PlugNum=NA){
  
####Opens up WiscSIMS datafiles that contain d13C in the
  #file name and parses it to a data table with the same column names
  #returns error message if any columns are missing or have not been
  #identified in the initial data set
  
  #Arguments:
  #   InputFile:  Input excel file chosen by user with WiscSIMS naming convention
  
  #Returns:
  #   ParsedFile: Output dataframe with R friendly column names, bracket ID, Sample vs Standard ID
  #   ErrorList:  Output character string listing errors encountered with column names

#### For troubleshooting...
  #InputFile <- file.choose()
  #PlugNum <- NA
  ####Test to see if input file is a proper Excel file with d13C in name ####
InputFile <- file.choose()
  if(grepl("d13C", InputFile)==FALSE|grepl(".xls[x]?", InputFile)==FALSE){
    
    stop('Not valid Excel file')
    
  }

  #### Determine if required packages are loaded ####
  
  library(readxl)
  library(plyr)

  Input <- as.data.frame(read_excel(InputFile))

####Replace column names using non-exhaustive list of column names for d13C files based on Spring 2014 file observation####
  
  for(j in 1:length(colnames(Input))){
    
    if(colnames(Input)[j]== c("date", "Date")){
      
      colnames(Input)[j] <- "Date"
      
    }

    if(colnames(Input)[j]%in% c("time", "Time")){
      
      colnames(Input)[j] <- "Time"
      
    }

    if(colnames(Input)[j]%in% c("d13C PDB", "\u03B413C ‰ VPDB")){
    
      colnames(Input)[j] <- "d13CVPDB"
      
    }
    
    if(colnames(Input)[j]%in% c("2SD (ext.)", "Er (2S)", "2SD", "Std_1SD")){
      
      colnames(Input)[j] <- "SD2ext"
      
    }
    

    if(colnames(Input)[j]%in% c("Mass Bias (‰)", "IMF", "Bias")){
      
      colnames(Input)[j] <- "IMF"
      
    }
    

    if(colnames(Input)[j]%in% c("13/12_raw", "d13_C", "d13C_m", "\u03B413C ‰ measured")){
      
      colnames(Input)[j] <- "d13Cmeas"
      
    }
    

    if(colnames(Input)[j]%in% c("d13C-2SE", "Er(2S)", "2SE (int.)")){
      
      colnames(Input)[j] <- "SE2int"
      
    }
    

   if(colnames(Input)[j]%in% c("12C (Mcps)")){
      
      colnames(Input)[j] <- "C12cps"
      
    }
    

    if(colnames(Input)[j]%in% c("IP(nA)", "IP(nA)  1.7 to 1.9", "IP (nA)")){
      
      colnames(Input)[j] <- "IP(nA)"
      
    }

    if(colnames(Input)[j]%in% c("Yield 10MHz/nA")){
      
      colnames(Input)[j] <- "Yield_Mcps/nA"
      Input$`Yield_1MHz/nA` <- Input$`Yield_1MHz/nA`*10
      
    }
    
    if(colnames(Input)[j]%in% c("Yield 1MHz/nA", "Yield (Mcps/nA)")){
      
      colnames(Input)[j] <- "Yield_Mcps/nA"
      
    }

    if(colnames(Input)[j]%in% c("DTFA-X")){
      
      colnames(Input)[j] <- "DTFAX"
      
    }
    
    if(colnames(Input)[j]%in% c("DTFA-Y")){
      
      colnames(Input)[j] <- "DTFAY"
      
    }
    
    if(colnames(Input)[j]%in% c("13CH/13C")){
      
      colnames(Input)[j] <- "CHC"
      
    }
    
  }

  
colnames(Input)[2]<-"Comment"  
colnames(Input)[1]<-"File"

####List of required column names for the file####
#### more of these might be added later

UniformColumns <- c("File", "Comment", "d13CVPDB", "SD2ext", "IMF", "d13Cmeas", "SE2int", "C12cps", "IP(nA)", "Yield_Mcps/nA", "Date", "Time", "X", "Y", "DTFAX", "DTFAY", "Mass", "CHC")

ExtraColumns <- colnames(Input)[!colnames(Input) %in% UniformColumns]

MissingColumns <- UniformColumns[!UniformColumns %in% colnames(Input)]

####If Missing CHC, then just make a blank CHC Column####
if(grep('CHC', MissingColumns)>0){
  
  Input$CHC <- rep(NA, nrow(Input))
  
}
####Stop the program if there are missing required column names####
####Return a list of missing column names 

if(length(MissingColumns)>0){
  
  stop(paste("Missing:", toString(MissingColumns)))
  
}

Output <- Input[,UniformColumns]

Output$INDEX <- 1:nrow(Output)

#### Identify samples and standards using grepl####
#### This is a course identification of samples and standards
#### based on presence of the following character strings
#### UW WI KIM SC for both general running standards and
#### session specific standard matrix matching

Output$MATERIAL <- ifelse(grepl("UW|WI|KIM|SC", Output$Comment, ignore.case = TRUE), 'STD', ifelse(!grepl("UW|WI|KIM|SC", Output$Comment, ignore.case = TRUE), 'Sample', NA))

Output$MATERIAL[is.na(Output$File)==TRUE]<-NA

Output$MATERIAL[is.na(Output$d13CVPDB)==FALSE&Output$MATERIAL=='STD']<-'STD?'

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
#### with no d13Cmeas value
#### This is actually really bad...
#### In the future maybe we can make some standardization here?

MountStart <- Output$INDEX[is.na(Output$Comment)==FALSE&is.na(Output$d13Cmeas)]

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

#### Relative CHC and Relative Yield ####
Output$UNIQUEGRP <- as.factor(paste(Output$MATERIAL, Output$GROUPNUM))
levels(Output$UNIQUEGRP)[levels(Output$UNIQUEGRP)=="NA NA"] <- NA
Output <- Output[order(Output$GROUPNUM),]
AllGroups <- unique(as.character(Output$UNIQUEGRP))

Samples <- unique(Output$UNIQUEGRP[Output$MATERIAL=="Sample"& !is.na(Output$MATERIAL)])
#### Add columns to dataframe
Output$REL_YIELD <- rep(NA,times=nrow(Output))
Output$REL_CHC <- rep(NA,times=nrow(Output))
Output$BRACKET2SD <- rep(NA,times=nrow(Output))
Output$STDd13C <- rep(NA,times=nrow(Output))

RunStd <- -0.91

for(i in 1:length(Samples)){
  i<-1
  StartGroup <- AllGroups[match(Samples[i],AllGroups)-1]
  EndGroup <- AllGroups[match(Samples[i],AllGroups)+1]
  
  if(is.na(EndGroup)){
    
    EndGroup <- "NOPE"
    
  }
  
  ReplaceLogic <- Output$UNIQUEGRP==Samples[i]&is.na(Output$UNIQUEGRP)==FALSE
  
  SelectLogic <- Output$UNIQUEGRP==StartGroup & !is.na(Output$UNIQUEGRP)| Output$UNIQUEGRP==EndGroup & !is.na(Output$UNIQUEGRP)
  
  Output$GUESS.SAMP[SelectLogic] <- unique(Output$GUESS.SAMP[ReplaceLogic])

  Output$BRACKET2SD[ReplaceLogic] <- 2*sd(Output$d13Cmeas[SelectLogic])
  
  Meand13C <- mean(Output$d13Cmeas[SelectLogic], na.rm=TRUE)
  BracketBias <- (((1+Meand13C/1000)/(1+RunStd/1000))-1)*1000
  
  Output$STDd13C[ReplaceLogic] <- (((1+Output$d13Cmeas[ReplaceLogic]/1000)/(1+BracketBias/1000))-1)*1000
  
  Output$REL_YIELD[ReplaceLogic] <- Output$Yield[ReplaceLogic]/mean(Output$Yield[SelectLogic], na.rm=TRUE)
  
  Output$REL_CHC[ReplaceLogic] <- Output$CHC[ReplaceLogic]-mean(Output$CHC[SelectLogic], na.rm=TRUE)

}

for(i in 1:nrow(Output[Output$Comment == "bracket average and 2SD"&!is.na(Output$Comment),])){
  
Output$GUESS.SAMP[Output$Comment == "bracket average and 2SD"&!is.na(Output$Comment)][i] <- Output$GUESS.SAMP[Output$INDEX==(Output$INDEX[Output$Comment == "bracket average and 2SD"&!is.na(Output$Comment)][i])-2]

}

for(i in 1:nrow(Output[Output$Comment == "average and 2SD"&!is.na(Output$Comment),])){
  
  Output$GUESS.SAMP[Output$Comment == "average and 2SD"&!is.na(Output$Comment)][i] <- Output$GUESS.SAMP[Output$INDEX==(Output$INDEX[Output$Comment == "average and 2SD"&!is.na(Output$Comment)][i])-1]
  
}

Output <- Output[order(Output$INDEX),]
Output <- Output[,c("File",
                    "Comment",
                    "d13CVPDB",
                    "SD2ext",
                    "IMF",
                    "d13Cmeas",
                    "SE2int",
                    "O16cps",
                    "IP(nA)",
                    "Yield",
                    "DATETIME",
                    "AnalysisLength",
                    "X",
                    "Y",
                    "DTFAX",
                    "DTFAY",
                    "Mass",
                    "CHC",
                    "MATERIAL",
                    "GROUPNUM",
                    "GUESS.SAMP",
                    "MOUNTNUM",
                    "UNIQUEGRP",
                    "REL_YIELD",
                    "REL_CHC",
                    "BRACKET2SD",
                    "STDd13C",
                    "STDd13Cvpdb")]

return(Output)

}
