BracketRecalc <- function(InputFile, IsotopeMethod){
  
  #Function to recalculate running standard corrected isotope values using the running standard for each bracket.
  #Also calculates the hydride ratio (e.g. 16O1H/16O) and
  #relative yield.
  
  ####Args:
  #     InputFile: Dataframe with uniform column names from ColumnRename() and StandardID()
  #     IsotopeMethod: Character string refering to specific isotope method (e.g. "d18O10")
  
  #Returns:
  #     Output: Dataframe with Standard corrected isotope ratio, hydride, and yield appended
  Output <- InputFile
  
  #### Relative Hydride, Relative Yield, recalc isotope####
  Output$UNIQUEGRP <- as.factor(paste(Output$MATERIAL, Output$GROUPNUM))
  levels(Output$UNIQUEGRP)[levels(Output$UNIQUEGRP)=="NA NA"] <- NA
  Output <- Output[order(Output$GROUPNUM),]
  AllGroups <- unique(as.character(Output$UNIQUEGRP))
  
  Samples <- unique(Output$UNIQUEGRP[Output$MATERIAL=="Sample"& !is.na(Output$MATERIAL)])
  
  #### Add columns to dataframe
  Output$REL_YIELD <- NA
  Output$REL_Hyd <- NA
  Output$BRACKET2SD <- NA
  Output$STDiso <- NA
  
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
  #tryCatch(
   # {
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
    #},
   # warning=function(cond){
   #   message(paste("Bracket Calculation failed"))
   # }
  #)
  return(Output)
}