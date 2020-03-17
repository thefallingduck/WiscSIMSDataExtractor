StandardID <- function(InputFile, IsotopeMethod){
  
  #Identifies samples, standards, and blocks of analyses from SIMS data table to help recalculate bracket level parameters.
  
  #Args:
  #Returns:
  
  
  MountStart <- InputFile$INDEX[is.na(InputFile$Comment)==FALSE&is.na(InputFile$DTFAX)] #DTFAX is column in all datasets and is therefore useful here.
  
  MOUNTNUM <- vector(length = nrow(InputFile))
  
  for(i in 2:length(MountStart)){
    
    MOUNTNUM[MountStart[i-1]:MountStart[i]] <- i
    
  }
  
  InputFile <- cbind(InputFile, MOUNTNUM)
  
  InputFile$MOUNTNUM <- as.factor(InputFile$MOUNTNUM)
  
  levels(InputFile$MOUNTNUM) <- InputFile$Comment[MountStart]
  
  #### Guess at different samples ####
  #### Based on Levenshtein Distance of comment string
  #### Number of clusters IDed manually or set
  
  str <- InputFile$Comment[is.na(InputFile$File)==FALSE&InputFile$MATERIAL=='Sample']
  
  Comment <- str
  
  str <- sub(" Cs.*", "", str)
  
  d  <- adist(str)
  
  rownames(d) <- Comment
  
  hc <- hclust(as.dist(d))
  
  if(is.numeric(PlugNum)){
    
    df <- data.frame(Comment, GUESS.SAMP = as.factor(cutree(hc,k=PlugNum)))
    
    levels(df$GUESS.SAMP) <- df$Comment[match(levels(df$GUESS.SAMP), df$GUESS.SAMP)]
    
  }else{
    
    GuessPlugs <- length(c(which(diff(InputFile$GROUPNUM[InputFile$MATERIAL=="Sample"&is.na(InputFile$MATERIAL)==FALSE]%%2)!=0)))
    
    df <- data.frame(Comment, GUESS.SAMP = as.factor(cutree(hc,k=GuessPlugs)))
    
    levels(df$GUESS.SAMP) <- df$Comment[match(levels(df$GUESS.SAMP), df$GUESS.SAMP)]
    
    
  }
  
  #### Bind GUESS.SAMP to InputFile ####
  
  InputFile <- merge(InputFile, df, by.x="Comment", by.y = "Comment", all.x = TRUE, sort = FALSE)
  
  InputFile$GUESS.SAMP[is.na(InputFile$GUESS.SAMP)==TRUE] <- InputFile$MATERIAL[is.na(InputFile$GUESS.SAMP)==TRUE]
}