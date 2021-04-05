TextClusterGroup <- function(Input){

#### Guess at different samples ####
#### Based on Levenshtein Distance of comment string
#### Number of clusters IDed manually or set

str <- Output$Comment[!is.na(Output$File)&Output$MATERIAL=='Sample']
print(length(str))
str[is.na(str)] <- ""
str <- str[!is.na(str)]
print(length(str))

File <- Output$File[!is.na(Output$File)&Output$MATERIAL=='Sample']
print(length(File))
File[is.na(File)] <- ""
File <- File[!is.na(File)]
print(length(File))
#print(File)
#File <- File[grep('.asc$', File)]

Comment <- str

str <- sub(" Cs.*", "", str)

d  <- adist(str)

rownames(d) <- Comment

hc <- hclust(as.dist(d))

if(is.numeric(PlugNum)){
  
  df <- data.frame(Comment, File, GUESS.SAMP = as.factor(cutree(hc,k=PlugNum)))
  
  levels(df$GUESS.SAMP) <- df$Comment[match(levels(df$GUESS.SAMP), df$GUESS.SAMP)]
  
}else{
  
  
  #GuessPlugs <- length(c(which(diff(Output$GROUPNUM[Output$MATERIAL=="Sample"&is.na(Output$MATERIAL)==FALSE]%%2)!=0)))
  Duration <- quantile(Output$AnalysisLength, probs = c(.99), na.rm = TRUE)
  GuessPlugs <- length(Output$AnalysisLength[Output$AnalysisLength>Duration&!is.na(Output$AnalysisLength)])
  Output$MountChange <- NA
  print(length(unique(Output$GUESS.SAMP2)))
  print(length(which(Output$AnalysisLength>Duration)))
  Output$MountChange[which(Output$AnalysisLength>Duration)] <- "Change"
  
  if(GuessPlugs>2){
    
    df <- data.frame(Comment, File, GUESS.SAMP = as.factor(cutree(hc,k=GuessPlugs)))
    
    levels(df$GUESS.SAMP) <- df$Comment[match(levels(df$GUESS.SAMP), df$GUESS.SAMP)]
    
  }else{
    
    #Replace Comment[1] with something more complex...
    
    df <- data.frame(Comment, File, GUESS.SAMP = as.factor(rep(Comment[1], times = length(Comment))))
    
    levels(df$GUESS.SAMP) <- df$Comment[match(levels(df$GUESS.SAMP), df$GUESS.SAMP)]
    
  }
  
}

#### Bind GUESS.SAMP to Output ####

Output <- merge(Output, df, by.x="File", by.y = "File", all.x = TRUE, sort = FALSE, suffixes = c("",".y"))
Output$GUESS.SAMP <- as.character(Output$GUESS.SAMP)
Output$GUESS.SAMP[is.na(Output$GUESS.SAMP)==TRUE] <- Output$MATERIAL[is.na(Output$GUESS.SAMP)==TRUE]
Output$GUESS.SAMP <- as.factor(Output$GUESS.SAMP)

Output <- Output[,!(names(Output) == "Comment.y")]

return(Output)

}