CatchExcelBrackets <- function(Input){
  Output <- Input
  for(i in 1:nrow(Output[grepl("bracket", Output$Comment, ignore.case = TRUE),])){
    
    Output$GUESS.SAMP[grepl("bracket", Output$Comment, ignore.case = TRUE)][i] <- Output$GUESS.SAMP[Output$INDEX==(Output$INDEX[grepl("bracket", Output$Comment, ignore.case = TRUE)][i])-2]
    
  }
  
  for(i in 1:nrow(Output[grepl("bracket", Output$Comment, ignore.case = TRUE)&grepl("average", Output$Comment, ignore.case = TRUE),])){
    
    Output$GUESS.SAMP[grepl("bracket", Output$Comment, ignore.case = TRUE)&grepl("average", Output$Comment, ignore.case = TRUE)][i] <- Output$GUESS.SAMP[Output$INDEX==(Output$INDEX[grepl("bracket", Output$Comment, ignore.case = TRUE)&grepl("average", Output$Comment, ignore.case = TRUE)][i])-1]
    
  }
  
  return(Output)
}