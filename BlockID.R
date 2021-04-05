BlockID <- function(Input) {
  
    Output <- Input
    Material <- Output$MATERIAL
    
    # Potential to do in this function...
    # Reduce the number of mounts by rejecting STD-STD-STD as a mount change?
    
    RunIndex <- with(
      rle(Material),
      data.frame(
        number = values,
        start = cumsum(lengths) - lengths + 1,
        end = cumsum(lengths)
      )[order(values), ]
    )
    
    RunIndex$rows <- RunIndex$end - RunIndex$start
    
    RunIndex <- RunIndex[is.na(RunIndex$number) == FALSE, ]
    RunIndex <- RunIndex[order(RunIndex$start), ]
    RunIndex <- RunIndex[RunIndex$rows > 0, ]
    RunIndex$MountChange <-
      c(FALSE, RunIndex$number[-1] == RunIndex$number[-length(RunIndex$number)])
    RunIndex$StdString <-
      c(FALSE, RunIndex$MountChange[-1] != RunIndex$MountChange[-length(RunIndex$MountChange)])
    RunIndex$ChangeDuration <- Output$AnalysisLength[RunIndex$start]
    
  
  return(RunIndex)
  
}