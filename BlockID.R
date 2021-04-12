#' Identify mount changes in WiscSIMS file
#' @name BlockID
#' @param Input a dataframe that includes \code{MATERIAL and AnalysisLength}
#'  columns with the STD-Sample-STD form.
#'
#' @return Dataframe with \code{number, start, end, }
#' @export
#'
#' @examples
#' \code{df <- data.frame()
#' df$MATERIAL <- c(rep('STD',times = 5),
#' rep('Sample',times = 5), 
#' rep('STD',times = 5),
#' rep('STD',times = 5),
#' rep('Sample',times = 5), 
#' rep('STD',times = 5))
#' 
#' df$AnalysisLength <- rep(3, times = nrow(df))
#' df$AnalysisLength[16] <- 20
#' 
#' Block_ID(df)
#' }
#' @seealso 
#' [AddBracketStructure()]
#' 
Block_ID <- function(Input) {
  output <- Input
  material <- Output$MATERIAL

  # Potential to do in this function...
  # Reduce the number of mounts by rejecting STD-STD-STD as a mount change?

  run_index <- with(
    rle(material),
    data.frame(
      number = values,
      start = cumsum(lengths) - lengths + 1,
      end = cumsum(lengths)
    )[order(values), ]
  )

  run_index$rows <- run_index$end - run_index$start

  run_index <- run_index[is.na(run_index$number) == FALSE, ]
  run_index <- run_index[order(run_index$start), ]
  run_index <- run_index[run_index$rows > 0, ]
  run_index$mountt_change <-
    c(FALSE, run_index$number[-1] == run_index$number[-length(run_index$number)])
  run_index$StdString <-
    c(FALSE, run_index$mountt_change[-1] != run_index$mountt_change[-length(run_index$mountt_change)])
  run_index$ChangeDuration <- Output$AnalysisLength[run_index$start]


  return(run_index)
}