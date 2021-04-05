#' Return minimum string repeated in character vector
#'
#' @param Input A character vector.
#'
#' @return A character vector that is the shortest repeated sequence of characters for the \code{Input} vector.
#' @export
#'
#' @examples
MaxStringRepeated <- function(Input) {
  Input <- Input[!is.na(Input)]
  Shortest <- Input[max(which(nchar(Input) == min(nchar(Input))))]
  ShortestChar <- nchar(Shortest)
  i <- 2
  Test <- TRUE
  while (Test) {
    Name <- substr(Shortest, start = 1, stop = i)
    Test <- all(grepl(
        pattern = Name,
        x = Input,
        ignore.case = TRUE
      ))
    i <- i + 1
    ifelse(i<ShortestChar&Test==TRUE, Test <- TRUE, Test <- FALSE)
  }
 
  Output <- substr(Shortest, start = 1, stop = (i-1))
  
  return(Output)
}