#' Identify running and calibration standards
#' @name StandardID
#' Uses a lookup table with regex to look through \code{Comment} column of a
#' WiscSIMS excel sheet.
#' @param InputFile 
#' @param IsotopeMethod 
#'
#' @return
#' @export
#'
#' @examples
StandardID <- function(InputFile, IsotopeMethod) {
  # Identifies running standards based on GREP library

  # Args:
  #       InputFile: Datafame after column rename
  #       IsotopeMethod: Included to limit standards

  # Returns:
  #       Output: Dataframe with standard column including simplified standard names

  Output <- InputFile
  Output$RegexSTD <- Output$Comment
  Output$is_standard <- Output$Comment

  url1 <-
    "https://github.com/EarthCubeGeochron/Sparrow-WiscSIMS/blob/master/Test-Data/WiscSIMSrunSTDS.xlsx?raw=true"
  GET(url1, write_disk(tf <- tempfile(fileext = ".xlsx")))

  Standards <- read_excel(tf)

  for (i in 1:nrow(Standards)) {
    Output$RegexSTD[grepl(Standards$REGEX[i], Output$Comment, ignore.case = TRUE)] <-
      Standards$StdName[i]

    Output$is_standard[grepl(Standards$REGEX[i], Output$Comment, ignore.case = TRUE)] <-
      TRUE
  }

  Output$is_standard[is.na(Output$File)] <- NA

  Output$is_standard[!is.na(Output$SD2ext) &
    !is.na(Output$DTFAX)] <- NA

  Output$RegexSTD[is.na(Output$is_standard)] <- NA

  Output$is_standard[!is.na(Output$SD2ext) &
    !is.na(Output$DTFAX)] <- FALSE

  return(Output)
}
