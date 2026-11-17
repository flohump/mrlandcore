#' @title correctLesiv2022
#' @description Cells without data (ocean, no map coverage) get a share of zero.
#' @param x MAgPIE object from readLesiv2022
#' @return corrected MAgPIE object
#' @author Florian Humpenoeder
correctLesiv2022 <- function(x) {
  x[is.na(x)] <- 0
  return(x)
}
