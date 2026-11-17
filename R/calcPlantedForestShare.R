#' @title calcPlantedForestShare
#' @description Share of the cell land under planted forest (Lesiv et al. 2022 classes 31 and 32) on the 67420 cells.
#' Used by calcLanduseInitialisationBase to place the national planted-forest area within a country.
#' @return MAgPIE object on lpjcell level, share of the cell
#' @author Florian Humpenoeder
#' @examples
#' \dontrun{
#' calcOutput("PlantedForestShare", aggregate = FALSE)
#' }
#' @importFrom magclass dimSums getItems setYears
#' @importFrom mstools toolGetMappingCoord2Country
calcPlantedForestShare <- function() {
  x <- readSource("Lesiv2022", convert = "onlycorrect")
  x <- setYears(dimSums(x[, , c("planted", "plantation")], dim = 3), NULL)
  mapping <- toolGetMappingCoord2Country(pretty = TRUE)
  x <- x[mapping$coords, , ]
  getItems(x, dim = 1, raw = TRUE) <- paste(mapping$coords, mapping$iso, sep = ".")
  getSets(x) <- c("x", "y", "iso", "year", "data")
  getNames(x) <- "planted"
  return(list(x = x, weight = NULL, unit = "share of cell land", isocountries = FALSE,
              description = paste("Planted forest (Lesiv et al. 2022, rotation above and up to 15 years)",
                                  "as share of the 0.5 degree cell")))
}
