#' @title calcForestArea
#'
#' @description Calculates consistent forest area and its subcategories based on FRA 2025 (primary forest of
#'              countries that leave it blank in FRA 2025 from FRA 2020, else FRA 2015) and LanduseInitialisation data.
#'
#' @return List of magpie object with results on country level, weight, unit and description.
#'
#' @author Kristine Karstens, Jan Philipp Dietrich
#' @examples
#' \dontrun{
#' calcOutput("ForestArea")
#' }
#' @export
calcForestArea <- function() {

  # set yearly time steps for historical time frame for forest area calculation
  histyears <- sort(magpiesets::findset("past_til2020"))
  startyear <- 1960
  endyear   <- as.integer(gsub("y", "", tail(histyears, 1)))
  years     <- paste0("y", seq(from = startyear, to = endyear, by = 1))

  # FRA 2025: total, naturally regenerating, primary and planted forest
  fra <- readSource("FRA2025", "forest_area")
  forest <- mbind(setNames(fra[, , "forestArea"], "Forest"),
                  setNames(fra[, , "naturallyRegeneratingForest"], "NatFor"),
                  setNames(fra[, , "primary"], "PrimFor"),
                  setNames(fra[, , "naturallyRegeneratingForest"] - fra[, , "primary"], "NatRegFor"),
                  setNames(fra[, , "plantedForest"], "PlantFor"))

  # Primary forest of countries that leave it blank in all FRA 2025 years (the reader sets these to 0): the share of
  # the country's most recent earlier report, FRA 2020, else FRA 2015; a reported 0 counts as a report
  .reported <- function(r) {
    r <- r[, , "primary"]
    return(getItems(r, 1)[as.vector(dimSums(r, dim = c(2, 3))) > 0])
  }
  .share <- function(prim, nat) {
    s <- collapseNames(prim) / collapseNames(nat)
    s[!is.finite(s)] <- 0
    return(time_interpolate(s, interpolated_year = getYears(forest), integrate_interpolated_years = TRUE,
                            extrapolation_type = "constant")[, getYears(forest), ])
  }
  fra20 <- readSource("FRA2020", "forest_area")
  fra15 <- readSource("FAO_FRA2015", "fac")[, , c("PrimFor", "NatRegFor")]
  natFor <- collapseNames(forest[, , "NatFor"])
  rep25 <- .reported(readSource("FRA2025", "forest_area_reported"))
  rep20 <- setdiff(.reported(readSource("FRA2020", "forest_area_reported")), rep25)
  rep15 <- setdiff(getItems(forest, 1), c(rep25, rep20))
  forest[rep20, , "PrimFor"] <- .share(fra20[rep20, , "primary"], fra20[rep20, , "naturallyRegeneratingForest"]) *
    natFor[rep20, , ]
  forest[rep15, , "PrimFor"] <- .share(fra15[rep15, , "PrimFor"], dimSums(fra15[rep15, , ], dim = 3)) *
    natFor[rep15, , ]
  forest[, , "NatRegFor"] <- setNames(natFor - collapseNames(forest[, , "PrimFor"]), "NatRegFor")

  forest <- time_interpolate(forest, interpolated_year = years,
                             integrate_interpolated_years = TRUE,
                             extrapolation_type = "constant")[, years, ]
  vcat(verbosity = 3, "Forest is interpolated for missing years and held constant for the period before FRA starts")

  ### fixing inconsistencies assuming total forest areas and shares of subcategories are reported correctly

  forestSumSub                  <- dimSums(forest[, , c("NatFor", "PlantFor")], dim = 3)
  forest[, , "PlantFor"]        <- toolNAreplace(forest[, , "PlantFor"] /
                                                   forestSumSub * setNames(forest[, , "Forest"], NULL))$x
  forest[, , "NatFor"]          <- toolNAreplace(forest[, , "NatFor"] /
                                                   forestSumSub * setNames(forest[, , "Forest"], NULL))$x

  forestSumSubSub               <- dimSums(forest[, , c("PrimFor", "NatRegFor")], dim = 3)
  forest[, , "PrimFor"]         <- toolNAreplace(forest[, , "PrimFor"] /
                                                   forestSumSubSub * setNames(forest[, , "NatFor"], NULL))$x
  forest[, , "NatRegFor"]       <- toolNAreplace(forest[, , "NatRegFor"] /
                                                   forestSumSubSub * setNames(forest[, , "NatFor"], NULL))$x

  ###########################
  # fixing missing data on split between PrimFor (primforest), NatRegFor (secdforest)
  # and PlantFor (forestry) with LUH data

  luh <- calcOutput("LUH3", landuseTypes = "LUH3", irrigation = FALSE, yrs = years,
                    cellular = FALSE, aggregate = FALSE)[, , c("primf", "secdf")]
  forest <- forest[, getYears(luh), ]

  secondaryForest <- luh[, , "secdf"] - setNames(forest[, getYears(luh), "PlantFor"], NULL)
  if (any(secondaryForest < 0)) {
    tmp <- secondaryForest
    tmp[tmp > 0] <- 0
    tmp <- dimSums(tmp, dim = 1)
    vcat(verbosity = 2, paste("Mismatch of FAO forestry and Hurtt secondary forest:",
                              paste(paste(getYears(tmp), round(tmp, 0), "Mha, "), collapse = " "), ". cut off."))
    secondaryForest[secondaryForest < 0] <- 0
  }
  forestry <- luh[, , "secdf"] - secondaryForest

  luhForest <- mbind(setNames(forestry, "PlantFor"),
                     setNames(luh[, , c("primf")], "PrimFor"),
                     setNames(secondaryForest, "NatRegFor")) + 10^-10
  # 10^-10 added to allow share estimation even under missing area information
  luhForestShare <- luhForest / dimSums(luhForest, dim = 3)
  luhNatForestShare <- luhForest[, , c("PrimFor", "NatRegFor")] / dimSums(luhForest[, , c("PrimFor", "NatRegFor")],
                                                                          dim = 3)

  miss <- where(round(dimSums(forest[, , c("NatFor", "PlantFor")], dim = 3), 6) !=
                  round(forest[, , "Forest"], 6))$true$regions

  if (length(miss) > 0) {
    forest[miss, , c("PlantFor", "PrimFor", "NatRegFor")] <- luhForestShare[miss, , ] *
      setNames(forest[miss, , "Forest"], NULL)
    forest[miss, , "NatFor"] <- setNames(forest[miss, , "PrimFor"] + forest[miss, , "NatRegFor"], NULL)
  }

  miss <- where(round(dimSums(forest[, , c("PrimFor", "NatRegFor")], dim = 3), 6) !=
                  round(forest[, , "NatFor"], 6))$true$regions
  if (length(miss > 0)) {
    forest[miss, , c("PrimFor", "NatRegFor")] <- luhNatForestShare[miss, , ] * setNames(forest[miss, , "NatFor"], NULL)
  }

  ####################################

  map <- data.frame(fao    = c("Forest", "NatFor",     "PrimFor",    "NatRegFor",  "PlantFor"),
                    magpie = c("forest", "natrforest", "primforest", "secdforest", "forestry"))
  out <- toolAggregate(forest, map, from = "fao", to = "magpie", dim = 3)

  if (max(abs(dimSums(out[, , c("natrforest", "forestry")]) - out[, , "forest"])) > 10^-6 ||
        max(abs(dimSums(out[, , c("secdforest", "primforest")], dim = 3) - out[, , "natrforest"])) > 10^-6) {
    warning("There are inconsistencies within the forest area data set.")
  }

  out <- mrdownscale::toolReplaceExpansion(out, "primforest", "secdforest",
                                           warnThreshold = 35)

  return(list(x = out,
              weight = NULL,
              unit = "Mha",
              description = "Forest area and its subcategories"))
}
