#' @title readLesiv2022
#' @description Forest management classes of Lesiv et al. 2022 (100 m, 2015) as area shares of the 0.5 degree cell:
#' natural forest without and with signs of management (classes 11, 20), planted forest with a rotation above
#' 15 years (31), plantation with a rotation up to 15 years (32), oil palm (40) and agroforestry (53).
#' @return MAgPIE object on 0.5 degree cells (x.y coordinates), shares of the cell
#' @author Florian Humpenoeder
#' @examples
#' \dontrun{
#' readSource("Lesiv2022", convert = "onlycorrect")
#' }
#' @importFrom magclass as.magpie clean_magpie
#' @importFrom terra rast aggregate resample res as.data.frame
readLesiv2022 <- function() {
  r <- rast("FML_v3-2_with-colorbar.tif")
  classes <- c(natural_unmanaged = 11, natural_managed = 20, planted = 31, plantation = 32, oil_palm = 40,
               agroforestry = 53)
  fact <- round(0.5 / res(r))
  tmpl <- rast(xmin = -180, xmax = 180, ymin = -90, ymax = 90, resolution = 0.5, crs = "EPSG:4326")
  out <- NULL
  for (k in names(classes)) {
    a <- resample(aggregate(r == classes[[k]], fact = fact, fun = "mean", na.rm = TRUE), tmpl, method = "bilinear")
    d <- as.data.frame(a, xy = TRUE, na.rm = FALSE)
    coords <- paste(gsub(".", "p", sprintf("%.2f", d$x), fixed = TRUE),
                    gsub(".", "p", sprintf("%.2f", d$y), fixed = TRUE), sep = ".")
    m <- as.magpie(array(d[[3]], dim = c(nrow(d), 1, 1), dimnames = list(coords, "y2015", k)), spatial = 1)
    out <- if (is.null(out)) m else mbind(out, m)
  }
  getSets(out) <- c("x", "y", "year", "class")
  return(clean_magpie(out))
}
