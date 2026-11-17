#' @title downloadLesiv2022
#' @description Global forest management map 2015 at 100 m (Lesiv et al. 2022), Zenodo record 5879022 (version 3).
#' @return Metadata list
#' @author Florian Humpenoeder
#' @importFrom utils download.file
downloadLesiv2022 <- function() {
  base <- "https://zenodo.org/api/records/5879022/files/"
  for (f in c("FML_v3-2_with-colorbar.tif", "legend.xlsx")) {
    download.file(paste0(base, utils::URLencode(f), "/content"), destfile = f, mode = "wb")
  }
  return(list(url = "https://zenodo.org/records/5879022",
              doi = "10.5281/zenodo.5879022",
              title = "Global forest management data at a 100m resolution for the year 2015",
              author = "Myroslava Lesiv, Dmitry Schepaschenko, Marcel Buchhorn, Linda See et al.",
              version = "3.2",
              release_date = "2021-02-15",
              license = "CC BY 4.0",
              reference = "Lesiv et al. (2022) Sci Data 9, 199, doi:10.1038/s41597-022-01332-3"))
}
