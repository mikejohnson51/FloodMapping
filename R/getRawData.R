#' Get HUC6 Flowlines, Catchment Boundaries, and HAND Products
#'
#'This function builds a list of needed file and downloads them from CyberGIS FTP server. Spatial boundaries are the HUC6 units and avialable data include NHDflowlines,
#'catchment boundary, the HAND raster, and the catchment mask.
#'
#' @param
#' HUC6 A six unit identifer for the HUC6 unit. Can be a single value or a string.
#' @param
#' need.shp If TRUE then the shapefiles for the flowlines and HUC6 catchment boundary will be downloaded.
#' @param
#' need.hand.data If TRUE the HAND raster and catchment mask .tif files will be downloaded. This are large files and take a significant amount of time to download,
#' only do so if interested in applying the HAND method for innudation mapping.
#'
#' @examples
#' get_HUC6_data(HUC6 = c("180600",180701), need.shp = TRUE, need.hand.data =TRUE)
#'
#' @author
#' Mike Johnson
#'
#' @return
#' This function downloads the associated shapefiles into the /Geospatial/Flowlines folder built using
#' \code{\link{build_files}}
#'
#' @seealso
#' \code{\link{build_files}}
#' \code{\link{get_rating_curve}}
#'
#' @export
#'


getRawData = function(AOI){

  HUC6 = AOI$huc6

  base.path = paste0("./LivingFlood/Raw/spatial/")

  base.url = "https://web.corral.tacc.utexas.edu/nfiedata/HAND/"

### GET HAND DATA

  for(i in seq_along(HUC6)) {

    file = paste0(HUC6[i], "hand.tif")

    if (!file.exists(paste0(base.path, file))) {
      URL = paste0(base.url, HUC6[i], "/", file)
      download.file(url = URL,
                    destfile = paste0(base.path,  file))
    }
  }

### GET CATCHMENT DATA

  for(i in seq_along(HUC6)) {
    file = paste0(HUC6[i], "catchmask.tif")

    if (!file.exists(paste0(base.path, file))) {
      URL = paste0(base.url, HUC6[i], "/", file)
      download.file(url = URL,
                    destfile = paste0(base.path, file))
    }
  }

### GET RATING CURVE DATA

  for(i in seq_along(HUC6)) {
    file = paste0("hydroprop-fulltable-", HUC6[i], ".csv")

    if (!file.exists( paste0(base.path, HUC6[i], "rating.csv"))) {

    rc = data.table::fread(paste0(base.url, HUC6[i], "/", file))

    rc1 = rc %>% select("CatchId", "Stage", 'Discharge (m3s-1)' )

    write.csv(rc, paste0(base.path, HUC6[i], "rating.csv"))

    }
  }
}

