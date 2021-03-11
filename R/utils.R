#' @title Identify HUC6 Units within AOI
#' @param AOI an AOI
#' @param level HUC level to extract (default = 6)
#' @return a sf object
#' @export
#' @importFrom sf st_transform st_bbox read_sf st_crs

findHUC6 = function(AOI, level = 6){

  ws.crs = '+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs'

  bb = st_transform(AOI, ws.crs) %>% sf::st_bbox()
  bb.ordered =  paste(bb[1],bb[2],bb[3],bb[4], sep = "%2C")

  endpoint = level / 2

  url = paste0('https://hydro.nationalmap.gov/arcgis/rest/services/wbd/MapServer/',
                 endpoint,
                 '/query?',
                 '&geometry=',
                 bb.ordered,
                 '&geometryType=esriGeometryEnvelope',
                 '&outFields=*',
                 '&returnGeometry=true',
                 '&returnZ=false',
                 '&returnM=false',
                 '&returnExtentOnly=false',
                 '&f=geoJSON')

    tryCatch({ sf::read_sf(url) %>% st_transform(sf::st_crs(AOI)) },
                   warning = function(w) { NULL },
                   error = function(e) { NULL })
}


#' @title Crop and Reproject tiff using GDAL
#' @description Wrapper around gdalwarp. Projects rasters to 3857
#' @param input a path to the input file
#' @param output the path to the output file
#' @param name the name of the region used in LivingFlood
#' @param aoi.path a path to an AOI object on disk
#' @param method resampling method (e.g. 'near')
#' @return on disk rasters files (tiffs)
#' @export
#' @importFrom sf gdal_utils

crop_project = function(input, output, name, aoi.path, method){

  sf::gdal_utils(util = "warp",
                 source = input,
                 destination = output,
                 options = c('-t_srs', "EPSG:3857",
                             '-cutline', aoi.path,
                             '-crop_to_cutline', TRUE,
                             "-r", method))
#
#   gdalUtilities::gdalwarp(input, output,
#                       t_srs = 'EPSG:3857',
#                       #dstnodata = NA,
#                       cutline  = aoi.path,
#                       crop_to_cutline = TRUE,
#                       r = method,
#                       overwrite = TRUE)

}

#' @title Align Rasters of possibly differnt diminsions/stats
#' @description Align catchmask and HAND rasters to catchmask grid
#' @param huc6 A HUC6 unit
#' @param name.dir the directory where data is, and, will be written
#' @return on disk raster files (tiffs)
#' @export
#' @importFrom raster crs raster ncol nrow
#' @importFrom sf st_bbox
#' @importFrom sf gdal_utils


align_rasters = function(huc6, name.dir){

  all   = list.files(name.dir, full.names = TRUE, pattern = huc6)
  all   = grep("tif$", all, value = T)

  #catch = grep("catch", all, value = T)

  hand.path  =  grep("hand", all, value = T)

  tmp = paste0(dirname(hand.path), "//tmp_", basename(hand.path))

  hand = raster(hand.path)
  proj4_string <- as.character(crs(hand))

  te = as.numeric(st_bbox(hand))

  ts = c(ncol(hand), nrow(hand))

  sf::gdal_utils(util = "warp",
                 source = hand.path,
                 destination = tmp,
                          options = c('-t_srs', proj4_string,
                               '-te', te, '-ts', ts))

  #gdalUtilities::gdalwarp(hand, tmp, te = te, t_srs = proj4_string, ts = ts)

  file.remove(hand.path)
  file.rename(tmp, hand.path)
}

#' @title Merge Rasters
#' @description Merge rasters from different HUC6 units
#' @param name.dir a directory send the files
#' @param name a name of a study unit
#' @return a list of local HAND and catchmask filepaths
#' @export
#' @importFrom sf gdal_utils

merge_rasters = function(name.dir, name){

  #catch.path = paste0(name.dir, "/catchmask_", name,".tif")
  hand.path  = paste0(name.dir, "/hand_", name,".tif")

  all = list.files(name.dir, full.names = TRUE)

  #catch = grep("catchmask.tif", all, value = T)

  # sf::gdal_utils(util = "warp",
  #                source = catch,
  #                destination = catch.path,
  #                options = c('-r', "near", "-overwrite"))
  # gdalUtilities::gdalwarp(srcfile = catch,
  #                     dstfile = catch.path,
  #                     overwrite = TRUE, r = 'near')

  #file.remove(catch, recursive = TRUE)

  hand = grep("hand.tif", all, value = T)

  sf::gdal_utils(util = "warp",
                 source = hand,
                 destination = hand.path,
                 options = c('-r', "bilinear", "-overwrite"))

  # gdalUtilities::gdalwarp(hand,
  #                     hand.path,
  #                     overwrite = TRUE,
  #                     r = 'bilinear')

  file.remove(hand, recursive = TRUE)

  return(#list(
    hand.path# = hand.path,
    #catch.path = catch.path
  #)
  )
}
