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
#' @importFrom gdalUtilities gdalwarp

crop_project = function(input, output, name, aoi.path, method){

  gdalUtilities::gdalwarp(input, output,
                      t_srs = 'EPSG:3857',
                      #dstnodata = NA,
                      cutline  = aoi.path,
                      crop_to_cutline = TRUE,
                      r = method,
                      overwrite = TRUE)

}

#' @title Align Rasters of possibly differnt diminsions/stats
#' @description Align catchmask and HAND rasters to catchmask grid
#' @param HUC6 A HUC6 unit
#' @param name.dir the directory where data is, and, will be written
#' @return on disk raster files (tiffs)
#' @export
#' @importFrom raster crs raster ncol nrow
#' @importFrom sf st_bbox
#' @importFrom gdalUtilities gdalwarp


align_rasters = function(HUC6, name.dir){
  all   = list.files(name.dir, full.names = TRUE, pattern = HUC6)
  catch = grep("catch", all, value = T)

  hand  =  grep("hand", all, value = T)

  tmp = paste0(dirname(hand), "//tmp_", basename(hand))

  catchmask = raster(catch)
  proj4_string <- as.character(crs(catchmask))

  te = as.numeric(st_bbox(catchmask))

  ts = c(ncol(catchmask), nrow(catchmask))

  synced <- gdalUtilities::gdalwarp(hand, tmp, te = te, t_srs = proj4_string, ts = ts)

}



#' @title Merge Rasters
#' @description Merge rasters from different HUC6 units
#' @param name.dir a directory send the files
#' @param name a name of a study unit
#' @return a list of local HAND and catchmask filepaths
#' @export
#' @importFrom gdalUtilities gdalwarp

merge_rasters = function(name.dir, name){

  catch.path = paste0(name.dir, "/catchmask_", name,".tif")
  hand.path  = paste0(name.dir, "/hand_", name,".tif")

  all = list.files(name.dir, full.names = TRUE)

  catch = grep("catchmask.tif", all, value = T)

  gdalUtilities::gdalwarp(srcfile = catch,
                      dstfile = catch.path,
                      overwrite = TRUE, r = 'near')

  file.remove(catch, recursive = TRUE)

  hand = grep("hand.tif", all, value = T)

  gdalUtilities::gdalwarp(hand,
                      hand.path,
                      overwrite = TRUE,
                      r = 'bilinear')

  file.remove(hand, recursive = TRUE)

  return(list(
    hand.path = hand.path,
    catch.path = catch.path
  ))
}

#' @title Extract Rating Curve Table
#' @description Extract Rating Curve Table
#' @param HUC6 a HUC6 unit
#' @param raw.dir the directory where RAW downloaded HAND data goes
#' @param comids a vector of COMIDs of interest
#' @return a rating curve tabel
#' @importFrom fst read.fst
#' @importFrom dplyr filter
#' @export

get_rc_table = function(HUC6, raw.dir, comids){
  COMID <- NULL
  rc.path = paste0(raw.dir, "/hydroprop-fulltable-", HUC6, ".nohand0.fst")
  rc = fst::read.fst(rc.path)
  rc %>% filter(COMID %in% comids)
}
