#' @title Crop and merge and write raster sets
#' @description Crop and merge datasets that cross HUC6 boundaries
#' @param input a raster stack
#' @param bb a bounding box to crop to
#' @param write.path a path to write processed data
#' @keywords internal
#' @return NULL
#' @export
#' @author Mike Johnson

mosaic.lf = function(input, bb){

  s = list()

  if(grepl('Spatial', class(bb))){ bb = sf::st_as_sf(bb) }

  for(i in seq_along(input)){
    dat <- raster::raster(input[i])

    bb = sf::st_transform(bb, as.character(dat@crs))

    if(!is.null(raster::intersect(raster::extent(dat),raster::extent(bb)))){
      s[[paste0("raster", i)]] <- raster::crop(dat, bb, snap = "out")
    }
  }

  if(length(s) > 1){
    names(s) <- NULL
    message("Mosaicing raster...")
    utils::flush.console()
    s$fun <- max
    s$na.rm <- TRUE
    s$tolerance = 30
    mos = do.call(raster::merge, s)
    gc()
  } else {
    mos = s[[1]]
  }

  return(mos)
}
