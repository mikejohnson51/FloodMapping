mosaic.lf = function(input, bb, write.path){

  s = list()

  for(i in seq_along(input)){
    dat <- raster::raster(input[i])

    bb = as_Spatial(sf::st_transform(bb, dat@crs))

    if(!is.null(raster::intersect(raster::extent(dat),raster::extent(bb)))){
      s[[paste0("raster", i)]] <- raster::crop(dat, bb, snap = "out")
      message("Raster Cropped.")
    } else {
      message("Raster not needed")
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
  raster::writeRaster(mos, filename = write.path, format="GTiff", overwrite=TRUE, options="COMPRESS=LZW")
}
