processData = function(AOI){

  bounds = AOI$AOI
  region = AOI$region
  HUC6 = AOI$huc6
  ids = AOI$comids

  #### HAND

  path = paste0("./LivingFlood/", region, "/spatial/HAND/", region, "_hand.tif")

  if(!file.exists(path)){

    hand.rasters = lapply(list.files(paste0("./LivingFlood/Raw"), pattern = paste0(HUC6, "hand"), full.names = T), raster::raster)

    hand.fin = clipMerge(hand.rasters, bounds)

    raster::writeRaster(hand.fin, path, options = c('TFW=YES'))

  } else {

    hand.fin = raster::raster(path)
  }


#### Catchment

  path = paste0("./LivingFlood/", region, "/spatial/Catchments/", region, "_catch.tif")

  if(!file.exists(path)){

   catch.rasters = lapply(list.files(paste0("./LivingFlood/Raw"), pattern = paste0(HUC6, "catchmask"), full.names = T), raster::raster)

    catch.fin = clipMerge(catch.rasters, bounds)

    raster::writeRaster(catch.fin, path, options = c('TFW=YES'))

  } else {

    catch.fin = raster::raster(path)
  }


  ### Rating Curves

  path = paste0("./LivingFlood/", region, "/spatial/RatingCurves/", region, "_rating.csv")

  if(!file.exists(path)){

    ratings = lapply(list.files(paste0("./LivingFlood/Raw"), pattern = paste0(HUC6, "rating"), full.names = T), data.table::fread)

    ratings = do.call(rbind, ratings) %>% filter(CatchId %in% ids) %>% select("CatchId", "Stage", "Discharge (m3s-1)")

    write.csv(ratings, path)

  } else {

    ratings = data.table::fread(path)
  }


  return(list(hand = hand.fin, catchments = catch.fin, ratings = ratings))

}

