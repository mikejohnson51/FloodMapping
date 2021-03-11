#' @title Map flood extents from processed data
#' @description This function takes a folder of processed data and generates a HAND inundation raster for each timestep
#' @param hand.path a path to HAND tif
#' @param catchment.path a path to catchmask tif
#' @param flows.path a path to a flows nc file
#' @param add numeric. A uniform depth to add to simulated inudation
#' @param threshold a value to exclude values less then
#' @return a raster stack of innudation maps
#' @export
#' @importFrom raster raster getValues stack addLayer
#' @importFrom dplyr filter rename left_join mutate
#' @importFrom RNetCDF open.nc var.get.nc
#' @importFrom nwmHistoric extract_nwm
#' @importFrom rlang .data

map_flood = function(hand.path, catchment.path,
                     flows.path, add = 0,
                     threshold  = 0){

  catch = raster::raster(catchment.path)
  h     = raster::raster(hand.path)

  catch.v =  raster::getValues(catch)
  hand.v  =  raster::getValues(h)

  comids = unique(catch.v)

  ahg_connection = open.nc(system.file("extdata", "conus_srcs.nc",
                                  package = "FloodMapping"))

  ahg =  data.frame(comid = var.get.nc(ahg_connection, "feature_id"),
                    c     = var.get.nc(ahg_connection, "c", unpack = TRUE),
                    f     = var.get.nc(ahg_connection, "f", unpack = TRUE))


  flows = nwmHistoric::extract_nwm(flows.path, comids = comids)  %>%
    dplyr::left_join(ahg, by = "comid") %>%
    dplyr::mutate(depths = c * .data$values ^ f)

  ts = unique(flows$dateTime)
  a = raster::stack()
  for(j in 1:length(ts)){
    tmp   <- filter(flows, .data$dateTime == ts[j])
    val.v <- (tmp$depths[match(catch.v, tmp$comid)]) - threshold
    fin.v <- val.v - hand.v
    fin.v[fin.v < 0] <- NA
    f.v <- matrix(fin.v, ncol = ncol(catch), byrow = T)
    f <- catch
    f[] <- f.v
    a = raster::addLayer(a, f)
  }

  names(a) = paste0('ts_', 1:length(ts))

  return(a)
}



