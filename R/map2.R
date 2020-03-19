#' @title Build named flow matrix from CONUS netcdf file
#' @param x a file path to a flow NetCDF
#' @param comids a vector of COMIDs
#' @return data.frame
#' @export
#' @importFrom RNetCDF open.nc var.get.nc close.nc
#' @importFrom fastmatch fmatch
#' @importFrom stats setNames na.omit

nc_flow_mat = function(x, comids){

  nc = RNetCDF::open.nc(x)

  feature_id = var.get.nc(nc, "feature_id")

  ind  = fastmatch::fmatch(comids, feature_id)

  time = var.get.nc(nc, "time") %>% as.POSIXct(origin = "1970-01-01")

  extractNC = function(x){
    var.get.nc(nc, "streamflow", start = c(x,1),
               count = c(1,NA), unpack = TRUE)
  }

  streamflow = lapply(ind, extractNC)

  RNetCDF::close.nc(nc)

  cbind(comids, do.call(rbind,streamflow)) %>%
    data.frame() %>%
    setNames(c('comid', paste0('t_', time))) %>%
    na.omit()

}

#' @title Map flood extents from processed data
#' @description This function takes a folder of processed data and generates a HAND inudation raster for each timestep
#' @param hand.path a path to HAND tif
#' @param catchment.path a path to catchmask tif
#' @param rating.path a path to rating fst file
#' @param flows.path a path to a flows nc file
#' @param add numeric. A uniform depth to add to simulated inudation
#' @return a raster stack of innudation maps
#' @export
#' @importFrom foreach %dopar% foreach
#' @importFrom fst read.fst
#' @importFrom raster getValues
#' @importFrom dplyr filter rename
#' @importFrom stats na.omit setNames
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel detectCores

map_flood = function(hand.path, catchment.path, rating.path, flows.path, add = 0){

  #j = NULL
  #`%dopar%` = foreach::`%dopar%`

  COMID <- comid <- NULL

  rating_curves = fst::read.fst(rating.path)

  catch = raster::raster(catchment.path)
  h     = raster::raster(hand.path)

  catch.v =  getValues(catch)
  hand.v  =  getValues(h)

  comids = unique(catch.v)

  if(raster::extension(flows.path) == '.nc'){
    flows = nc_flow_mat(flows.path, comids)
  } else {
    flows = fst::read.fst(flows.path) %>%
      dplyr::filter(COMID %in% comids) %>%
      dplyr::rename(comid = COMID)
  }

  if(length(unique(rating_curves$COMID)) > length(unique(flows$comid))){
    index = rating_curves$COMID %in% flows$comid
    rating_curves = rating_curves[index,]

  } else if(length(unique(rating_curves$COMID)) < length(unique(flows$comid))) {
    index = flows$comid %in% rating_curves$COMID
    flows = flows[index,]
  }

  stage = list()

  for(i in 1:length(comids)){
    flow = flows %>%
      dplyr::filter(comid == comids[i])

    curve = rating_curves[rating_curves$COMID == comids[i],]
    fin = NULL

    for(j in 2:ncol(flow)){
      tmp = curve$Y[which.min(abs(curve$Q - flow[1,j]))]
      if(length(tmp) < 0){ tmp = NA }

      fin = append(tmp + add, fin)
    }
      stage[[i]] =  c(curve$COMID[1], fin)
  }

  stage =  do.call(rbind,stage) %>%
    data.frame() %>%
    na.omit()  %>%
    setNames(c('COMID', paste0("timestep", 1:(ncol(flow)-1))))


  doParallel::registerDoParallel( parallel::detectCores() - 1 )

  a <- foreach::foreach(j = 2:ncol(stage), .combine = raster::stack) %dopar% {
    val.v <- stage[fastmatch::fmatch(catch.v, stage$COMID), j]
    fin.v <- val.v - hand.v
    fin.v[fin.v < 0] <- NA
    f.v <- matrix(fin.v, ncol = ncol(catch), byrow = T)
    f <- catch
    f[] <- f.v
    return(f)
  }

  names(a) = names(flows)[2:ncol(flows)]

  return(a)
}



