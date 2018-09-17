#' @title Map flood extents from processed data
#' @description This function takes a folder of processed data and generates a HAND inudation raster for each timestep
#' @param name.dir the directory where processed data is stored
#' @param write logical. Should output rasters be written to disk (default = TRUE)
#' @return a raster stack of innudation maps
#' @export
#' @author Mike Johnson

map = function(name.dir, write = TRUE, add = 0){

  j = NULL
  `%dopar%` = foreach::`%dopar%`

  all.files = list.files(name.dir, recursive = T, full.names = TRUE)

  load(all.files[grepl("flows.rda", all.files)])
  load(all.files[grepl("rating.rda", all.files)])

  catchmentv = velox::velox(all.files[grepl("catchmask.tif", all.files)])
  handv = velox::velox(all.files[grepl("hand.tif", all.files)])

  if(length(unique(rating_curves$CatchId)) > length(unique(flows$COMIDS))){
    index = rating_curves$CatchId %in% flows$COMIDS
    rating_curves = rating_curves[index,]

  } else if(length(unique(rating_curves$CatchId)) < length(unique(flows$COMIDS))) {
    index = flows$COMIDS %in% rating_curves$CatchId
    flows = flows[index,]
  }

  comids = unique(flows$COMIDS)
  stage = NULL

  for(i in seq_along(comids)){
    flow = flows[flows$COMIDS == comids[i],] + add
    curve = rating_curves[rating_curves$CatchId == comids[i],]
    fin = NULL

    for(j in 2:dim(flow)[2]){
      tmp = curve$Stage[which.min(abs(curve$`Discharge..m3s.1.` - flow[1,j]))]
      if(length(tmp) <= 0){ tmp = NA }
      fin = append(tmp, fin)
    }
    stage = rbind(stage, fin)
  }

  stage = cbind(comids, stage)
  rownames(stage) = NULL
  colnames(stage) = c('COMID', paste0("timestep", 1:(dim(stage)[2]-1)))
  stage = as.data.frame(stage, stringsAsFactors = FALSE)

  catch.v = as.vector(t(catchmentv$rasterbands[[1]]))
  hand.v =  as.vector(t(handv$rasterbands[[1]]))

  doParallel::registerDoParallel( parallel::detectCores() - 1 )

  a <- foreach::foreach(j = 2:NCOL(stage), .combine = raster::stack) %dopar% {
    val.v = stage[fastmatch::fmatch(catch.v, stage$COMID), j]
    fin.v = val.v - hand.v
    fin.v[fin.v <= 0] <- NA
    #fin.v[fin.v > 0] <- 1
    f.v = matrix(fin.v, ncol = catchmentv$dim[2], byrow = T)
    f = raster::raster(f.v)
    raster::extent(f) <- catchmentv$extent
    raster::crs(f) = catchmentv$crs
    raster::res(f) <- catchmentv$res
    return(f)
  }

  names(a) = paste0("timestep", c(1:dim(a)[3]))

  return(a)
}

