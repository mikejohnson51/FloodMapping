#' @title Crop and merge and write raster sets
#' @description Crop and merge datasets that cross HUC6 boundaries
#' @param input a raster stack
#' @param bb a bounding box to crop to
#' @param write.path a path to write processed data
#' @keywords internal
#' @return NULL
#' @export
#' @author Mike Johnson

mosaic.lf = function(input = NULL, AOI = NULL){

  s = list()
  bb = getBoundingBox(AOI)

  for(i in seq_along(input)){
   tmp <- raster::raster(input[i])
   bb = sf::st_transform(bb, as.character(tmp@crs))

    if(!is.null(raster::intersect(raster::extent(tmp),raster::extent(bb)))){
      s[[i]] <- raster::crop(tmp, bb, snap = "out")
      }
  }

  origins<-t(data.frame(lapply(s,raster::origin)))

  min_origin<-c(min(origins[,1]),min(origins[,2]))

  change_origins <- function(x,y){
    raster::origin(x)<-y
    x
  }

  s <- lapply(s, function(x,y) change_origins(x,min_origin))

  if(length(s) == 1){
    mos = s[[1]]
  } else {
    mos = do.call(raster::merge, s)
  }
}

