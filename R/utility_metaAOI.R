#' @title Get AOI meta data
#' @description Break a spatial geometry into descriptive meta data
#' @param AOI a spatial, raster or sf object
#' @return a list of features describing the AOI
#' @keywords internal
#' @examples
#' \dontrun{
#' AOI = getAOI('Colorado Springs') %>% metaAOI
#' }
#' @author Mike Johnson

metaAOI = function(AOI){

  bb = bbox_st(AOI)

  latCent = sum(bb$ymin, bb$ymax) / 2

  return(
   list(
      latCent = latCent,
      lngCent = sum(bb$xmin, bb$xmax) / 2,
      height  = round(69 * (abs(bb$ymin - bb$ymax)), 0),
      width   = round(69 * cos(latCent * pi/180)*(abs(bb$xmin - bb$xmax)), 0),
      origin  = "center"
     )
   )

}
