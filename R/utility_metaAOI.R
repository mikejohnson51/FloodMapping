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

  latCent = mean(AOI@bbox[2,])

  return(
   list(
      latCent = latCent,
      lngCent = mean(AOI@bbox[1,]),
      height  = round(69 * (abs(AOI@bbox[2,1] - AOI@bbox[2,2])), 0),
      width   = round(69 * cos(latCent * pi/180)*(abs(AOI@bbox[1,1] - AOI@bbox[1,2])), 0),
      origin  = "center"
     )
   )

}
