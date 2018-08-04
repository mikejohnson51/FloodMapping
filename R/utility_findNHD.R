#' Find National Hydrography Data Stream Networks
#'
#' @description
#' \code{findNHD} returns a list of \code{Spatial*} Objects cropped to an Area of Interest.\cr\cr
#'
#' @param AOI  If TRUE, returns a list of COMIDS for NHD reaches
#' @param ids If TRUE, data is written to a HydroData folder in users working directory.
#'
#' @export
#' @author Mike Johnson
#'

findNHD = function(AOI = NULL, ids = FALSE) {

  if(!(class(AOI) %in% c("list","HydroData"))){AOI = list(AOI = AOI)}

  sl = query_cida(AOI$AOI, type = 'nhdflowline_network', spatial = T)

  if(!is.null(sl)){

  AOI[["nhd"]] = sl

  }

  return(AOI)
}
