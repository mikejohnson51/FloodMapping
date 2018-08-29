.cida = function(AOI, type, spatial = TRUE){

  df = data.frame(server = c(rep("WBD", 2),
                             rep("nhdplus", 3),
                             rep("NWC", 2)),

                  type =   c("huc08",
                             "huc12",

                             "nhdflowline_network",
                             "catchmentsp",
                             "nhdwaterbody",

                             "gagesII",
                             "gagesii_basins"),

                  stringsAsFactors = F)

  bb = AOI@bbox

  if(!(type %in% df$type)){stop("Type not found.")}

  server = df[which(df$type == type),1]

  url_base <- paste0("https://cida.usgs.gov/nwc/geoserver/",
                     server,
                     "/ows",
                     "?service=WFS",
                     "&version=1.0.0",
                     "&request=GetFeature",
                     "&typeName=",
                     server, ":", type,
                     "&outputFormat=application%2Fjson",
                     "&srsName=EPSG:4269")

  url <- paste0(url_base, "&bbox=",
                paste(bb[2,1], bb[1,1],
                      bb[2,2], bb[1,2],
                      "urn:ogc:def:crs:EPSG:4269", sep = ","))

  sl = tryCatch({sf::st_zm(sf::read_sf(url))},
                error = function(e){
                  return(NULL)
                }, warning = function(w){
                  return(NULL)
                }
  )

  if(any(is.null(sl), nrow(sl) ==0)) {
    sl = NULL
    warning("O features found in this AOI.")} else {
      if(spatial) {sl = sf::as_Spatial(sl)}
    }

  return(sl)
}


#' @title Find National Hydrography Stream Networks
#' @description \code{findNHD} returns a list of \code{SimpleFeatures} object cropped to an Area of Interest.
#' @param AOI  a spatial geometry to clip to
#' @return a list with AOI geometry and NHD feature set
#' @keywords internal
#' @export
#' @author Mike Johnson
#'

findNHD = function(AOI = NULL) {

  if(!(class(AOI) %in% c("list","HydroData"))){AOI = list(AOI = AOI)}

  sl = .cida(AOI$AOI, type = 'nhdflowline_network', spatial = T)

  if(!is.null(sl)){

  AOI[["nhd"]] = sl

  }

  return(AOI)
}
