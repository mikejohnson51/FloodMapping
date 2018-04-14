#' Find National Hydrography Data Stream Networks
#'
#' @description
#' \code{findNHD} returns a list of \code{Spatial*} Objects cropped to an Area of Interest.\cr\cr
#' To better understand defining an AOI using '\emph{state}', '\emph{county}' and '\emph{clip_unit}' see \code{getAOI} and \code{getClipUnit}.\cr\cr
#' Returned \code{list} can be interactivly explored via \code{\link{explore}} and COMID values (\code{ids = TRUE}) allow for National Water Model access via \code{getNWM}.\cr\cr
#' All outputs are projected to \code{CRS'+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0+no_defs'} and stream networks are (\emph{down})loaded from the \href{https://cida.usgs.gov}{USGS}.
#'
#' @param state     Full name(s) or two character abbriviation(s). Not case senstive
#' @param county    County name(s). Requires \code{state} input.
#' @param clip_unit SpatialObject* or list. For details see \code{getClipUnit}
#' @param boundary  If TRUE, the AOI \code{SpatialPolygon(s)} will be joined to returned list
#' @param basemap   If TRUE, a basemap will be joined to returned list
#'
#'  If a user wants greater control over basemap apperance replace TRUE with either:
#' \itemize{
#' \item't':  google terrain basemap
#' \item's':  google sattilite imagery basemap
#' \item'h':  google hybrid basemap
#' \item'r':  google roads basemap
#' }
#'
#' @param ids  If TRUE, returns a list of COMIDS for NHD reaches
#' @param save If TRUE, data is written to a HydroData folder in users working directory.
#'
#' @seealso  \code{\link{getAOI}}
#' @seealso  \code{\link{getNWM}}
#' @seealso  \code{\link{explore}}
#'
#' @family HydroData 'find' functions
#'
#' @return
#' \code{findNHD} returns a list of minimum length 1:
#'
#' \enumerate{
#' \item 'flowlines': A \code{SpatialLinesDataFrame}\cr
#'
#'
#' Pending parameterization, \code{findNHD} can also return:
#'
#' \item 'basemap':   A \code{RasterLayer*} basemap if \code{basemap = TRUE}
#' \item 'boundry':   A \code{SpatialPolygon*} of AOI if \code{boundary = TRUE}
#' \item 'fiat':      A \code{SpatialPolygon*} of intersected county boundaries if \code{boundary = TRUE}
#' \item 'ids':       A vector of COMIDs if \code{ids = TRUE}
#' }
#'
#'
#' @examples
#' \dontrun{
#' # Find NHD data for El Paso County, Colorado
#' }
#'
#' @export
#' @author Mike Johnson


defineHydro = function(state = NULL,
                       county = NULL,
                       clip_unit = NULL) {

  region = describe(state = state,
                    county = county,
                    clip_unit = clip_unit)

  build_files(region = region)

  if (!file.exists(paste0("./LivingFlood/", region, "/spatial/AOI/AOI.shp"))) {
    AOI = getAOI(state = state,
                 county = county,
                 clip_unit = clip_unit)
    message(
      "AOI defined as the ",
      nameAOI(
        state = state,
        county = county,
        clip_unit = clip_unit
      ),
      ". Shapefile determined. Now loading NHD flowline data...\n"
    )

    rgdal::writeOGR(
      obj = as(AOI, "SpatialPolygonsDataFrame"),
      dsn = paste0("./LivingFlood/", region, "/spatial/AOI"),
      layer = "AOI",
      driver = "ESRI Shapefile"
    )

  } else {
    message("AOI boundary exists.\nLoading shapefile ... ")
    AOI = rgdal::readOGR(paste0("./LivingFlood/", region, "/spatial/AOI/AOI.shp"),
                         verbose = FALSE) %>% spTransform(HydroDataProj)
  }


  if (!file.exists(paste0("./LivingFlood/", region, "/spatial/NHD/nhd_flowlines.shp"))) {
    bb = AOI@bbox

    URL = paste0(
      "https://cida.usgs.gov/nwc/geoserver/nhdplus/ows?service=WFS&version=2.0.0&request=GetFeature&typeNames=nhdplus:nhdflowline_network&srsName=EPSG:4326&bbox=",
      min(bb[2, ]),
      ",",
      min(bb[1, ]),
      ",",
      max(bb[2, ]),
      ",",
      max(bb[1, ]),
      "&outputFormat=SHAPE-ZIP"
    )

    sl = download.shp(URL = URL, type = 'NHD flowlines') %>% spTransform(HydroDataProj)
    sl = sl[AOI, ]

    rgdal::writeOGR(
      obj = sl,
      dsn = paste0("./LivingFlood/", region, "/spatial/NHD"),
      layer = "nhd_flowlines",
      driver = "ESRI Shapefile"
    )

  } else {
    message("NHD data exists for AOI.\nLoading shapefile ... ")
    sl = rgdal::readOGR(paste0(
      "./LivingFlood/",
      region,
      "/spatial/NHD/nhd_flowlines.shp"
    ),
    verbose = FALSE)

  }

  COMID <- sl$comid
  huc6 <- substr(sl$reachcode, 1, 6)

  label <- paste(
    paste("<strong>COMID:</strong>", COMID),
    paste("<strong>HUC 6:</strong>", huc6),
    sep = "<br/>"
  )

  m = leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron, group = "Base") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
    addProviderTiles(providers$Esri.NatGeoWorldMap , group = "Terrain") %>%

    addScaleBar("bottomleft") %>%
    addMiniMap(tiles = providers$OpenStreetMap.BlackAndWhite,
               toggleDisplay = TRUE) %>%
    addMeasure(
      position = "bottomleft",
      primaryLengthUnit = "feet",
      primaryAreaUnit = "sqmiles",
      activeColor = "red",
      completedColor = "green"
    ) %>%

    addPolygons(
      data = AOI,
      fillColor = "transparent",
      group = 'AOI',
      color = 'black',
      stroke = TRUE,
      weight = 3,
      opacity = .9,
      smoothFactor = 0.7
    ) %>%

    addPolylines(
      data = sl,
      color = 'blue',
      weight = sl$streamorde,
      popup = label,
      group = "NHD",

      highlight = highlightOptions(
        weight = 10,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      )
    ) %>%

    addLayersControl(
      baseGroups = c("Base", "Imagery", "Terrain"),
      overlayGroups = c("AOI", "NHD"),
      options = layersControlOptions(collapsed = T)
    )


  print(m)

  return(list(
    spatial = sl,
    AOI = AOI,
    comids = sl$comid,
    huc6 = unique(substr(sl$reachcode, 1, 6)),
    region = region))


}
