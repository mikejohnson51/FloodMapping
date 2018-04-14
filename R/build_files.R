#' Setting Up A File Structure
#'
#' This function helps define your folder strucutre to execute the LivingFlood:
#'
#' @examples
#' build_files()
#' @author
#' Mike Johnson
#'
#' @return
#' This function will create three subfolders in your working directory set with 'setwd()'.
#' These forlders are: \cr \cr
#' (1) Spatial \cr \cr
#' (2) Forecasts \cr \cr
#' (3) Output
#'
#'
#' @export
#'

build_files = function(region = NULL) {

  raw.dir = normalizePath(paste0("./LivingFlood/"), mustWork = FALSE)

  needBuild(raw.dir)
  needBuild(paste0(raw.dir, "/", region))

  needBuild(paste0(raw.dir, "/Raw"))
  needBuild(paste0(raw.dir, "/Raw/spatial"))
  needBuild(paste0(raw.dir, "/Raw/forecasts"))

  needBuild(paste0(raw.dir, "/", region, "/spatial"))
  needBuild(paste0(raw.dir, "/", region, "/spatial/HAND"))
  needBuild(paste0(raw.dir, "/", region, "/spatial/Catchments"))
  needBuild(paste0(raw.dir, "/", region, "/spatial/NHD"))
  needBuild(paste0(raw.dir, "/", region, "/spatial/TIGER"))
  needBuild(paste0(raw.dir, "/", region, "/spatial/RatingCurves"))
  needBuild(paste0(raw.dir, "/", region, "/spatial/AOI"))

  needBuild(paste0(raw.dir, "/", region, "/Forecasts"))

  needBuild(paste0(raw.dir, "/", region, "/Output"))

}




needBuild = function(path = NULL) {
  if (!file.exists(path)) {
    dir.create(path)
  }
}



describe = function(state = NULL,
                    county = NULL,
                    clip_unit = NULL) {
  if (!is.null(county) & length(county) < 3) {
    AOI   = paste0(paste0(gsub(" ", "", county), collapse = "_"),
                   if (!is.null(county)) {
                     "_"
                   },
                   paste0(state, collapse = "_"))
  } else if (!is.null(state) & length(state) < 3) {
    AOI = paste0(state, collapse = "_")
  } else if (!is.null(clip_unit)) {
    if (class(clip_unit[[1]]) == 'character') {
      AOI = clip_unit[[1]]

    }

  if (class(clip_unit[[1]]) == 'numeric') {
    AOI = suppressMessages(ggmap::revgeocode(
      location = c(clip_unit[[2]], clip_unit[[1]]),
      output = 'more'
    ))
    AOI = paste0(
      gsub(" ", "", AOI$administrative_area_level_2),
      "_",
      gsub(" ", "", AOI$administrative_area_level_1),
      collapse = '_'
    )
  }
}
  return(AOI)
}



