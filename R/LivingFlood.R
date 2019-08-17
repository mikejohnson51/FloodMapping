#' LivingFlood
#'
#' \code{LivingFlood} package
#'
#' Generate near real-time and forecasted flood extents using the National Water Model
#'
#' See the README on [github](https://github.com/mikejohnson51/FloodMapping)
#'
#' @docType package
#'
#' @name LivingFlood
#'
#' @import AOI
#' @import nwm
#' @importFrom crayon white red yellow cyan %+%
#' @importFrom data.table fread
#' @importFrom jsonlite fromJSON
#' @importFrom doParallel registerDoParallel
#' @importFrom fastmatch fmatch
#' @importFrom foreach %dopar% foreach
#' @importFrom httr GET POST progress write_disk RETRY
#' @importFrom parallel detectCores
#' @importFrom raster raster merge extent intersect crop crs writeRaster projectRaster
#' @importFrom sf read_sf write_sf as_Spatial st_transform st_as_sf
#' @importFrom stats reshape
#' @importFrom utils read.csv write.csv globalVariables unzip
#' @importFrom velox velox

NULL
