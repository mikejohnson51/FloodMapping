#' LivingFlood
#'
#' \code{LivingFlood} package
#'
#' Generate near real-time and forecasted flood extents using the National Water Model
#'
#' See the README on github
#'
#' @docType package
#'
#' @name LivingFlood
#'
#' @import AOI
#' @import nwm
#' @importFrom crayon white red yellow cyan %+%
#' @importFrom data.table fread
#' @importFrom doParallel registerDoParallel
#' @importFrom fastmatch fmatch
#' @importFrom foreach %dopar% foreach
#' @importFrom httr GET progress write_disk
#' @importFrom parallel detectCores
#' @importFrom raster raster merge extent intersect crop crs writeRaster
#' @importFrom sf read_sf write_sf as_Spatial st_transform st_as_sf
#' @importFrom stats reshape
#' @importFrom utils read.csv write.csv globalVariables
#' @importFrom velox velox

NULL
