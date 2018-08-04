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
#' @importFrom data.table fread
#' @importFrom raster raster merge extent intersect crop crs writeRaster
#' @importFrom sf read_sf write_sf as_Spatial st_transform
#' @importFrom curl curl_download
#' @importFrom utils read.csv write.csv globalVariables

NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1"){  utils::globalVariables(c("path", "ratings")) }
