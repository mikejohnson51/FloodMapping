.download.url = function(url, dest, type, i, HUC6){

  `%+%` = crayon::`%+%`

  if(!file.exists(dest)){
    cat(crayon::white(paste0("Downloading ", toupper(type), " data (", i, "/", length(HUC6), "): ")) %+% crayon::yellow(basename(url)), "\n")
    x = httr::GET(url = url, httr::write_disk(dest, overwrite = T), httr::progress(type = "down"))

    if(x$status_code != 200){ stop("Error in download. Please ensure HUC6 is valid, and internet connection is working\n") }

    cat(crayon::white(paste0(toupper(type), " data (", i, "/", length(HUC6), ") saved to: ")) %+% crayon::green(basename(dest), "\n"))
  } else {
    cat(crayon::white(paste0( toupper(type), " data (", i, "/", length(HUC6), ") already exists: ")) %+% crayon::cyan(basename(dest)), "\n")
  }

}


.create.dir = function(base.path, folders = c('hand', 'catchmask', 'rating')){

  if(is.null(folders)){
    if (!dir.exists(base.path)) { dir.create(base.path) }
  } else {

    for(i in 1:length(folders)){
      path = paste0(base.path, "/", folders[i])
      if (!dir.exists(path)) { dir.create(path) }
    }
  }
}

#' @title Downlaod RAW hand data
#' @description Function to isolate HUC6 regions of interest and download the appropriate data from the University of Texas server,
#' @param base.path the path to main Living Flood directory
#' @param HUC6 the HUC6 of interest
#' @param type the type of data desired. Acceptable values are: 'hand', 'catchmask', 'rating'
#' @return the requested file downloaded as a GeoTIFF for RDA file
#' @export
#' @author Mike Johnson
#' @keywords internal
#' @examples
#' \dontrun{
#' downloadRaw(base.path = base.path, HUC6 = 180702, type = 'hand')
#' downloadRaw(base.path = base.path, HUC6 = 180702, type = 'catchmask')
#' downloadRaw(base.path = base.path, HUC6 = 180702, type = 'rating')
#' }


downloadRaw = function(base.path, HUC6, type){

   .create.dir(base.path = base.path)

  if(!(type %in% c("hand", "catchmask", "rating"))){ stop(type, " not found.") }

  for (i in 1:length(HUC6)) {

    base.url = paste0("https://web.corral.tacc.utexas.edu/nfiedata/HAND/", HUC6[i], "/")

    if (type %in% c('hand', 'catchmask')) {

      url = paste0(base.url, HUC6[i], type, ".tif")
      tmp = paste0(base.path, "/", type, "/", type, "_", HUC6[i], ".tif")
      .download.url(url, tmp, type = type, i, HUC6)

    } else if (type == 'rating') {

    url = paste0(base.url, "hydroprop-fulltable-", HUC6[i], ".nohand0.csv")
    tmp = paste0(base.path, "/rating/rating_", HUC6[i], ".rda")

    if (!file.exists(tmp)) {
        cat(crayon::white(paste0("Downloading ", toupper(type), " data (", i, "/", length(HUC6), "): ")) %+% crayon::yellow(basename(url)), "\n")
        ratings <-  data.table::fread(url, select = c("CatchId", "Stage", 'Discharge (m3s-1)'), showProgress = FALSE)
        ratings <- data.frame(ratings)
        save(ratings, file = tmp, compress = 'xz')
        rm(ratings)
        cat(crayon::white(paste0(toupper(type), " data (", i, "/", length(HUC6), ") saved to: ")) %+% crayon::green(basename(tmp), "\n"))
    } else {
        cat(crayon::white(paste0( toupper(type), " data (", i, "/", length(HUC6), ") already exists: ")) %+% crayon::cyan(basename(tmp)), "\n")
    }
  }
 }
}

