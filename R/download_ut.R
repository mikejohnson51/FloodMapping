#' @title Download HAND, RC, and Cathmask rasters by HUC6
#' @description This function downloads staged HAND data to a local directory.
#' @param HUC6 A HUC6 unit to download
#' @param outdir The folder path where data should be downloaded and extracted
#' @param type The type of data to download. Options include 'catchmask'. 'hand', or 'rating curve'.
#' If 'all' is used (DEFAULT) all three type are downloaded.
#' @export


download_ut <- function(HUC6, outdir, type = 'hand'){

  files = c('catchmask', 'rating', 'hand')

  if(any(!type %in% c(files, 'all'))){
    stop(paste("Type must be one or more of: ", paste(files, collapse = ", "), "or all"))
  }

  if(type == 'all'){ type = files}

  base.url = 'http://web.corral.tacc.utexas.edu/nfiedata/HAND/'

  g = expand.grid(HUC6, type)

  urls = paste0(base.url, g$Var1, "/", g$Var1, g$Var2, '.tif')

  urls[g$Var2 == "rating"] = paste0(base.url, g$Var1, "/hydroprop-fulltable-", g$Var1, ".nohand0.csv") [g$Var2 == "rating"]


  message("Beginning download of HUC6-", HUC6, " files...")
  for(i in 1:length(urls)){
    downloader(outdir, url = urls[i])
  }
}

#' @title Function to download data from URL to out directory using httr.
#' @description General downloader
#' @param dir path to output directory
#' @param url the location of the online resource
#' @importFrom httr GET write_disk progress

downloader <- function(dir, url) {

  if (!dir.exists(dir)) { dir.create(dir, recursive = T) }

  file = paste0(dir, basename(url))

  if (!file.exists(file)) {
    message("\tDownloading ", basename(url))

    resp <-  httr::GET(url,
                         httr::write_disk(file, overwrite = TRUE),
                         httr::progress())

      if (resp$status_code != 200) {
        warning(basename(url), " Download unsuccessfull :(")
      }
  } else {
    message("\t", basename(url), " already exists ...")
  }

  return(file)
}
