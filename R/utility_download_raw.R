downloadRaw = function(base.path, HUC6, type){

  if (!dir.exists(paste0(base.path, "/hand"))) {
    dir.create(paste0(base.path, "/hand"))
  }

  if (!dir.exists(paste0(base.path, "/catchmask"))) {
    dir.create(paste0(base.path, "/catchmask"))
  }

  if (!dir.exists(paste0(base.path, "/rating"))) {
    dir.create(paste0(base.path, "/rating"))
  }

  if(!(type %in% c("hand", "catchmask", "rating"))){ stop(type, " not found.") }

  for (i in 1:length(HUC6)) {

    base.url = paste0("https://web.corral.tacc.utexas.edu/nfiedata/HAND/",
                      HUC6[i],
                      "/")

    if (type %in% c('hand', 'catchmask')) {
      tmp = paste0(base.path, "/", type, "/", type, "_", HUC6[i], ".tif")

      if (!file.exists(tmp)) {
        message("Downloading `", type , "` data")
        curl::curl_download(
          url = paste0(base.url, HUC6[i], type, ".tif"),
          destfile = tmp,
          mode = 'wb',
          quiet = FALSE
        )
      } else { message("`", type , "` data already exists in ", basename(base.path))}
    }

   if (type == 'rating') {

    tmp = paste0(base.path, "/rating/rating_", HUC6[i], ".rda")

    if (!file.exists(tmp)) {
      message("Downloading `", type , "` data")
      csv.path = paste0(base.url, "hydroprop-fulltable-", HUC6[i], ".nohand0.csv")

      ratings <-  data.table::fread(csv.path,
                                    select = c("CatchId", "Stage", 'Discharge (m3s-1)'))
      save(ratings, file = tmp, compress = 'xz')
    } else { message("`", type , "` data already exists in ", basename(base.path))}
  }
  }
}
