getData = function(AOI = NULL, name = NULL, write.path = NULL){

  if(!(class(AOI) %in% c("list","HydroData"))){AOI = list(AOI = AOI)}

  if(is.null(name)){name = revGeo(mean(AOI$AOI@bbox[2,]), mean(AOI$AOI@bbox[1,]))}

  if(is.null(write.path)){main.dir = normalizePath(paste0("./LIVINGFLOOD/"), mustWork = FALSE)
  } else { main.dir = normalizePath(path, mustWork = FALSE) }

  raw.dir = paste0(main.dir, "/RAW")
  name.dir = paste0(main.dir, "/", name)
  meta.path = paste0(main.dir, "/meta_data.csv")

  if(!dir.exists(main.dir)){ dir.create(main.dir)}
  if(!dir.exists(raw.dir)){ dir.create(raw.dir)}

  AOI  = AOI %>% findNHD()
  huc6 = unique(substr(AOI$nhd$reachcode,1,6))

  meta = data.frame(name = name, revAOI(AOI$AOI), huc6 = huc6, nhd_num = length(AOI$nhd), stringsAsFactors = FALSE)

  if(!file.exists(meta.path)){
    write.csv(meta, meta.path, row.names = FALSE)
    dir.create(name.dir)
    } else {
    xxx = read.csv(meta.path, stringsAsFactors = F)
    xxx[nrow(xxx) + 1, ] = meta[1,]
    d = duplicated(xxx[2:NCOL(xxx)], fromLast = TRUE )
    n = xxx[which(d, T),]$name
    xxx = xxx[!d, ]
    write.csv(xxx, meta.path, row.names = FALSE)

    if(sum(d) > 0){
      message("This region has already been processed under the name `", n, "` renaming directory `", name, "`")
      file.rename(paste0(main.dir, "/", n), paste0(main.dir, "/", name))

    for(i in seq_along(huc6)){
      downloadRaw(raw.dir, huc6[i], "hand")
      downloadRaw(raw.dir, huc6[i], "catchmask")
      downloadRaw(raw.dir, huc6[i], "rating")
    }
    } else {
      dir.create(name.dir)
    }
  }

  processData(AOI,
              raw.dir = raw.dir,
              write.path = name.dir)

 return(AOI)

}



