getData = function(AOI = NULL, name = NULL, write.path = NULL){

  `%+%` = crayon::`%+%`

  if(is.null(write.path)){main.dir = normalizePath(paste0("./LIVINGFLOOD/"), mustWork = FALSE)
  } else { main.dir = normalizePath(write.path, mustWork = FALSE) }

  if(is.null(name)){
   xx =  list.files(main.dir)
   xx = xx[!grepl(".csv|RAW", xx)]
   name = paste0( "Region", sprintf("%03d", sum(grepl("Region", xx)) + 1))
  }

  raw.dir = paste0(main.dir, "/RAW")
  name.dir = paste0(main.dir, "/", name)
  meta.path = paste0(main.dir, "/meta_data.csv")

  .create.dir(main.dir)
  .create.dir(raw.dir)

  AOI  = AOI %>% findNHD()
  HUC6 = unique(substr(AOI$nhd$reachcode,1,6))

  meta = data.frame(name = name, metaAOI(AOI$AOI), HUC6 = HUC6, nhd_num = length(AOI$nhd), stringsAsFactors = FALSE)

  if(!file.exists(meta.path)){
    write.csv(meta, meta.path, row.names = FALSE)
    dir.create(name.dir)
    d = 0
  } else {
    xxx = read.csv(meta.path, stringsAsFactors = F)
    xxx[nrow(xxx) + 1, ] = meta[1,]
    d = duplicated(xxx[2:NCOL(xxx)], fromLast = TRUE )
    n = xxx[which(d, TRUE),]$name
    xxx = xxx[!d, ]
    write.csv(xxx, meta.path, row.names = FALSE)
  }

    if(sum(d) > 0){

      cat(crayon::white(paste0("\nThis region has already been processed under the name `", n, "`:")) %+% crayon::green(paste0(" renaming directory `", name, "`\n\n")))
      file.rename(paste0(main.dir, "/", n), paste0(main.dir, "/", name))
      name.dir = paste0(main.dir, "/", name)
    } else {dir.create(name.dir)}

    for(j in 1:length(HUC6)){
      downloadRaw(base.path = raw.dir, HUC6 = HUC6[j], type =  "hand")
      downloadRaw(base.path = raw.dir, HUC6 = HUC6[j], type =  "catchmask")
      downloadRaw(base.path = raw.dir, HUC6 = HUC6[j], type =  "rating")
    }

  cat(crayon::white(paste0("===============================================================================================\n")))
  cat(crayon::red(paste0("RAW data complete\n")))
  cat(crayon::white(paste0("===============================================================================================\n\n")))

  processData(AOI = AOI, raw.dir = raw.dir, write.path = name.dir)

 return(name.dir)

}



