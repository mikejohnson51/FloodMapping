#' @title Download, process, and save HAND, catchmask and Rating Curve Data
#' @param AOI a AOI spatial object to map floods within
#' @param dir a main working directory for all floodmapping projects
#' @param name a name for current project
#' @return a list of file paths
#' @export
#' @importFrom dplyr pull mutate bind_rows
#' @importFrom fst write.fst
#' @importFrom raster raster getValues
#' @importFrom sf st_transform write_sf

getRawData = function(AOI, dir, name){

  raw.dir <- raw.files <- type <- NULL

  HUC6 = findHUC6(AOI, level = 6) %>% dplyr::pull(HUC6)

    xx = lapply(HUC6, download_ut, outdir = dir)

    # Make name folder
    name.dir = paste0(dir,"//", name)
    catch.path = paste0(name.dir, "/catchmask_", name,".tif")
    hand.path = paste0(name.dir, "/hand_", name,".tif")

    if(!file.exists(catch.path) | !file.exists(hand.path)){

    if(!dir.exists(name.dir)){ dir.create(name.dir)}
    # Write AOI
    aoi.path = paste0(name.dir, "//", name, ".shp")
    AOI = AOI %>% st_transform(3857)
    sf::write_sf(AOI, dsn = aoi.path)

    # Build list
    to_process = data.frame(HUC6 = rep(HUC6, each = 2),
                            name = name,
                            raw.files = grep("tif", list.files(raw.dir, pattern = paste(HUC6, collapse = "|"), full.names = TRUE), value = TRUE),
                            stringsAsFactors = FALSE) %>%
      mutate(cropped = paste0(name.dir, "/", basename(raw.files))) %>%
      mutate( type = ifelse(grepl("hand", raw.files), "HAND", "CATCH")) %>%
      mutate(method = ifelse(type == "HAND", 'bilinear', 'near'))

    # Crop
    for(i in 1:nrow(to_process)){
      crop_project(to_process$raw.files[i], to_process$cropped[i],
                   to_process$name[i], aoi.path, to_process$method[i])
    }

    # Align
    for(i in 1:length(HUC6)){ align_rasters(HUC6[i], name.dir) }

    # Merge Rasters
    files = merge_rasters(name.dir, name)

    comids = unique(getValues(raster(files$catch.path)))

    } else {
      files = list(hand.path = hand.path, catch.path = catch.path)
    }

    files$rating.path =  paste0(name.dir, '/rating_',name,'.fst')

    if(!file.exists(files$rating.path)){

    lapply(HUC6, get_rc_table, raw.dir = dir, comids = comids ) %>%
      bind_rows() %>%
      fst::write.fst(path = files$rating.path)
    }

    return(files)

}



