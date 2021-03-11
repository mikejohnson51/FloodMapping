#' @title Download, process, and save HAND, catchmask and Rating Curve Data
#' @param AOI a AOI spatial object to map floods within
#' @param dir a main working directory for all floodmapping projects
#' @param name a name for current project
#' @return a list of file paths
#' @export
#' @importFrom dplyr pull mutate bind_rows
#' @importFrom raster raster getValues writeRaster
#' @importFrom sf st_transform write_sf st_cast
#' @importFrom fasterize fasterize

getRawData = function(AOI, dir, name){

  HUC6 = findHUC6(AOI, level = 6)$huc6

  lapply(HUC6, download_ut, outdir = dir)

    # Make name folder
    name.dir = paste0(dir,"//", name)
    catch.path = paste0(name.dir, "/catchmask_", name,".tif")
    hand.path = paste0(name.dir, "/hand_", name,".tif")

  if(!file.exists(catch.path) |
     !file.exists(hand.path)){

    if(!dir.exists(name.dir)){ dir.create(name.dir) }
    # Write AOI
    aoi.path = paste0(name.dir, "//", name, ".gpkg")
    AOI      = st_transform(AOI, 3857)

    sf::write_sf(AOI, dsn = aoi.path)

    # Build list
    to_process = data.frame(HUC6 = rep(HUC6, each = 1),
                            name = name,
                            raw.files = grep("tif$",
                                             list.files(dir, pattern = paste(HUC6, collapse = "|"),
                                             full.names = TRUE), value = TRUE),
                            stringsAsFactors = FALSE) %>%
      mutate(cropped = paste0(name.dir, "/", basename(.data$raw.files))) %>%
      mutate(type    = ifelse(grepl("hand", .data$raw.files), "HAND", "CATCH")) %>%
      mutate(method  = ifelse(.data$type == "HAND", 'bilinear', 'near'))

    # Crop
    message('Cropping and Projecting Rasters...')
      for(i in 1:nrow(to_process)){
        crop_project(input    = to_process$raw.files[i],
                     output   = to_process$cropped[i],
                     name     = to_process$name[i],
                     aoi.path = aoi.path,
                     method   = to_process$method[i])
      }

    # Align
    message('Aligning Rasters...')
      for(i in 1:length(HUC6)){
        align_rasters(huc6 = HUC6[i], name.dir)
      }

    # Merge Rasters
    message('Merging Rasters...')
    files  = merge_rasters(name.dir, name)
    hand = raster(hand.path)
    cat = nhdplusTools::get_nhdplus(AOI, realization = "catchment")
    catchmask = fasterize::fasterize(sf::st_cast(cat), hand, field = "featureid")
    writeRaster(catchmask, catch.path)
    #comids = unique(getValues(raster(files$catch.path)))

    files = list(hand.path = hand.path,
                 catch.path = catch.path)

    } else {
      message('Files Already Processed...')
      files = list(hand.path = hand.path,
                   catch.path = catch.path)
    }

    return(files)
}



