#' @title Download streamflow forecasts
#' @description Wrapper around the NWM package to download National Water Model data from the HydroShare THREDDS server
#' @param name.dir the directory with processed geospatial data
#' @param config the model configureation (default is 'medium_range')
#' @param date the date of interest given as "YYYYMMDD" defaults to current date
#' @param t the time a forcast is made (t = 1:23) defaults to most current forecast
#' @param f the number of time period into the future (eg how many flood raster to make)
#' @return a path to the processed data directory
#' @export
#' @author Mike Johnson

getFlows = function(name.dir, config = 'medium_range', date = NULL, t = NULL, f = 8){

  `%+%` = crayon::`%+%`

  AOI = sf::read_sf(paste(name.dir, 'geo', 'AOI.shp', sep = "/")) %>% sf::as_Spatial()

  f = as.numeric(gsub("f", "", eval(parse(text = paste0("nwm::nwm$", config, "$meta$flist")))))[1:f]

  filelist = nwm::getFilelist(config, type = 'channel', date, t, f = f)

  cat(crayon::white(paste0("Downloading flow data for ", length(filelist), " timesteps\n")))

  flows = nwm::downloadNWM(AOI = AOI, filelist = filelist, param = 'streamflow')

  cat(crayon::white(paste0(dim(flows$streamflow)[1], " records archived.\n")))

  flows = stats::reshape(flows$streamflow, idvar = "COMIDS", timevar = "DateTime", direction = "wide")

  save(flows, file = paste0(name.dir, "/hydro/flows.rda"), compress = 'xz')

  cat(crayon::white("Streamflow data saved to: ") %+% crayon::yellow("/hydro/flows.rda\n"))

  return(name.dir)

 }

