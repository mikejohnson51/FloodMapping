#' @title Download streamflow forecasts
#' @description Wrapper around the NWM package to download National Water Model data from the HydroShare THREDDS server
#' @param name.dir the directory with processed geospatial data
#' @param config the model configureation (default is 'medium_range')
#' @param startDate the date of interest given as "YYYYMMDD" defaults to current date
#' @param t time of forecast
#' @param f hours foward from t
#' @param n if f is null n can be used to limit the number of files returned
#' @return a path to the processed data directory
#' @export
#' @author Mike Johnson

getFlows = function(name.dir, config = 'medium_range', startDate = NULL,  t  = NULL, f = NULL, n = NULL){

  `%+%` = crayon::`%+%`

  AOI = sf::read_sf(paste(name.dir, 'geo', 'AOI.shp', sep = "/")) %>% sf::as_Spatial()

  f = as.numeric(gsub("f", "", eval(parse(text = paste0("nwm::nwm$", config, "$meta$flist")))))[1:f]

  filelist = nwm::getFilelist(config, type = 'channel', startDate, t = t, f = f, n = n)

  cat(crayon::white(paste0("Downloading flow data for ", length(filelist), " timesteps\n")))

  flows.raw = nwm::downloadNWM(AOI = AOI, filelist = filelist, param = 'streamflow')
  flows.df = data.frame(flows.raw$streamflow)[,1:3]

  cat(crayon::white(paste0(dim(flows.df)[1], " records archived.\n")))

  flows = stats::reshape(flows.df, idvar = "COMIDS", timevar = "DateTime", direction = "wide")
  names(flows) = c("COMIDS", paste('timestep', c(1:(dim(flows)[2]-1))))

  save(flows, file = paste0(name.dir, "/hydro/flows.rda"), compress = 'xz')

  cat(crayon::white("Streamflow data saved to: ") %+% crayon::yellow("/hydro/flows.rda\n"))

  return(name.dir)

}




