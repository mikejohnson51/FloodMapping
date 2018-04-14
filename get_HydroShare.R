#' Hourly Value NWM Data Retrival
#'
#' Imports National Water Model streamflow data from the Hydroshare Data Explore or a folder of previously downloaded NETCDF files.
#'
#' @param comids used to identify NHD reaches to subset CONUS forecast. Can be given as a numeric vector, a AOI shapefile (use defineAOI()) or a shapefile of NHD flowlines
#' @param startDate character starting date for data retrieval in the form YYYY-MM-DD for data retrivial, YYYY-MM-DD-HH-MM-SS for folder of NETCDF files.
#' @param endDate character ending date for data retrieval in the form YYYY-MM-DD. Only used with config = 'analysis_assim'
#' @param config character describing what forecast congiguration to access. Options are 'analysis_assim', 'short_range', 'medium_range', or 'long_range'
#' @param time numeric decribing time a forecast is made in UTC timezone. Must be between 0 and 23.
#' @param forecast numeric describing number of hours forcasted out from defined 'time'
#' @param keep.flowlines logical. If TRUE, the NHD shapefile will be returned with flow data (cannot be used if comids defined by vector)
#' @param keep.basemap logical. If TRUE, the google basemap will be returned with flow data in a list
#' @param path character to folder of NETCDF files.
#' @param interval.hr logical. Hourly step between folder of NETCDF files
#'
#' @examples
#' \dontrun{
#' #Get NWM data for November 15 - 30, 2017 for the HUC8 surrondining UCSBs campus
#'
#' ucsb.flow = readNWM(comids = get_WBD(location = 'UCSB', level = 10),
#'                     startDate = "2017-11-15", endDate = "2017-11-20",
#'                     config = "analysis_assim", time = 0, forecast = 0,
#'                     keep.flowlines = T, keep.basemap = T)
#'
#' #Get data from download forecast of the 2015 Christmas Day flood in Tuscaloosa, AL:
#'
#' al.flow = readNWM(comids = define_AOI(clip_unit = list("National Water Center", 10, 10)),
#'                   path = "/RetroData_12.25.2015", startDate = "2015-12-25 00:00:00",
#'                   interval.hr = 1)
#'}
#' @export
#' @author
#' Mike Johnson

get_forecast_files = function(
                  startDate = NULL,
                  endDate = NULL,
                  config = "medium_range",
                  time = NULL,
                  forecast = NULL
                  ){

  config = "medium_range"
  param = 'channel'
  startDate = "2018-04-10"
  time = 0
  f = 1
  endDate = "2018-04-11"

  ########

if(is.null(endDate)){ endDate = startDate}
dates =  seq.Date(as.Date(startDate), as.Date(endDate), 1)

today = as.Date(format(Sys.Date(), tz = "GMT"))
yesterday = today - 1

if(c(today, yesterday) %in% dates){ method = 'NOAA NOMADS'} else { method = "HYDROSHARE"}
message("Data being downloaded from ", method)

# ________ ENTER NOMADS METHOD ___________ #

# ________ ENTER HS METHOD ___________ #


if(all(config == 'analysis_assim',  as.Date(startDate) <= as.Date('2016-05-28'))){stop("Data for 'analyis_assim' configuration only avialable after 2016-05-28")}

if(all(config != 'analysis_assim', any(startDate < (today - 40), endDate < (today - 40)))){stop(gsub("(\\b[a-z])", "\\U\\1", tolower(gsub("_", " ", config)), perl=TRUE), " forecasts  archived for 40 days. Select date after ",
                                                                                                                   today - 40, " or 'analysis_assim' configuration.")}



if(!(config %in% c("short_range", "long_range", "medium_range", "analysis_assim"))){stop("Forecast param must be either (1)'short_range', (2)'long_range', (3)'medium_range, (4)'analysis_assim'")}

if(all(any(config == 'analysis_assim', config == 'short_range'), any(max(time) > 23, min(time) < 0))){stop("Time must be between 0 and 23 for ", tolower(gsub("_", " ", config)))}
if(all(config == 'medium_range', !(time %in% c(0,6,12,18)))){stop("Time must be between 0, 6, 12, or 18 for medium range")}


  if(!is.null(endDate) ){ endDate = paste0("&endDate=", endDate)} else {endDate = NULL}
  if(!is.null(time) ){ time = paste0("&time=", paste(time,collapse=","))} else {time = NULL}
  if(!is.null(param)){ param = paste0("&geom=",  paste(param,collapse=","))} else {param = NULL}



getFilelist = paste0("https://appsdev.hydroshare.org/apps/nwm-data-explorer/api/GetFileList/?",
                         "config=", config,
                         "&startDate=",dates,"&time=",
                         paste0(time,collapse = ","),
                         param)


files = NULL

for(i in seq_along(getFilelist)){
  files = rbind(files, readLines(getFilelist[i], warn =F))
}


fileList_serve = as.vector(sapply(regmatches(files, gregexpr('(\").*?(\")', files, perl = TRUE)), function(y) gsub("^\"|\"$", "", y))) ## subset file names



#############

if(all(config == "short_range" , is.null(f))){ f =  1:18          }
if(all(config == "medium_range", is.null(f))){ f =  seq(3,240, 3) }

files = fileList_serve[grep(paste0("f", sprintf("%03d", f)),  fileList_serve)]

print(files)


     for (i in 1:length(files)){
       message(paste0("Downloading netcdf file (", i, "/", length(files), ")"))
       download.file(url = paste0("https://appsdev.hydroshare.org/apps/nwm-data-explorer/api/GetFile?file=", files[i]), mode = "wb", destfile = paste0("./LivingFlood/Raw/forecasts", files[i]), quiet = T)
     }





#if(!is.null(forecast)){
#  if(config == "analysis_assim"){
#    toMatch = c(sprintf('%02d', forecast))
#  } else if(config == "short_range"){
#    toMatch = c(sprintf('%03d', forecast))
#  }
#    files = unique(grep(paste0("tm", toMatch, collapse = "|"), files, value = TRUE))
#  }




