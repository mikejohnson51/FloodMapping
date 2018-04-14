#' Getting Most Current NWM data
#'
#'This function accesses the NOAA Nomads server to download the most up to date short term forcast NetCDF files.
#'These files cover the spatial domain of the continential United States. These files will be stored in the 'NetCDFs/Current' folder
#'created using the 'build_files()' function in the NWM package. Each time this function is run the old files will be removed.
#'Therefore if you see something interesting be sure to save the data elsewhere!
#'
#' @examples
#' get_current()
#' @author
#' Mike Johnson
#' @export
#' @return
#' This fuction saves the 18 NetCDF files related to the most recent short term forecast to the NetCDFs/Current folder.



get_nomads = function(type = NULL, time = NULL) {


date = gsub("-","", format(Sys.Date(), tz = "GMT")) ## get the current data

if(is.null(type)){ type = "medium" }

base.url <- paste0("http://nomads.ncep.noaa.gov/pub/data/nccf/com/nwm/prod/nwm.", date,  "/", type, "_range/") ## set base URL

files = readLines(base.url) ## get available files

fileList = lapply(regmatches(files, gregexpr('(\").*?(\")', files, perl = TRUE)), function(y) gsub("^\"|\"$", "", y)) ## subset file names

fileList = fileList[grep("channel", fileList)] ## Extract channel data

if( type == "medium"){
for( i in seq(18, 0, -6) ){
  time = paste0("t", sprintf("%02d", i))
  fileList_time = fileList[grep(time, fileList)] ## Extract time of interest
  if(length(fileList_time) != 0) {break}
}
}

if ( type == "short"){
  time = strptime(format(Sys.time(), tz = "GMT"), format = "%Y-%m-%d %H:%M:%S")$hour

  for( i in seq(time, 0, -1) ){
    time = paste0("t", sprintf("%02d", i))
    fileList_time = fileList[grep(time, fileList)] ## Extract time of interest
    if(length(fileList_time) != 0) {break}
  }
}


fileList.fin = head(fileList_time, 3) # Limit to Two day

urls = paste0(base.url, fileList.fin)

for (i in seq_along(urls)){
  if(!file.exists(urls[i])){
  download.file(url = urls[i], mode = "wb", destfile = paste0("./LivingFlood/Raw/forecasts/", fileList.fin[i]))
  }
}
}

