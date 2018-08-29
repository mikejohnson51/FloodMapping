.write.shp = function(input = NULL, path, var = NULL){

if(!grepl('sf', class(input))){ input = sf::st_as_sf(input)}
if(var == 'nhdflowlines'){ names(input) <- make.names(names(input), unique = TRUE) }

suppressWarnings(
sf::st_write(input, dsn = paste0(path, "/", var, ".shp"), layer = var,
               driver = "ESRI Shapefile", delete_dsn = TRUE, quiet = TRUE)
)

}


processData = function(AOI = NULL, raw.dir = NULL, write.path = NULL){

`%+%` = crayon::`%+%`

h.path = paste0(write.path, "/hydro")
g.path = paste0(write.path, "/geo")

.create.dir(write.path, folders = c('hydro', 'geo'))

comids = AOI$nhd$comid
huc6 = unique(substr(AOI$nhd$reachcode, 1, 6))

.write.shp(AOI$AOI, g.path, "AOI")
.write.shp(AOI$nhd, g.path, "nhdflowlines")

all.files = list.files(raw.dir, paste(huc6,collapse="|"), full.names = TRUE, recursive = TRUE)

####

hand.files = all.files[grepl("hand", all.files)]
path = paste0(g.path, "/hand.tif")

if(!file.exists(path)){
  mosaic.lf(input = hand.files, AOI$AOI, write.path = path)
  cat(crayon::white("HAND data cropped and merged for", basename(write.path)), "\n")
}

catch.files = all.files[grepl("catch", all.files)]
path = paste0(g.path, "/catchmask.tif")

if(!file.exists(path)){
  mosaic.lf(input = catch.files, AOI$AOI, write.path = path)
  cat(crayon::white("CATCHMASK data cropped and merged for", basename(write.path)))
}

####
rating.files = all.files[grepl("rating", all.files)]
path = paste0(h.path, "/ratings_curves.rda")

if(!file.exists(path)){

rc = list()
ratings = NULL #Avoid global param before load

for(k in seq_along(rating.files)){
  load(rating.files[k])
  rc[[k]] = ratings[ratings$CatchId %in% comids,]
}

rating_curves = do.call(rbind, rc)
save(rating_curves, file = path, compress = 'xz')

cat(crayon::white("Rating curve data filtered and saved for", basename(write.path), "\n"))
}
}
