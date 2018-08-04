processData = function(AOI = NULL, raw.dir = NULL, write.path = NULL){

h.path = paste0(write.path, "/hydro")
g.path = paste0(write.path, "/geo")

if(!dir.exists(h.path)){ dir.create(h.path)}
if(!dir.exists(g.path)){ dir.create(g.path)}

bb = AOI$AOI
comids = AOI$nhd$comid
huc6 = substr(AOI$nhd$reachcode, 1, 6)
names(AOI$nhd) <- make.names(names(AOI$nhd), unique = TRUE)

suppressWarnings({

if(!file.exists(paste0(g.path, "/AOI.shp"))){
sf::st_write(sf::st_as_sf(bb), dsn = paste0(g.path, "/AOI.shp"), layer = "AOI",
               driver = "ESRI Shapefile", delete_dsn = TRUE, quiet = TRUE)
}

if(!file.exists(paste0(g.path, "/nhdflowlines.shp"))){
sf::st_write(sf::st_as_sf(AOI$nhd), dsn = paste0(g.path, "/nhdflowlines.shp"), layer = "nhd",
               driver = "ESRI Shapefile", delete_dsn = TRUE, quiet = TRUE)
}

})

toMatch = paste(huc6,collapse="|")

all.files = list.files(raw.dir, toMatch, full.names = TRUE, recursive = TRUE)

####

hand.files = all.files[grepl("hand", all.files)]
  path = paste0(g.path, "/hand.tif")

if(!file.exists(path)){
  mosaic.lf(input = hand.files, bb, write.path = path)
}

message("`HAND` data processed")
####

catch.files = all.files[grepl("catch", all.files)]
  path = paste0(g.path, "/catchmask.tif")

if(!file.exists(path)){
  mosaic.lf(input = catch.files, bb, write.path = path)
}

message("`Catchmask` data processed")

####
rating.files = all.files[grepl("rating", all.files)]
  path = paste0(h.path, "/ratings_curves.rda")

rc = list()
for(k in seq_along(rating.files)){
  load(rating.files[1])
  rc[[k]] = ratings[ratings$CatchId %in% comids,]
}

rating_curves = do.call(rbind, rc)
save(rating_curves, file = path, compress = 'xz')

message("`Rating Curve` data processed")

}
