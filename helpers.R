rc.files = list.files('/Volumes/Seagate5tb/FloodMappingStudies/data/09262016_Cedar_River_at_Vinton_IA/HAND', pattern = 'rating', full.names = T)

xx = list()
for(i in 1:length(rc.files)){
  load(rc.files[i])
  xx[[i]] = rc
}

rating = do.call(rbind, xx)

rating = data.frame(rating)

load("/Volumes/Seagate5tb/FloodMappingStudies/data/09262016_Cedar_River_at_Vinton_IA/flows/2016092615_flows.rda")

flows = items$flows


y = get_stage(flows = flows, rating_curves = rating)

head(y)

stage = data.frame(y)

save(stage, file = "/Volumes/Seagate5tb/FloodMappingStudies/data/09262016_Cedar_River_at_Vinton_IA/flows/2016092615_stage.rda", compress = 'xz')


library(sf)

nhd = read_sf("/Volumes/Seagate5tb/FloodMappingStudies/data/09262016_Cedar_River_at_Vinton_IA/geo/nhdflowlines.shp")

unique(substr(nhd$reachcd, 1,6))



sum(flows$COMIDS == vec)


sum(items$flows$COMIDS %in% rating$CatchId)

6947800 %in% items$flows$COMIDS
6947800 %in% rating$CatchId
