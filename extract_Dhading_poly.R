library("rgdal")
library("dplyr")
library(leaflet)

npl<-lnd84<-readRDS("data/NPL_adm4.rds")

plot(npl)
glimpse(npl@data)

unique(npl@data$NAME_3)
npl_dhading<-npl[npl@data$NAME_3=="Dhading",]

plot(npl_dhading)
saveRDS(object = npl_dhading, file="data/dhading_VDC.Rds")

proj4string(npl_dhading)
proj4string(schools)<-CRS("+init=epsg:4326")

dhad_84<-spTransform(npl_dhading,CRS("+init=epsg:4326"))

dhad_84 %>% leaflet() %>% addTiles() %>% addPolygons() %>% addMarkers(data=schools)


plot(dhad_84)


plot(schools)
