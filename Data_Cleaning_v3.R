library(sp)
library(rgeos)
library(maptools)
library(raster)   ## To convert an "Extent" object to a "SpatialPolygons" object.
library(rgeos)
library(tmap)


schools_raw<-read.csv("data/School_P2_Dhading_2016_11_17_04_31_03_568497.csv")
schools_subset<-read.csv(file ="data/School_P2_imp_variables.csv")
sel<-grepl("_imp", names(schools_subset))


schools_subset<-schools_subset[,sel]
headers<-as.data.frame(names(schools_subset)) 
write.csv(headers,"data/headers.csv")

updated_headers <-read.csv("data/updated_headers.csv")
names(schools_subset)<-updated_headers$revised_headers
names(schools_subset)

schools_subset$school_emis<-schools_raw$general_detail.emis.school_emis
saveRDS(schools_subset,file="subset_schoolData.RDS")



schools_subset$kg_flag_multi<-ifelse(schools_subset$kg_flag_multi=="True",1,0)
schools_subset$prim_flag_multi<-ifelse(schools_subset$prim_flag_multi=="True",1,0)
schools_subset$lowsec_flag_multi<-ifelse(schools_subset$lowsec_flag_multi=="True",1,0)
schools_subset$sec_flag_multi<-ifelse(schools_subset$sec_flag_multi=="True",1,0)
schools_subset$highsec_flag_multi<-ifelse(schools_subset$highsec_flag_multi=="True",1,0)
schools_subset$highed_flag_multi<-ifelse(schools_subset$highed_flag_multi=="True",1,0)

# The add Numeric function converts factors to numeric datatype
addNumeric2<- function(df, start, end) {
  # df<-schools_subset
  # start<-12
  # end<-16
  dfname<-as.character(comment(df))
  firstcol<-names(df[85])
  basedf<-df[firstcol]
  for (i in start:end){
    # i<-start+2
    
    colname<-as.character(names(df[i]))
    fmla<-paste0(dfname,"[",dfname,"$",colname,"!='n/a',]")
    dfnew<-eval(parse(text=fmla))
    newcolname<-paste0(colname)
    new_eval<-paste0("dfnew","$",newcolname,"<-as.numeric(as.character(dfnew","$",colname,"))")
    eval(parse(text=new_eval))
    len<-length(dfnew)
    col_of_interest<-paste0("dfnew[,c('school_emis','",colname,"')]")
    k<-eval(parse(text=col_of_interest))
    
    basedf<-left_join(by = "school_emis",basedf,k)
    # join with main df
    text<-paste0("df$",colname,"=basedf$",colname)
    eval(parse(text=text))
  }
  
  return (df)
}

# Convert columns 12-16 to numeric datatype
# schools_subset_1<-schools_subset
comment(schools_subset)<-"schools_subset"
schools_subset<-addNumeric2(schools_subset,12,16)

# Maintenance commitee
schools_subset$maintenance_committee_flag<-ifelse(schools_subset$maintenance_committee_flag=="yes",1,0)
schools_subset$maintenance_fund_flag<-ifelse(schools_subset$maintenance_fund_flag=="yes",1,ifelse(schools_subset$maintenance_fund_flag=="dk",NA,0))

# Geographic Coordinates
schools_subset$lat<-as.numeric(as.character(schools_subset$lat))
schools_subset$long<-as.numeric(as.character(schools_subset$long))
schools_subset$elev<-as.numeric(as.character(schools_subset$elev))
schools_subset$precision<-as.numeric(as.character(schools_subset$precision))

# Material type to flags
schools_subset$material_unfired_brick_multi<-ifelse(schools_subset$material_unfired_brick_multi=="True",1,0)
schools_subset$material_fired_brick_multi<-ifelse(schools_subset$material_fired_brick_multi=="True",1,0)
schools_subset$material_stome_multi<-ifelse(schools_subset$material_stome_multi=="True",1,0)
schools_subset$material_timber_multi<-ifelse(schools_subset$material_timber_multi=="True",1,0)
schools_subset$material_bamboo_multi<-ifelse(schools_subset$material_bamboo_multi=="True",1,0)
schools_subset$material_other_multi<-ifelse(schools_subset$material_other_multi=="True",1,0)

# Travel Time to building material
# schools_subset_1<-schools_subset
comment(schools_subset)<-"schools_subset"
schools_subset<-addNumeric2(schools_subset,29,32)

# Travel Time to major town
comment(schools_subset)<-"schools_subset"
schools_subset<-addNumeric2(schools_subset,35,38)

# Landslide risk flag
schools_subset$landslide_risk_flag<-ifelse(schools_subset$landslide_risk_flag=="yes",1,ifelse(schools_subset$landslide_risk_flag=="dk",NA,0))
schools_subset$landslide_history_flag<-ifelse(schools_subset$landslide_history_flag=="yes",1,0)

# General topography
schools_subset$general_topo_flat_multi<-ifelse(schools_subset$general_topo_flat_multi=="True",1,0)
schools_subset$general_topo_terrace_multi<-ifelse(schools_subset$general_topo_terrace_multi=="True",1,0)
schools_subset$general_topo_sloping_site_multi<-ifelse(schools_subset$general_topo_sloping_site_multi=="True",1,0)
schools_subset$general_topo_sch_near_slope_base<-ifelse(schools_subset$general_topo_sch_near_slope_base=="True",1,0)
schools_subset$general_topo_top_of_hill_multi<-ifelse(schools_subset$general_topo_top_of_hill_multi=="True",1,0)

# Road Condition
revalue(schools_subset$road_condition,c("NA"="not_available"))

# Funding
schools_subset$funding_gov_multi<-ifelse(schools_subset$funding_gov_multi=="True",1,ifelse(schools_subset$funding_gov_multi=="n/a",NA,0))
schools_subset$funding_ingo_multi<-ifelse(schools_subset$funding_ingo_multi=="True",1,ifelse(schools_subset$funding_ingo_multi=="n/a",NA,0))
schools_subset$funding_ngo_multi<-ifelse(schools_subset$funding_ngo_multi=="True",1,ifelse(schools_subset$funding_ngo_multi=="n/a",NA,0))
schools_subset$funding_community_multi<-ifelse(schools_subset$funding_community_multi=="True",1,ifelse(schools_subset$funding_community_multi=="n/a",NA,0))
schools_subset$funding_int_donor_multi<-ifelse(schools_subset$funding_int_donor_multi=="True",1,ifelse(schools_subset$funding_int_donor_multi=="n/a",NA,0))
schools_subset$funding_local_donor_multi<-ifelse(schools_subset$funding_local_donor_multi=="True",1,ifelse(schools_subset$funding_local_donor_multi=="n/a",NA,0))
schools_subset$funding_dk_multi<-ifelse(schools_subset$funding_dk_multi=="True",1,ifelse(schools_subset$funding_dk_multi=="n/a",NA,0))

# TLC
schools_subset$tlc_kg_multi<-ifelse(schools_subset$tlc_kg_multi=="True",1,ifelse(schools_subset$tlc_kg_multi=="n/a",NA,0))
schools_subset$tlc_prim_multi<-ifelse(schools_subset$tlc_prim_multi=="True",1,ifelse(schools_subset$tlc_prim_multi=="n/a",NA,0))
schools_subset$tlc_lowsec_multi<-ifelse(schools_subset$tlc_lowsec_multi=="True",1,ifelse(schools_subset$tlc_lowsec_multi=="n/a",NA,0))
schools_subset$tlc_sec_multi<-ifelse(schools_subset$tlc_sec_multi=="True",1,ifelse(schools_subset$tlc_sec_multi=="n/a",NA,0))
schools_subset$tlc_highsec_multi<-ifelse(schools_subset$tlc_highsec_multi=="True",1,ifelse(schools_subset$tlc_highsec_multi=="n/a",NA,0))
schools_subset$tlc_highed_multi<-ifelse(schools_subset$tlc_highed_multi=="True",1,ifelse(schools_subset$tlc_highed_multi=="n/a",NA,0))
schools_subset$tlc_other_multi<-ifelse(schools_subset$tlc_other_multi=="True",1,ifelse(schools_subset$tlc_other_multi=="n/a",NA,0))

# Proximity of vulnerable Location
schools_subset$prox_vulnerable_buildings<-ifelse(schools_subset$prox_vulnerable_buildings=="True",1,0)
schools_subset$prox_chemical_factories<-ifelse(schools_subset$prox_chemical_factories=="True",1,0)
schools_subset$prox_forest<-ifelse(schools_subset$prox_forest=="True",1,0)
schools_subset$prox_large_tree<-ifelse(schools_subset$prox_large_tree=="True",1,0)
schools_subset$prox_none<-ifelse(schools_subset$prox_none=="True",1,0)
schools_subset$prox_other<-ifelse(schools_subset$prox_other=="True",1,0)
schools_subset$prox_perimeter_walls<-ifelse(schools_subset$prox_perimeter_walls=="True",1,0)
schools_subset$prox_water_towers<-ifelse(schools_subset$prox_water_towers=="True",1,0)
schools_subset$prox_utility_plant<-ifelse(schools_subset$prox_utility_plant=="True",1,0)

# Space Heating
schools_subset$space_heating<-ifelse(schools_subset$space_heating=="yes",1,ifelse(schools_subset$space_heating=="dk",NA,0))


# Generate Summary Statistics
summary(schools_subset)


# Preparing geographical data
########## Plot schools
coordinates<-schools_subset[,c("long","lat")]
names(coordinates)<-c("long","lat")

# Convert coordinates to matrix and create spatial ponts object
mat<-data.frame("long"=as.numeric(coordinates$long), "lat"=as.numeric(coordinates$lat))
coord_m<-as.matrix((mat))
dhading_schools_pts<-SpatialPointsDataFrame(coords=coord_m, data=schools_subset)

# Load Dhading VDC Polygons
dhading_polygons<-readRDS("data/dhading_VDC.Rds")

# Correct projections
proj4string(dhading_schools_pts)<-CRS("+init=epsg:4326")
proj4string(dhading_polygons)<-NA_character_
proj4string(dhading_polygons)<-CRS("+init=epsg:4326")

# Create backups of spatial objects 
ddng_sch<-dhading_schools_pts
ddng_poly<-dhading_polygons


# Plot map using leaflet
ddng_poly %>% leaflet() %>% addTiles() %>% addPolygons(popup=~OBJECTID)
  # addMarkers(data=ddng_sch, clusterOptions = markerClusterOptions()) 

# Plot using plot
ddng_poly@data<-ddng_poly@data[,c("OBJECTID","NAME_4")]

# Kalleri is assigned to two polygons. We habe to split it into two polygons and merge it back with the original dataframe
ddng_poly2<-ddng_poly[(ddng_poly$NAME_4=="Kalleri"),]
gCentroid(ddng_poly2)

CP <- as(extent(80, 90, 27.86835, 28.5), "SpatialPolygons")
proj4string(CP)<-NA_character_
proj4string(CP)<-CRS("+init=epsg:4326")

# Viewing the polygons
ddng_poly2 %>% leaflet() %>% addTiles() %>% addPolygons() %>%addPolygons(data=CP)
dhuwakot <- gIntersection(CP, ddng_poly2)
plot(dhuwakot)

CP <- as(extent(80, 90,25,27.86835), "SpatialPolygons")
proj4string(CP)<-NA_character_
proj4string(CP)<-CRS("+init=epsg:4326")

# Viewing the polygons
ddng_poly2 %>% leaflet() %>% addTiles() %>% addPolygons() %>%addPolygons(data=CP)
kalleri <- gIntersection(CP, ddng_poly2)
plot(kalleri)


# Removing incorrect polygons and merging new ones
merged_polygons<-union(dhuwakot,kalleri)

# Create a data_frame for merged polygons 
dw_kl_df<-data.frame("OBJECTID"=c(100,200), "NAME_4"=c("Dhuwakot","Kalleri"))
merged_polygons_df<-SpatialPolygonsDataFrame(merged_polygons, data=dw_kl_df)

# Remove Kalleri from old Dataframe
ddng_poly_wo_kalleri<-ddng_poly[ddng_poly$NAME_4!="Kalleri",]

# Combine the two
ddng_poly<-union(ddng_poly_wo_kalleri,merged_polygons_df)

# Correct data frame
ddng_poly$OBJECTID<-ifelse(is.na(ddng_poly$OBJECTID.1),as.character(ddng_poly$OBJECTID.2), as.character(ddng_poly$OBJECTID.1))
ddng_poly$NAME_4<-ifelse(is.na(ddng_poly$NAME_4.1),as.character(ddng_poly$NAME_4.2), as.character(ddng_poly$NAME_4.1))

ddng_poly<-ddng_poly[,c("OBJECTID","NAME_4")]
# Plot map using leaflet
ddng_poly %>% leaflet() %>% addTiles() %>% addPolygons(popup=~NAME_4)

# Now we correct vdc names in school points and Poly dataframe
uniq_kll_vdcs<-as.data.frame(unique(schools_subset$vdc_label))
uniq_ddng_poly_vdcs<-as.data.frame(ddng_poly$NAME_4)

mapping<-read.csv("data/vdc_name_map.csv")
head(mapping)

ddng_poly$vdc_label<-mapping$surveyname


ddng_sch_data<-ddng_sch@data
num_of_schools<-aggregate(ddng_sch_data, by=list(ddng_sch_data$vdc_label), FUN=length)
num_of_schools<-num_of_schools[,c("Group.1","school_name")]
ddng_poly@data<-left_join(ddng_poly@data,num_of_schools, by=c("vdc_label"="Group.1"))
colnames(ddng_poly@data)<-c("OBJECTID","NAME_4","vdc_label","tot_sch")

ddng_sch@data$max_ed<-ifelse(ddng_sch$highed_flag_multi==1, "Higher Ed.", ifelse((ddng_sch$highsec_flag_multi==1),"Higher Sec.", ifelse((ddng_sch$sec_flag_multi==1),"Sec.",ifelse((ddng_sch$lowsec_flag_multi==1),"Lower Sec.", ifelse((ddng_sch$prim_flag_multi==1),"Primary", ifelse((ddng_sch$kg_flag_multi==1),"KG",NA)))))) 
ddng_sch@data$max_ed<-as.factor(ddng_sch$max_ed)

ddng_sch$approx_dist_to_road<-revalue(ddng_sch$approx_dist_to_road,c("betn_1_3km"="Between 1 Km to 3 Km", "betn_3_5km"="Between 3 Km to 5 Km","betn_500m_1km"="Between 500m to 1 Km", "betn_5_10km"="Between 5 Km to 10 Km", "less_than_500m"="Less than 500m", "more_than_10km"="More than 10 Km"))




# save geographic data for further analysis
saveRDS(ddng_poly, file="data/ddng_poly.Rds")
saveRDS(ddng_sch, file="data/ddng_sch.Rds")

