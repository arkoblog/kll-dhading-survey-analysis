# Load required libraries
library(dplyr)
library(leaflet)
library(plyr)
# Load data
schools_raw<-read.csv("data/School_P2_Dhading_2016_11_17_04_31_03_568497.csv")

# Backup
schools_w<-schools_raw

# QC: Number of unoique schols vs number of rows
nrow(schools_w) # 609 - Number of rows
length(unique(schools_w$general_detail.school.image_school_name)) #609 - Number of unique school names
length(unique(schools_w$general_detail.emis.school_emis)) #609 - Number of unique school emis

# Create a dataframe that counts the numer of distinct values for all columns
columnCount<-as.data.frame(apply(schools_w, 2, function(x)length(unique(x))))

####################
# Step 1: Ensuring Data Consistency - It has to be ensured that data is in the correct format for analysis
####################
##### We are going to use Emis as the primary key

# Converting school_level columns to flag variables
school_level<-schools_w[,c("general_detail.emis.school_emis","general_detail.school_level.kindergarten","general_detail.school_level.primary","general_detail.school_level.lower_secondary","general_detail.school_level.secondary","general_detail.school_level.higher_secondary","general_detail.school_level.higher_education","general_detail.school_level.other","general_detail.school_level_other")]
school_level$kg_flag<-ifelse(school_level$general_detail.school_level.kindergarten=="True",1,0)
school_level$primary_flag<-ifelse(school_level$general_detail.school_level.primary=="True",1,0)
school_level$lowsec_flag<-ifelse(school_level$general_detail.school_level.lower_secondary=="True",1,0)
school_level$sec_flag<-ifelse(school_level$general_detail.school_level.secondary=="True",1,0)
school_level$highsec_flag<-ifelse(school_level$general_detail.school_level.higher_secondary=="True",1,0)
school_level$highed_flag<-ifelse(school_level$general_detail.school_level.higher_education=="True",1,0)

names(school_level)

# School level dataset now contains gflags according to the level of education it provides
school_level<-school_level[,c(1,10,11,12,13,14,15)]

# School Demograpghics: Converting student counts, disability counts, etc. to numeric values, and cleaning columns 
student_demographics<-schools_w[,c("general_detail.emis.school_emis","general_detail.general_detail_1.number_student","general_detail.general_detail_1.number_pupils_boys","general_detail.general_detail_1.number_pupils_girls","general_detail.general_detail_1.number_pupils_special_needs","general_detail.general_detail_4.difficulty_mobility_girl","general_detail.general_detail_4.seeing_girl","general_detail.general_detail_4.hearing_girl","general_detail.general_detail_4.other_girl","general_detail.general_detail_4.specify_other_girl","general_detail.general_detail_4.difficulty_mobility_boys","general_detail.general_detail_4.seeing_boys","general_detail.general_detail_4.hearing_boys","general_detail.general_detail_4.other_boys","general_detail.general_detail_4.specify_other_boys","general_detail.number_pupils_staff_estimate","general_detail.general_detail_2.number_staff_female","general_detail.general_detail_2.number_staff_male","general_detail.general_detail_5.difficulty_mobility_girl","general_detail.general_detail_5.seeing_girl","general_detail.general_detail_5.hearing_girl","general_detail.general_detail_5.other_girl")]
# Renaming Columns
names(student_demographics)<-c("general_detail.emis.school_emis","total_number_student","number_pupils_boys","number_pupils_girls","number_pupils_special_needs","difficulty_mobility_girl","difficulty_seeing_girls","difficulty_hearing_girls", "difficulty_other_girls", "difficulty_specify_other_girls","difficulty_mobility_boys","difficulty_seeing_boys","difficulty_hearing_boys", "difficulty_other_boys", "difficulty_specify_other_boys","number_pupils_staff_esimate","total_staff_female","total_staff_male","difficulty_mobility_staff_girl","difficulty_seeing_staff_girls","difficulty_hearing_staff_girls", "difficulty_other_staff_girls")


# The add numeric function converts the column from factor to numeric datatype
addNumeric<- function(df, start, end) {
  # df<-student_demographics
  # start<-2
  # end<-9
  dfname<-as.character(comment(df))
  firstcol<-names(df[1])
  basedf<-df[firstcol]
  for (i in start:end){
    # i<-2
    colname<-as.character(names(df[i]))
    fmla<-paste0(dfname,"[",dfname,"$",colname,"!='n/a',]")
    dfnew<-eval(parse(text=fmla))
    newcolname<-paste0(colname,"_num")
    new_eval<-paste0("dfnew","$",newcolname,"<-as.numeric(as.character(dfnew","$",colname,"))")
    eval(parse(text=new_eval))
    len<-length(dfnew)
    k<-dfnew[,c(1,len)]
    basedf<-left_join(basedf,k)
  }
  return (basedf)
}


# Converting factors to numeric datatype
# Adding a comment is the only way to access the name of a data frame in the form of a string (this is needed for the AddNumeric2 function to work)
comment(student_demographics)<-"student_demographics"
student_demographics<-left_join(student_demographics,addNumeric(student_demographics,2,9))

comment(student_demographics)<-"student_demographics"
student_demographics<-left_join(student_demographics,addNumeric(student_demographics,11,14))

comment(student_demographics)<-"student_demographics"
student_demographics<-left_join(student_demographics,addNumeric(student_demographics,17,22))


student_demographics<-student_demographics[,c(1,23:40,10,15,16)]
summary(student_demographics)

student_demographics$num_bucket<- as.factor(ifelse((student_demographics$total_number_student_num>1000),"greater_than_1000",ifelse((student_demographics$total_number_student_num<20),"less_than_20", ifelse((student_demographics$total_number_student_num>=20 & student_demographics$total_number_student_num<=49),"betn_20_49", ifelse((student_demographics$total_number_student_num>=50 & student_demographics$total_number_student_num<=99), "betn_50_99", ifelse((student_demographics$total_number_student_num>=100 & student_demographics$total_number_student_num<=499), "betn_100_499",ifelse((student_demographics$total_number_student_num>=500 & student_demographics$total_number_student_num<=999), "betn_500_999",NA)))))))


write.csv(student_demographics,"school_demographics.csv",row.names = F)
write.csv(school_level,"school_levels.csv",row.names = F)


########## Plot schools
coordinates<-schools_w[,c("general_detail._gps_coordinates_latitude","general_detail._gps_coordinates_longitude")]
names(coordinates)<-c("lat","long")

# Convert coordinates to matrix
mat<-data.frame("long"=as.numeric(coordinates$long), "lat"=as.numeric(coordinates$lat))
coord_m<-as.matrix((mat))

dhading_schools<-SpatialPoints(coord=coord_m)
proj4string(schools)<-CRS("+init=epsg:4326")
dhading<-readRDS("data/dhading_VDC.Rds")

proj4string(dhading)<-NA_character_
proj4string(dhading)<-CRS("+init=epsg:4326")

# Plot map using leaflet
dhading %>% leaflet() %>% addTiles() %>% addPolygons()%>%addMarkers(data=schools, clusterOptions = markerClusterOptions()) 

# Cleaning files
schools_subset<-read.csv(file ="data/School_P2_imp_variables.csv")
sel<-grepl("_imp", names(schools_subset))


schools_subset<-schools_subset[,sel]
headers<-as.data.frame(names(schools_subset)) 
write.csv(headers,"data/headers.csv")

updated_headers <-read.csv("data/updated_headers.csv")
names(schools_subset)<-updated_headers$revised_headers
names(schools_subset)

schools_subset$school_emis<-schools_w$general_detail.emis.school_emis
saveRDS(schools_subset,file="subset_schoolData.RDS")



schools_subset$kg_flag_multi<-ifelse(schools_subset$kg_flag_multi=="True",1,0)
schools_subset$prim_flag_multi<-ifelse(schools_subset$prim_flag_multi=="True",1,0)
schools_subset$lowsec_flag_multi<-ifelse(schools_subset$lowsec_flag_multi=="True",1,0)
schools_subset$sec_flag_multi<-ifelse(schools_subset$sec_flag_multi=="True",1,0)
schools_subset$highsec_flag_multi<-ifelse(schools_subset$highsec_flag_multi=="True",1,0)
schools_subset$highed_flag_multi<-ifelse(schools_subset$highed_flag_multi=="True",1,0)

# The add Numeric function converts factors to numeric datatype
addNumeric2<- function(df, start, end) {
  # df<-schools_subset_1
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
schools_subset$maintenance_fund_flag<-ifelse(schools_subset$maintenance_fund_flag=="yes",1,ifelse(schools_subset$maintenance_fund_flag=="dk",2,0))

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
schools_subset$landslide_risk_flag<-ifelse(schools_subset$landslide_risk_flag=="yes",1,ifelse(schools_subset$landslide_risk_flag=="dk",2,0))
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
schools_subset$funding_gov_multi<-ifelse(schools_subset$funding_gov_multi=="True",1,ifelse(schools_subset$funding_gov_multi=="n/a",2,0))
schools_subset$funding_ingo_multi<-ifelse(schools_subset$funding_ingo_multi=="True",1,ifelse(schools_subset$funding_ingo_multi=="n/a",2,0))
schools_subset$funding_ngo_multi<-ifelse(schools_subset$funding_ngo_multi=="True",1,ifelse(schools_subset$funding_ngo_multi=="n/a",2,0))
schools_subset$funding_community_multi<-ifelse(schools_subset$funding_community_multi=="True",1,ifelse(schools_subset$funding_community_multi=="n/a",2,0))

# Check
schools_subset$funding_int_donor_multi<-ifelse(schools_subset$funding_int_donor_multi=="True",1,ifelse(schools_subset$funding_int_donor_multi=="n/a",2,0))



summary(schools_subset$funding_community_multi)
summary(as.factor(schools_subset$funding_int_donor_multi))
