# Load required libraries
library(dplyr)

# Load data
schools_raw<-read.csv("data/School_P2_Dhading_2016_11_17_04_31_03_568497.csv")

schools_w<-schools_raw
variables<-as.data.frame(names(schools_w))

# QC: Number of unoique schols vs number of rows
nrow(schools_w) # 609 - Number of rows
length(unique(schools_w$general_detail.school.image_school_name)) #609 - Number of unique school names
length(unique(schools_w$general_detail.emis.school_emis)) #609 - Number of unique school emis
rowcount<-as.data.frame(apply(schools_w, 2, function(x)length(unique(x)))) #storing rowcounts of all variables

# Converting school_level to flag variables
school_level<-schools_w[,c("general_detail.emis.school_emis","general_detail.school_level.kindergarten","general_detail.school_level.primary","general_detail.school_level.lower_secondary","general_detail.school_level.secondary","general_detail.school_level.higher_secondary","general_detail.school_level.higher_education","general_detail.school_level.other","general_detail.school_level_other")]
school_level$kg_flag<-ifelse(school_level$general_detail.school_level.kindergarten=="True",1,0)
school_level$primary_flag<-ifelse(school_level$general_detail.school_level.primary=="True",1,0)
school_level$lowsec_flag<-ifelse(school_level$general_detail.school_level.lower_secondary=="True",1,0)
school_level$sec_flag<-ifelse(school_level$general_detail.school_level.secondary=="True",1,0)
school_level$highsec_flag<-ifelse(school_level$general_detail.school_level.higher_secondary=="True",1,0)
school_level$highed_flag<-ifelse(school_level$general_detail.school_level.higher_education=="True",1,0)

school_level<-a[,c(1,9,10,11,12,13,14)]

# Number of Students
student_demographics<-schools_w[,c("general_detail.emis.school_emis","general_detail.general_detail_1.number_student","general_detail.general_detail_1.number_pupils_boys","general_detail.general_detail_1.number_pupils_girls","general_detail.general_detail_1.number_pupils_special_needs","general_detail.general_detail_4.difficulty_mobility_girl","general_detail.general_detail_4.seeing_girl","general_detail.general_detail_4.hearing_girl","general_detail.general_detail_4.other_girl","general_detail.general_detail_4.specify_other_girl","general_detail.general_detail_4.difficulty_mobility_boys","general_detail.general_detail_4.seeing_boys","general_detail.general_detail_4.hearing_boys","general_detail.general_detail_4.other_boys","general_detail.general_detail_4.specify_other_boys","general_detail.number_pupils_staff_estimate","general_detail.general_detail_2.number_staff_female","general_detail.general_detail_2.number_staff_male","general_detail.general_detail_5.difficulty_mobility_girl","general_detail.general_detail_5.seeing_girl","general_detail.general_detail_5.hearing_girl","general_detail.general_detail_5.other_girl")]
# Renaming Columns
names(student_demographics)<-c("general_detail.emis.school_emis","total_number_student","number_pupils_boys","number_pupils_girls","number_pupils_special_needs","difficulty_mobility_girl","difficulty_seeing_girls","difficulty_hearing_girls", "difficulty_other_girls", "difficulty_specify_other_girls","difficulty_mobility_boys","difficulty_seeing_boys","difficulty_hearing_boys", "difficulty_other_boys", "difficulty_specify_other_boys","number_pupils_staff_esimate","total_staff_female","total_staff_male","difficulty_mobility_staff_girl","difficulty_seeing_staff_girls","difficulty_hearing_staff_girls", "difficulty_other_staff_girls")
stu_dem_1<-student_demographics[student_demographics$total_number_student!="n/a",]

# Count number of rows where boys_estimate=n/a
length(stu_dem_1[stu_dem_1$number_pupils_boys=="n/a",]) #24
length(stu_dem_1[stu_dem_1$number_pupils_girls=="n/a",]) #24

# Remove them rows
rm(studem_2)
stu_dem_2<-stu_dem_1[stu_dem_1$number_pupils_boys!="n/a",] 
nrow(stu_dem_2)#597

stu_dem_3<-stu_dem_2[stu_dem_2$number_pupils_girls!="n/a",] #still 597, means its the same rows
nrow(stu_dem_3)


comment(stu_dem_3)<-"stu_dem_3"

stu_dem_3[,2][[2]]
length(stu_dem_3[,2])

convertToNumeric <-function(df, colstart, colend) {
  for (i in colstart:colend){
    df[,i]<-as.numeric(as.character(df[,i]))  
  }
  return (df)
}

comment(stu_dem_4)<-"stu_dem_4"

addNumeric<- function(df, columnNumber) {
  dfname<-as.character(comment(df))
  colname<-as.character(names(df[columnNumber]))
  fmla<-paste0(dfname,"[",dfname,"$",colname,"!='n/a',]")
  dfnew<-eval(parse(text=fmla))
  newcolname<-paste0(colname,"_num")
  new_eval<-paste0("dfnew","$",newcolname,"<-as.numeric(as.character(dfnew","$",colname,"))")
  eval(parse(text=new_eval))
  len<-length(dfnew)
  k<-dfnew[,c(1,len)]
  return (k)
}


comment(stu_dem_4)="stu_dem_4"

eval(parse(text="stu_dem_2"))

glimpse(stu_dem_4)

stu_dem_4<-convertToNumeric(stu_dem_3,2,4)
stu_dem_5<-stu_dem_4[stu_dem_4$number_pupils_special_needs!="n/a",]
stu_dem_5$number_pupils_special_needsnum<-as.numeric(as.character(stu_dem_5$number_pupils_special_needs))

comment(stu_dem_4)<-"stu_dem_4"
stu_dem_4<-left_join(stu_dem_4,addNumeric(stu_dem_4,2))
comment(stu_dem_4)<-"stu_dem_4"
stu_dem_4<-left_join(stu_dem_4,addNumeric(stu_dem_4,3))
comment(stu_dem_4)<-"stu_dem_4"
stu_dem_4<-left_join(stu_dem_4,addNumeric(stu_dem_4,4))
comment(stu_dem_4)<-"stu_dem_4"


stu_dem_4<-left_join(stu_dem_4,addNumeric(stu_dem_4,5))
comment(stu_dem_4)<-"stu_dem_4"
stu_dem_4<-left_join(stu_dem_4,addNumeric(stu_dem_4,6))
comment(stu_dem_4)<-"stu_dem_4"
stu_dem_4<-left_join(stu_dem_4,addNumeric(stu_dem_4,7))
comment(stu_dem_4)<-"stu_dem_4"
stu_dem_4<-left_join(stu_dem_4,addNumeric(stu_dem_4,8))
comment(stu_dem_4)<-"stu_dem_4"
stu_dem_4<-left_join(stu_dem_4,addNumeric(stu_dem_4,9))
comment(stu_dem_4)<-"stu_dem_4"
stu_dem_4<-left_join(stu_dem_4,addNumeric(stu_dem_4,10))
comment(stu_dem_4)<-"stu_dem_4"
stu_dem_4<-left_join(stu_dem_4,addNumeric(stu_dem_4,11))
comment(stu_dem_4)<-"stu_dem_4"
stu_dem_4<-left_join(stu_dem_4,addNumeric(stu_dem_4,5))
comment(stu_dem_4)<-"stu_dem_4"



write.csv(student_demographics,"student_demographics.csv")

summary(a)

unique(school_level$sc_level)
a<-a[,c(1:14)]
# 
# schools_w$gd_kg_flag<-ifelse(schools_w$general_detail.school_level.kindergarten=="True",1,0)
# schools_w$gd_primary_flag<-ifelse(schools_w$general_detail.school_level.primary=="True",1,0)
# schools_w$gd_lowsec_flag<-ifelse(schools_w$general_detail.school_level.lower_secondary=="True",1,0)
# schools_w$gd_sec_flag<-ifelse(schools_w$general_detail.school_level.secondary=="True",1,0)
# schools_w$gd_kg<-ifelse(schools_w$general_detail.school_level.kindergarten=="True",1,0)

write.table(rowcount,"variables.txt", sep = "\t")
unique(schools_w$general_detail.school_level.kindergarten)


# vectors have variables of _one_ type
c(1, 2, "three")
# shorter arguments are recycled
(1:3) * 2
(1:4) * c(1, 2)
# warning! (why?)
(1:4) * c(1,2,3)




head(schools_w, n=2)
