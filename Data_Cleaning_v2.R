# Load required libraries
library(dplyr)

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

head(student_demographics[is.na(student_demographics$num_bucket),])

write.csv(student_demographics,"school_demographics.csv",row.names = F)
write.csv(school_level,"school_levels.csv",row.names = F)


