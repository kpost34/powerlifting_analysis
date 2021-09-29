#load packages
library(here)
library(readr)
library(dplyr)
library(skimr)
library(janitor)
library(purrr)
library(forcats)
library(visdat)
library(ggplot2)



#I. Data import
#*************
lift<-read_csv(here("data","raw_data","usa_sbd_data_2020-10-16.csv"),
               col_types="iccccdcccdcddddddddddddddddcddcccccccc")
#purposely made factors character strings because read_csv not parsing factors appropriately
#(creating empty factor levels)


#II. Data cleaning, wrangling, and preprocessing
#***********************************************
#1. Correct parsing problems
lift
head(lift$Date,n=10)
lift$Date<-parse_date(lift$Date,format="%m/%d/%y") #parses date
lift_facs<-lift[3:38] %>%
  mutate(across(where(is.character),as.factor))
#converts all character cols (except Name) to factors
lift_facs
f_lift<-bind_cols(lift[1:2],lift_facs)
#bind ID & Name with lift_facts df

#2. Preliminary data checking
dim(f_lift) #179725 x 38
glimpse(f_lift) #col types appear appropriate; some cols have many NAs
head(f_lift,n=10); tail(f_lift,n=10) #again, NAs; also negative squat values, which does not make sense

#3. Deeper data checking: NAs, constancy
#quantify NAs
skim(f_lift) #missingness...also constancy, distribution
sort(map_int(f_lift,~sum(is.na(.)))/179725,TRUE) #prop NA per col
#remove lift attempts (1-4) for each lift, MeetTown (b/c of excessive missingness)
#remove Tested (b/c of missingness and not highly informative)

#constancy
tabyl(f_lift$Equipment); tabyl(f_lift$Event); tabyl(f_lift$MeetCountry)
#100% of each variable (as expected), so remove
#SBD = squat, bench, and deadlift, which is filter of data before downloading
#same with Equipment (= raw), which means they used minimal equipment
#same with MeetCountry (= USA), which was expected

#other
tabyl(f_lift,AgeClass); tabyl(f_lift,BirthYearClass) #remove latter b/c former is more complete, informative, and granular


#4. Data cleaning
#remove columns for missingness, constancy, lack of informativeness
c_lift<-select(f_lift, 
               -c(Event,Equipment,BirthYearClass,Squat1Kg:Squat4Kg,Bench1Kg:Bench4Kg,
                  Deadlift1Kg:Deadlift4Kg,Tested,MeetCountry,MeetTown))

#col names
c_lift<-clean_names(c_lift) #clean col names
names(c_lift)[1]<-"id" #rename first col

#factors
c_lift %>%
  select(where(is.factor)) %>%
  names()
#returns names of the 9 factor columns

c_lift %>%
  select(where(is.factor)) %>%
  map(levels) %>%
  head()
#returns a subset of the levels of each factor
#some factors (e.g., age_class, division) appear to have incorrect levels
#other factors (e.g., weight_class_kg, place) are not in an intuitive order (e.g., numeric)

#correct incorrect factor levels
levels(c_lift$age_class) #"12-May" is actually age class 5-12
levels(c_lift$division) #"10-Sep" is 9-10, "11-Oct" is 10-11, etc for division

c_lift$age_class<-fct_recode(c_lift$age_class,"5-12"="12-May")
c_lift$division<-fct_recode(c_lift$division, 
                            "8-9"="9-Aug","9-10"="10-Sep","10-11"="11-Oct","11-12"="12-Nov","12-13"="13-Dec")

#correct order of factor levels (i.e., division, weight_class_kg, place)
c_lift$division<-fct_relevel(c_lift$division, "8-9") #put "8-9" first in order of levels
levels(c_lift$division) #checks out

c_lift$place<-fct_relevel(c_lift$place,c(as.character(1:120),"DD","DQ","G","NS")) 
#re-order levels such that place numbers are first and in numeric order (1-120) and special categories
#(e.g., DD, DQ) follow
levels(c_lift$place) #checks out

#assess missingness (following cleaning)
skim(c_lift)
vis_miss(c_lift,warn_large_data=FALSE) 
sort(map_int(c_lift,~sum(is.na(.)))/179725,TRUE)
#0 NAs for id, name, sex, place, federation, date, and meet name
#parent_federation and age > 5% missing
#rest are less than 5% missing

#5. Digging deeper/data exploration (regarding NAs)
#parent_federation
c_lift %>%
  filter(is.na(parent_federation)) %>%
  group_by(federation) %>%
  count() %>%
  print(n=22) 
#22 powerlifting federations that sanction events unaffiliated with parent federations

#age
sum(is.na(c_lift$age))

c_lift %>%
  filter(is.na(age)) %>%
  group_by(age_class) %>%
  count()
#11264 NAs for age; 8162 that also have missing age class
#remaining obs are distributed among all the other age classes

#age_class
c_lift %>%
  filter(is.na(age_class)) %>%
  select(age) %>%
  is.na() %>%
  sum()
#all (8162) obs missing age are also missing age class

#total_kg
c_lift %>%
  filter(is.na(total_kg)) %>%
  group_by(place) %>%
  count()
#all missing total_kg values were either disqualified for procedure or failed attempt (DQ), 
#disqualified for failing a drug test (DD), or no-showed event (NS)

#best3bench_kg
c_lift %>%
  filter(is.na(best3bench_kg)) %>%
  group_by(place) %>%
  count()
#where it's NA, there are still some place values 

c_lift %>%
  filter(is.na(best3bench_kg),!(place %in% c("DQ", "NS", "DD"))) %>%
  nrow()
#total of 24 placed with NA in best3bench_kg

c_lift %>%
  filter(is.na(best3bench_kg),place %in% c("DQ", "NS", "DD")) %>%
  nrow()
#3805 DS, DD, or NS

c_lift %>%
  filter(is.na(best3bench_kg)) %>%
  group_by(total_kg) %>%
  count() %>%
  print(n=21)
#3805 NAs for total_kg (as expected)

#best3bench_kg either 1) has a value, 2) is NA with an NA in total_kg and either a 
#DQ, NS, or DD in place, or 3) is NA with a value in total_kg and a numeric place

  
#best3deadlift_kg
c_lift %>%
  filter(is.na(best3deadlift_kg)) %>%
  group_by(place) %>%
  count()
#appears to be similar to bench press: actual places and DQ, NS, and DD

c_lift %>%
  filter(is.na(best3deadlift_kg),!(place %in% c("DQ", "NS", "DD"))) %>%
  select(place, total_kg, best3deadlift_kg)
#deadlift NAs that placed all have total_kg values


#best3squat_kg
c_lift %>%
  filter(is.na(best3squat_kg)) %>%
  group_by(place) %>%
  count()
#again, similar to bench and deadlift

c_lift %>%
  filter(is.na(best3squat_kg),!(place %in% c("DQ", "NS", "DD"))) %>%
  select(place, total_kg, best3squat_kg)
#again, squat NAs that have actual places also have values for total_kg


#meet_state
c_lift %>%
  filter(is.na(meet_state)) %>%
  group_by(meet_name) %>%
  count() %>%
  print(n=55)
#meets that are missing a state have a meet name

#bodyweight_kg
c_lift_wc_groups<-c_lift %>%
  filter(is.na(bodyweight_kg)) %>%
  group_by(weight_class_kg) %>%
  count()

c_lift_wc_groups
#some (70) inds missing bodyweight_kg have weight_class_kg

sum(c_lift_wc_groups[2]) #180 NAs for body weight (and 70 NAs for weight class too)

#weight_class_kg
c_lift %>%
  filter(is.na(weight_class_kg)) %>%
  group_by(bodyweight_kg) %>%
  count()
#8 obs with NA in weight_class_kg and non-NA in bodyweight_kg

c_lift %>%
  filter(date=='2020-03-07',meet_name=="Arnold Grand Prix by SBD")
#seems like these were all from the same meet


#division
sum(is.na(c_lift$division)) #only 52 NAs

c_lift %>%
  filter(is.na(division)) %>%
  group_by(meet_name) %>%
  count()
#looks like one meet did not have divisions

c_lift %>%
  filter(meet_name=="Raw Unity IX") %>%
  nrow()
#all participants in this meet did not have a division


#6. Deeper data-checking and cleaning
#check ranges of values
c_lift %>%
  select(where(is.numeric)) %>%
  map_df(range,na.rm=TRUE)
#all values but the best of each lift are fine
#best3-squat/bench/deadlift-_kg have negative values (which mean unsuccessful attempts)

#look at these data more closely...
#squat
c_lift %>%
  ggplot(aes(best3squat_kg)) +
  geom_histogram(color="black",fill="darkred")
#clearly there are sub-zero values

c_lift %>%
  ggplot(aes(best3bench_kg)) +
  geom_histogram(color="black",fill="darkred")

c_lift %>%
  ggplot(aes(best3deadlift_kg)) +
  geom_histogram(color="black",fill="darkred")
#same for bench and deadlift

c_lift %>%
  filter(best3squat_kg<0 | best3bench_kg<0 | best3deadlift_kg<0)
#785 obs have at least one negative value

c_lift %>%
  filter((best3squat_kg<0 | best3bench_kg<0 | best3deadlift_kg<0) 
         & is.na(total_kg) & place=="DQ" & is.na(dots) & is.na(wilks))

c_lift %>%
  filter((best3squat_kg<0 | best3bench_kg<0 | best3deadlift_kg<0) 
         & !is.na(total_kg) & place !="DQ" & !is.na(dots) & !is.na(wilks))
#no obs have at least one negative best lift and the following 
#1) non-NA for total_kg, 2) non-DQ for place, 3) non-NA for dots, 
#4) non-NA for wilks

#replaced negative (failed) lifts with 0s
c_lift$best3squat_kg[c_lift$best3squat_kg<0]<-0
c_lift$best3bench_kg[c_lift$best3bench_kg<0]<-0
c_lift$best3deadlift_kg[c_lift$best3deadlift_kg<0]<-0

#check ranges again
c_lift %>%
  select(where(is.numeric)) %>%
  map_df(range,na.rm=TRUE)
#now they all make sense

#check other aspects of data
dim(c_lift) #179725 x 20
glimpse(c_lift)
summary(c_lift)
skim(c_lift)


#III. Data export
#****************
write_csv(c_lift,here("data","tidy_data","tidy_usa_sbd_2020-10-16.csv"))
save(c_lift,file=here("data","tidy_data","tidy_usa_sbd_2020-10-16.rda"))
#saved tidy data as .csv and .rda files
#csv allows browsing using other software
#rda allow retention of R properties (e.g., factor level order)


