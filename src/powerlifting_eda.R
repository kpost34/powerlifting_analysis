#load packages
library(here)
library(dplyr)
library(ggplot2)
library(cowplot)
library(lubridate)
library(janitor)




#I. Data import
#**************
load(file=here("data","tidy_data","tidy_usa_sbd_2020-10-16.rda"))
c_lift


#II. Data visualizations
#**********************
#1. total_kg
#A. univariate
#histogram
c_lift %>%
  ggplot(aes(total_kg)) +
  geom_histogram(color="black",fill="darkblue") +
  labs(x="Total weight (kg)",
       y="Count") +
  theme(axis.title=element_text(face="bold",size=12),
        axis.text=element_text(size=11),
        panel.background=element_blank(),
        axis.line=element_line(color="black"))
#shows bimodal pattern with peaks around 325 and 600 kg

#B. bivariate
#area plot of total weight by sex
c_lift %>%
  ggplot(aes(total_kg)) +
  geom_area(aes(y=..count..,group=sex,fill=sex),stat="bin",color="black") +
  scale_fill_manual(labels=c("Female","Male"),values=c("steelblue","darkgreen")) +
  theme_classic() +
  labs(x="Total weight (kg)",y="Count",fill="Sex")
#indicates that bimodal pattern in histogram is related to sex differences

#boxplot of total weight by sex
c_lift %>%
  ggplot(aes(x=sex,y=total_kg)) +
  geom_boxplot(fill=c("red3","steelblue")) +
  scale_x_discrete(labels=c("Female","Male")) +
  theme_classic() +
  labs(x="Sex",y="Total weight (kg)") 
#again, more evidence of sex differences; also shows huge degrees of variation in total weight lifted

#parent-federation
c_lift %>%
  filter(sex=="F",age_class=="24-34",weight_class_kg==52,!is.na(parent_federation)) %>%
  ggplot(aes(parent_federation,total_kg,fill=parent_federation)) +
  geom_boxplot() +
  scale_fill_manual(values=c("steelblue","darkred","purple","darkgreen","dodgerblue","darkorange","violet")) +
  labs(x="Parent Federation",y="Total weight lifted (kg)",fill="Parent Federation") +
  theme_bw()
#specific comparison (for sex, age class, and weight class): clearly variation in performance within and across
#events sanctioned by different parent federations; WRPF events have the highest total weight lifted for this
#comparison, whereas WPC are lowest, and IPF and IPFL are in the middle. WPA lacks data. 


#C. multivariate
#scatter plot of age and total weight by sex (filtered by date to reduce # obs)
c_lift %>%
  filter(date>"2019-07-01") %>%
  ggplot(aes(age,total_kg,color=sex)) +
  geom_point(alpha=0.02) +
  geom_smooth(method=loess) + 
  scale_color_manual(labels=c("Female","Male"),values=c("blue","red")) +
  theme_bw() +
  labs(x="Age",y="Total weight (kg)",color="Sex")
#lifting ability grows up until about 25 years old (f) and 35 years old (m) where peaks occur as indicated in 
#previous figures then diminish

#scatter plot of body weight and total weight lifted by sex (filtered by date)
c_lift %>%
  filter(date>"2019-07-01") %>%
  ggplot(aes(bodyweight_kg,total_kg,color=sex)) +
  geom_point(alpha=0.02) +
  geom_smooth(method=loess) + 
  scale_color_manual(labels=c("Female","Male"),values=c("blue","red")) +
  theme_bw() +
  labs(x="Body weight (kg)",y="Total weight (kg)",color="Sex")
#strength increases with increasing body weight but marginal gain diminishes

#body weight and total weight by age_class 
c_lift %>%
  filter(date>"2019-07-01",!is.na(age_class)) %>%
  ggplot(aes(bodyweight_kg,total_kg,color=age_class)) +
  geom_point(alpha=0.02) +
  facet_wrap(~age_class) +
  geom_smooth(method=loess) + 
  theme_bw() +
  labs(x="Body weight (kg)",y="Total weight (kg)",color="Age Class")
#some age classes lack data points...but there seems to be a pattern where in which weight lifted increases
#with body weight initially (almost linearly) then it increases at a lower rate for 16-44 but evens out or 
#*decreases* for older age classes (i.e., 45-69)

#bodyweight-total weight lifted colored by wilks scores
c_lift %>%
  ggplot(aes(bodyweight_kg,total_kg,color=wilks)) +
  scale_color_continuous(type="viridis") +
  geom_point(alpha=0.1) +
  labs(x="Bodyweight (kg)", y="Total weight lifted (kg)",color="Wilks score") +
  theme_bw()
#as expected...this shows how Wilks scores are a metric that take into account the weight lifted for
#the participant's body weight



#2. Individual lifts
#A. univariate
c_lift %>%
  ggplot() +
  geom_freqpoly(aes(best3squat_kg,color="Squat"),size=1.2) +
  geom_freqpoly(aes(best3deadlift_kg,color="Deadlift"),size=1.2,linetype=4) +
  geom_freqpoly(aes(best3bench_kg,color="Bench"),size=1.2,linetype=3) +
  scale_color_manual(name="Lift",
                     values=c("Squat"="darkred","Deadlift"="dodgerblue","Bench"="darkgreen"),
                     labels=c("Squat","Deadlift","Bench")) +
  theme_light() +
  labs(x="Weight (kg)",y="Count")
#participants tend to lift more in the deadlift followed by squat and then bench 
#each movement has a bimodal distribution, likely due to sex


#B. bivariate
#distribution by sex
s1<-c_lift %>%
  filter(sex=="F") %>%
  ggplot() +
  geom_freqpoly(aes(best3squat_kg,color="Squat"),size=1.2) +
  geom_freqpoly(aes(best3deadlift_kg,color="Deadlift"),size=1.2,linetype=4) +
  geom_freqpoly(aes(best3bench_kg,color="Bench"),size=1.2,linetype=3) +
  scale_color_manual(name="Lift",
                     values=c("Squat"="darkred","Deadlift"="dodgerblue","Bench"="darkgreen"),
                     labels=c("Squat","Deadlift","Bench")) +
  scale_x_continuous(limits=c(0,500)) +
  theme_light() +
  labs(x="Weight lifted (kg)",y="Count")

s2<-c_lift %>%
  filter(sex=="M") %>%
  ggplot() +
  geom_freqpoly(aes(best3squat_kg,color="Squat"),size=1.2) +
  geom_freqpoly(aes(best3deadlift_kg,color="Deadlift"),size=1.2,linetype=4) +
  geom_freqpoly(aes(best3bench_kg,color="Bench"),size=1.2,linetype=3) +
  scale_color_manual(name="Lift",
                     values=c("Squat"="darkred","Deadlift"="dodgerblue","Bench"="darkgreen"),
                     labels=c("Squat","Deadlift","Bench")) +
  theme_light() +
  labs(x="Weight lifted (kg)",y="Count")

plot_grid(s1, s2,
          nrow=2,
          labels=c("female", "male"),
          label_x=.75,label_y=.95,
          label_size=10)
#now the sex differences for each lift can be detected more easily


#C. multivariate
#by age and sex
a1<-c_lift %>%
  filter(date>"2020-01-01") %>%
  ggplot(aes(age,best3squat_kg,color=sex)) +
  geom_point(alpha=0.05) +
  geom_smooth(method=loess) +
  theme_bw() +
  labs(x="Age",y="Weight squatted (kg)",color="Sex")

a2<-c_lift %>%
  filter(date>"2020-01-01") %>%
  ggplot(aes(age,best3bench_kg,color=sex)) +
  geom_point(alpha=0.05) +
  geom_smooth(method=loess) +
  theme_bw() +
  labs(x="Age",y="Weight benched (kg)",color="Sex")

a3<-c_lift %>%
  filter(date>"2020-01-01") %>%
  ggplot(aes(age,best3deadlift_kg,color=sex)) +
  geom_point(alpha=0.05) +
  geom_smooth(method=loess) +
  theme_bw() +
  labs(x="Age",y="Weight deadlifted (kg)",color="Sex")

a4<-c_lift %>%
  filter(date>"2020-01-01") %>%
  ggplot(aes(age,total_kg,color=sex)) +
  geom_point(alpha=0.05) +
  geom_smooth(method=loess) +
  theme_bw() +
  labs(x="Age",y="Total weight lifted (kg)",color="Sex")
  
plot_grid(a1,a2,a3,a4,
          labels=c("squat","bench","deadlift","total"),
          label_x=.52,label_y=.97,
          label_size=10)
#1) clear sex differences (males lifting more than females per movement and overall)
#2) age-weight lifted pattern: increased weight lifted by age until about 25-30 then decreases
#3) females tend to have a flatter age-weight lifted pattern than males


#by age class and sex
c_lift %>%
  filter(!is.na(age_class)) %>%
  ggplot(aes(sex,total_kg,fill=sex)) +
  stat_summary(geom="bar") +
  stat_summary(geom="errorbar",width=0.3) +
  scale_fill_manual(labels=c("female","male"),values=c("steelblue","darkred")) +
  facet_wrap(~age_class) +
  theme_bw() +
  labs(x="Sex",y="Total weight lifted (kg)")
#as expected, males > females for each age group; a second pattern is that the discrepancy is small
#for the youngest age class and grows until 24-34/35-39 before decreasing


#by bodyweight and sex
b1<-c_lift %>%
  filter(date>"2020-01-01") %>%
  ggplot(aes(bodyweight_kg,best3squat_kg,color=sex)) +
  geom_point(alpha=0.05) +
  geom_smooth(method=loess) +
  theme_bw() +
  labs(x="Bodyweight",y="Weight squatted (kg)",color="Sex")

b2<-c_lift %>%
  filter(date>"2020-01-01") %>%
  ggplot(aes(bodyweight_kg,best3bench_kg,color=sex)) +
  geom_point(alpha=0.05) +
  geom_smooth(method=loess) +
  theme_bw() +
  labs(x="Bodyweight",y="Weight benched (kg)",color="Sex")

b3<-c_lift %>%
  filter(date>"2020-01-01") %>%
  ggplot(aes(bodyweight_kg,best3deadlift_kg,color=sex)) +
  geom_point(alpha=0.05) +
  geom_smooth(method=loess) +
  theme_bw() +
  labs(x="Bodyweight",y="Weight deadlifted (kg)",color="Sex")

b4<-c_lift %>%
  filter(date>"2020-01-01") %>%
  ggplot(aes(bodyweight_kg,total_kg,color=sex)) +
  geom_point(alpha=0.05) +
  geom_smooth(method=loess) +
  theme_bw() +
  labs(x="Bodyweight",y="Total weight lifted (kg)",color="Sex")

plot_grid(b1,b2,b3,b4,
          labels=c("squat","bench","deadlift","total"),
          label_x=.52,label_y=.97,
          label_size=10)
#1) sex differences (males > females)
#2) increasing weight lifted (for each movement) as body weight increases (almost linearly) until the rate of
#growth diminishes for both sexes 
#3) the pattern in the latter part of #2 is flatter for females than males for squat, bench, and total weight but...
#4) for deadlift, the weight lifted decreases for males after an initial growth phase (while maintaining the flat
#pattern for females)


#3. Individual Lift Comparisons
#A. squat-bench
#1. bivariate
c_lift %>%
  filter(best3squat_kg>0, best3bench_kg>0) %>%
  ggplot(aes(best3squat_kg,best3bench_kg)) +
  geom_point(alpha=0.1,color="darkred") +
  geom_smooth(method=lm,color="black")
#positive linear relationship expected; however, some extreme points (e.g., high squat-low bench)


#2. multivariate
#by sex
c_lift %>%
  filter(best3squat_kg>0, best3bench_kg>0,date>"2020-01-01") %>%
  ggplot(aes(best3squat_kg,best3bench_kg,color=sex)) +
  geom_point(alpha=0.05) +
  geom_smooth(aes(color=sex),method=lm)
#positive linear relationships for each sex, but slope is greater for males


#B. squat-deadlift
c_lift %>%
  filter(best3squat_kg>0, best3deadlift_kg>0) %>%
  ggplot(aes(best3squat_kg,best3deadlift_kg)) +
  geom_point(alpha=0.1,color="darkblue") +
  geom_smooth(method=lm,color="black")
#positive linear relationship


#C. all three lifts
#pairwise correlations
#all best lifts
c_lift %>%
  select(best3squat_kg:best3deadlift_kg) %>%
  filter(!is.na(best3squat_kg),!is.na(best3bench_kg),!is.na(best3deadlift_kg)) %>%
  cor()
#squat-deadlift is the strongest (~0.923), whereas bench-deadlift is the weakest (~0.896) 

#only successful lifts
c_lift %>%
  filter(best3squat_kg>0, best3deadlift_kg>0,best3bench_kg>0) %>%
  select(best3squat_kg:best3deadlift_kg) %>%
  filter(!is.na(best3squat_kg),!is.na(best3bench_kg),!is.na(best3deadlift_kg)) %>%
  cor()
#squat-deadlift still the strongest (~0.932), whereas bench-deadlift still the weakest (~0.905)



#4. Age
#A. univariate
#histogram
c_lift %>%
  ggplot(aes(age)) +
  geom_histogram(fill="steelblue",color="black",bins=40) +
  labs(x="Age",y="Count") +
  scale_x_continuous(breaks=seq(0,80,20),expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0),limits=c(0,22500)) +
  theme_bw()
#shows that early 20s has greatest participation


#B. bivariate
#age distribution by sex
#area graph
c_lift %>%
  ggplot(aes(age)) +
  geom_area(aes(y=..count..,group=sex,fill=sex),stat="bin",color="black") +
  scale_fill_manual(labels=c("Female","Male"),values=c("steelblue","darkgreen")) +
  theme_classic() +
  labs(x="Age",y="Count",fill="Sex")

#frequency polygon (line graph)
c_lift %>%
  ggplot(aes(age)) +
  geom_freqpoly(aes(y=..count..,group=sex,color=sex),stat="bin") +
  scale_color_manual(labels=c("Female","Male"),values=c("steelblue","darkred")) +
  theme_classic() +
  labs(x="Age",y="Count",fill="Sex")
#clearly more male than female participants; age distribution similar by sex



#5. Ratings scores
#A. bivariate
#wilks-dots
c_lift %>%
  ggplot(aes(dots,wilks,alpha=.01)) +
  geom_point(color="steelblue") +
  geom_smooth(method="lm",color="black") +
  theme(legend.position="none")

c_lift %>%
  filter(!is.na(wilks),!is.na(dots)) %>%
  select(wilks,dots) %>%
  cor()
#clearly these assessment methods are highly correlated (p = 0.998)



#6. Meets
#A. exploration
#top 10 meets by size (# of participants)
c_lift %>%
  group_by(meet_name) %>%
  count(date,sort=TRUE) 
#1. 1372 participants at Raw Nationals on 10/16/19

#top 10 meets by frequency (# of times held)
c_lift %>%
  group_by(meet_name) %>%
  summarize(events=length(unique(date))) %>%
  arrange(desc(events))
#World Championships helds the most often during time period (i.e., 13 times)

#checking dates of World Championships
c_lift %>%
  filter(meet_name=="World Championships") %>%
  distinct(date) %>%
  print(n=13)

#checking dates of Colorado State Championships (held 4th most often with 9 dates)
c_lift %>%
  filter(meet_name=="Colorado State Championships") %>%
  distinct(date) %>%
  print(n=9)

c_lift %>%
  filter(sex=="M",meet_name=="Colorado State Championships") %>%
  tabyl(age_class,date)
#shows the participation by age_class and date

c_lift %>%
  filter(sex=="M",meet_name=="Colorado State Championships",age_class=="24-34",weight_class_kg=="93") %>%
  tabyl(date)
#shows participation for males in specific age and weight classes for each date
#notice that there was participation in 6 of 9 dates


#B. bivariate
#total_kg over time (for specific sex, age class, and weight class)
c_lift %>%
  filter(sex=="M",meet_name=="Colorado State Championships",age_class=="24-34",weight_class_kg=="93",!is.na(total_kg)) %>%
  group_by(date) %>%
  mutate(avg_total_lift_kg=sum(total_kg)/n(),
         se_total_lift_kg=sd(total_kg)/n()) %>%
  ggplot() +
  geom_line(aes(date,avg_total_lift_kg),size=1.2) +
  geom_linerange(aes(x=date,
                     y=avg_total_lift_kg,
                     ymin=avg_total_lift_kg - se_total_lift_kg,
                     ymax=avg_total_lift_kg + se_total_lift_kg),
                linetype=2,
                color="darkred") +
  scale_y_continuous(limits=c(0,550)) +
  labs(x="Date",
       y="Average total weight lifted (kg)",
       title="Total weight lifted by 24-34 year-old males in the 93 kg weight class \nat the Colorado State Championships from 2015 to 2020") +
  theme(plot.title.position="plot")
#shows average total weight lifted for a specific age class, weight class, and sex at the Colorado State
#Championships over the time period of the dataset


