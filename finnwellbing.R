
###set working directory
setwd("C:/Users/tangxin/OD/Work/Post-doc/Urban and Rual Finland/Tang")

setwd("C:/Users/tangxin/OD/Work/Post-doc/Urban and Rual Finland/Tang/finnwellbeinggap")

library(tidyverse)
library(foreign)
library(lme4)
library(lmerTest)
library(nlme)
library(car)
library(effects)
library(lattice)
library(emmeans)
library(sjstats)


#read raw dataset
thl0617 <- read.spss("ktk2006_2017_tang_en.sav", to.data.frame=TRUE)

#save as Rdata
save(thl0617, file = "thl0617.RData")

load("thl0617.RData")

#subset the data for the use
thl0617 <- subset(thl0617, select=c("year_cohort","year","School_ID","Resi_cat","Region"
                                    ,"gender","School_year","Age","likeschool_nuR","likeschool",
                                    "q1burnout","q2burnout", "q3burnout","ind_school_burnout",
                                    "moedu","faedu"))

#change data to nurimec
thl0617$q1burnout.n <- as.numeric(thl0617$q1burnout)
thl0617$q2burnout.n <- as.numeric(thl0617$q2burnout)
thl0617$q3burnout.n <- as.numeric(thl0617$q3burnout)
thl0617$ind_bur <- as.numeric(thl0617$ind_school_burnout)
thl0617$ind_bur <- recode(thl0617$ind_bur, "1=0; 2=1")

#missing rate checking
#https://cran.r-project.org/web/packages/naniar/vignettes/naniar-visualisation.html
library(naniar)
library(UpSetR)

n_var_miss(thl0617)

vis_miss(thl0617,
         warn_large_data = F)

gg_miss_upset(thl0617)
gg_miss_var(thl0617, show_pct = T)

gg_miss_fct(x = thl0617, fct = year_cohort)
gg_miss_case(thl0617, facet = year_cohort)

##count

ggplot(thl0617, aes(year_cohort, fill=Resi_cat))+
  geom_bar()+
  scale_fill_discrete(name = "Residence")+
  theme_bw(base_size = 22)
ggsave("resi_count_0617.png", scale = 2)


ggplot(subset(thl0617, !is.na(gender)), aes(year_cohort, fill=gender))+
  geom_bar()+
  scale_fill_manual(name = "Gender", values = c("Girl" = "red", "Boy" = "blue"))+
  theme_bw(base_size = 22)
ggsave("gender_count_0617.png", scale = 2)


#####Urban rural gap####
##plot school liking, urban-rural
ggplot(thl0617, 
       aes(y=likeschool_nuR, x=year_cohort, group=Resi_cat, color=Resi_cat)) +
  stat_summary_bin(fun.y = "mean", geom = "line", size=2)+
  labs(x = "Year", y="School Liking")+
  scale_color_discrete(name = "Residence")+
  theme_bw(base_size = 22)

ggsave("resi_schoolliking_0617.png", scale = 2)

##plot overwhelming, urban-rural
ggplot(thl0617, 
       aes(y=q1burnout.n, x=year_cohort, group=Resi_cat, color=Resi_cat)) +
  stat_summary_bin(fun.y = "mean", geom = "line", size=2)+
  labs(x = "Year", y="Overwhelming")+
  scale_color_discrete(name = "Residence")+
  theme_bw(base_size = 22)

ggsave("resi_overwhelming_0617.png", scale = 2)


##plot cynicism, urban-rural
ggplot(thl0617, 
       aes(y=q2burnout.n, x=year_cohort, group=Resi_cat, color=Resi_cat)) +
  stat_summary_bin(fun.y = "mean", geom = "line", size=2)+
  labs(x = "Year", y="Cynicism")+
  scale_color_discrete(name = "Residence")+
  theme_bw(base_size = 22)

ggsave("resi_cynicism_0617.png", scale = 2)


##plot inadequacy, urban-rural
ggplot(thl0617, 
       aes(y=q3burnout.n, x=year_cohort, group=Resi_cat, color=Resi_cat)) +
  stat_summary_bin(fun.y = "mean", geom = "line", size=2)+
  labs(x = "Year", y="Inadequacy")+
  scale_color_discrete(name = "Residence")+
  theme_bw(base_size = 22)

ggsave("resi_inadequacy_0617.png", scale = 2)

##plot burnout, urban-rural
ggplot(thl0617, 
       aes(y=ind_bur, x=year_cohort, group=Resi_cat, color=Resi_cat)) +
  stat_summary_bin(fun.y = "mean", geom = "line", size=2)+
  labs(x = "Year", y="Burnout")+
  scale_color_discrete(name = "Residence")+
  theme_bw(base_size = 22)

ggsave("resi_burnout_0617.png", scale = 2)


#####Gender gap####
#plot school liking,gender
ggplot(subset(thl0617, !is.na(gender)), 
       aes(y=likeschool_nuR, x=year_cohort, group=gender, color=gender)) +
  stat_summary_bin(fun.y = "mean", geom = "line", size=2)+
  labs(x = "Year", y="School Liking")+
  scale_color_manual(name = "Gender", values = c("Girl" = "red", "Boy" = "blue"))+
  theme_bw(base_size = 22)

ggsave("gender_schoolliking_0617.png", scale = 2)

#plot overwhelming,gender

ggplot(subset(thl0617, !is.na(gender)), 
       aes(y=q1burnout.n, x=year_cohort, group=gender, color=gender)) +
  stat_summary_bin(fun.y = "mean", geom = "line", size=2)+
  labs(x = "Year", y="Overwhelming")+
  scale_color_manual(name = "Gender", values = c("Girl" = "red", "Boy" = "blue"))+
  theme_bw(base_size = 22)

ggsave("gender_overwhelming_0617.png", scale = 2)


#plot cynicism,gender

ggplot(subset(thl0617, !is.na(gender)), 
       aes(y=q2burnout.n, x=year_cohort, group=gender, color=gender)) +
  stat_summary_bin(fun.y = "mean", geom = "line", size=2)+
  labs(x = "Year", y="Cynicism")+
  scale_color_manual(name = "Gender", values = c("Girl" = "red", "Boy" = "blue"))+
  theme_bw(base_size = 22)

ggsave("gender_cynicism_0617.png", scale = 2)


#plot inadequacy,gender

ggplot(subset(thl0617, !is.na(gender)), 
       aes(y=q3burnout.n, x=year_cohort, group=gender, color=gender)) +
  stat_summary_bin(fun.y = "mean", geom = "line", size=2)+
  labs(x = "Year", y="Inadequacy")+
  scale_color_manual(name = "Gender", values = c("Girl" = "red", "Boy" = "blue"))+
  theme_bw(base_size = 22)

ggsave("gender_inadequacy_0617.png", scale = 2)


##plot burnout, gender
ggplot(subset(thl0617, !is.na(gender)), 
       aes(y=ind_bur, x=year_cohort, group=gender, color=gender)) +
  stat_summary_bin(fun.y = "mean", geom = "line", size=2)+
  labs(x = "Year", y="Burnout")+
  scale_color_manual(name = "Gender", values = c("Girl" = "red", "Boy" = "blue"))+
  theme_bw(base_size = 22)

ggsave("gender_burnout_0617.png", scale = 2)