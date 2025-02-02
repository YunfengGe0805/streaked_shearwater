#Clear environment and set working directory
rm(list=ls())
setwd("C:/ALAN_StreakedShearwater")


datf_dn<-subset(data,timestamps >= ymd_hms("2023-09-01 00:00:00") | timestamps <=ymd_hms ("2023-05-01 00:00:00"))
datf_forage<-subset(datf_dn,states==2)
datnf_dn<-subset(data,timestamps >= ymd_hms("2023-05-01 00:00:00") &timestamps <=ymd_hms ("2023-09-01 00:00:00"))
datnf_forage<-subset(datnf_dn,states==2)


dataf<-data.frame(Time=c("Fishing","Fishing"), 
                 DayNight=c("Day","Night"),
                 Count=c(sum(datf_forage$class=="day"),sum(datf_forage$class=="night")))
datanf<-data.frame(Time=c("Fishing-ban","Fishing-ban"), 
                  DayNight=c("Day","Night"),
                  Count=c(sum(datnf_forage$class=="day"),sum(datnf_forage$class=="night")))
# Barplot
ggplot(data, aes(x=Time, y=Count,fill=DayNight)) + 
  geom_bar(position="stack", stat = "identity")+
  ggtitle("M140") +
  xlab("")

#Piechart
Ma<-read.csv(file = "m140ALANstates.csv")
mday<-subset(Ma,class=="day")
md1<-subset(mday,states==1)
md2<-subset(mday,states==2)
md3<-subset(mday,states==3)
Behaviors<-c("Resting","Foraging","Travelling")
Count<-c(nrow(md1),nrow(md2),nrow(md3))
Percentage<-c("15%","58%","27%")
data<-data.frame(Behaviors,Count,Percentage)

ggplot(data, aes(x="", y=Count,fill=Behaviors)) + 
  geom_bar(stat = "identity")+
  geom_text(aes(label = Percentage),
            position = position_stack(vjust = 0.5)) +
  coord_polar("y", start=0)+
  theme_void()+
  ggtitle("M140 Daytime Behaviors")

night<-subset(Ma,class=="night")
md1<-subset(night,states==1)
md2<-subset(night,states==2)
md3<-subset(night,states==3)
Behaviors<-c("Resting","Foraging","Travelling")
Count<-c(nrow(md1),nrow(md2),nrow(md3))
Percentage<-c("44%","44%","12%")
data<-data.frame(Behaviors,Count,Percentage)

ggplot(data, aes(x="", y=Count,fill=Behaviors)) + 
  geom_bar(stat = "identity")+
  geom_text(aes(label = Percentage),
            position = position_stack(vjust = 0.5)) +
  coord_polar("y", start=0)+
  theme_void()+
  ggtitle("M140 Nighttime Behaviors")

Fe<-read.csv(file = "f139ALANstates.csv")

mday<-subset(Fe,class=="day")
md1<-subset(mday,states==1)
md2<-subset(mday,states==2)
md3<-subset(mday,states==3)
Behaviors<-c("Resting","Foraging","Travelling")
Count<-c(nrow(md1),nrow(md2),nrow(md3))
Percentage<-c("14%","64%","23%")
data<-data.frame(Behaviors,Count,Percentage)

ggplot(data, aes(x="", y=Count,fill=Behaviors)) + 
  geom_bar(stat = "identity")+
  geom_text(aes(label = Percentage),
            position = position_stack(vjust = 0.5)) +
  coord_polar("y", start=0)+
  theme_void()+
  ggtitle("F139 Daytime Behaviors")

night<-subset(Fe,class=="night")
fn1<-subset(night,states==1)
fn2<-subset(night,states==2)
fn3<-subset(night,states==3)
Behaviors<-c("Resting","Foraging","Travelling")
Count<-c(nrow(fn1),nrow(fn2),nrow(fn3))
Percentage<-c("39%","51%","10%")
data<-data.frame(Behaviors,Count,Percentage)





ggplot(data, aes(x="", y=Count,fill=Behaviors)) + 
  geom_bar(stat = "identity")+
  geom_text(aes(label = Percentage),
            position = position_stack(vjust = 0.5)) +
  coord_polar("y", start=0)+
  theme_void()+
  ggtitle("F139 Nighttime Behaviors")


ggplot(datanf, aes(x="", y=Count,fill=DayNight)) + 
  geom_bar(stat = "identity")+
  coord_polar("y", start=0)+
  theme_void()+
  ggtitle("F139 foraging -fishing ban period")





# Libraries
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(tidyverse)
library(lubridate)

#Violin plot
m140<-read.csv(file = "m140ALANstates.csv")
m140$sex<-rep("male",times=nrow(m140))
f139<-read.csv(file = "f139ALANstates.csv")
f139$sex<-rep("female",times=nrow(f139))
data<-rbind(m140,f139)
data$sex <- as.factor(data$sex)
dat<-subset(data,class=="night")
dat<-subset(dat,ALAN>0)
datnf<-subset(dat,timestamps >= ymd_hms("2023-05-01 00:00:00") &timestamps <=ymd_hms ("2023-09-01 00:00:00"))
datf<-subset(dat,timestamps >= ymd_hms("2023-09-01 00:00:00") | timestamps <=ymd_hms ("2023-05-01 00:00:00"))

datnf%>%
  ggplot(aes(fill=sex,x=as.character(states), y=log(ALAN))) +
  geom_violin(position=position_dodge(0.75),width=1.4) +
  scale_x_discrete(labels=c('Resting', 'Foraging', 'Travelling'))+
  scale_fill_discrete(labels=c('Male', 'Female'))+
  geom_boxplot(position=position_dodge(0.75),width=0.1, color="grey", alpha=0.2) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("ALAN effect in fishing-ban period") +
  xlab("")

datf%>%
  ggplot(aes(fill=as.character(states),x=sex, y=log(ALAN))) +
  geom_violin(position=position_dodge(0.75),width=1.4) +
  scale_x_discrete(labels=c('Male', 'Female'))+
  geom_boxplot(position=position_dodge(0.75),width=0.1, color="grey", alpha=0.2) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("ALAN effect in fishing period") +
  xlab("")

datf%>%
  ggplot( aes(x=as.character(states), y=log(ALAN), fill=as.character(states))) +
  geom_violin(width=1.4) +
  scale_x_discrete(labels=c('Resting', 'Foraging', 'Travelling'))+
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("F139 fishing-ban period") +
  xlab("")

#Scatter plot
library(lubridate)
yday<-yday(data$timestamps)
ALAN<-data$ALAN
a<-data.frame(yday,ALAN)
ggplot(a, aes(x=yday, y=ALAN)) + 
  geom_point()+
  ggtitle("Average ALAN Intensity 2023") +
  ylab("DNB radiance (nanoWatts/sr/cm^2)")
