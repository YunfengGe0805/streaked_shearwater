#Clear environment and set working directory
rm(list=ls())
setwd("C:/ALAN_StreakedShearwater")

#load packages
library(momentuHMM); library(ggplot2); library(dplyr)
library(lubridate)
library(tidyverse)
#load data
load(file="一些试过没啥用的东西/m140rs.Rdata")
data<-m140rs%>%
  select(ID,
         long,
         lat,
         timestamps)
dat <- prepData(data,type='LL',coordNames = c("long","lat"))

# INITIALISE HMM DATA ---------------------------------------------------------
#Note: initial value guessed based on the paper Behavioural mapping of a pelagic seabird: combining multiple sensors and a hidden Markov model reveals the distribution of at-sea behaviour
### initial step distribution natural scale parameters
stepPar0 <- c(0.78,2.41,10.68,0.78,2.41,10.68)
### initial angle distribution natural scale parameters
anglePar0 <- c(0,pi,0,1,0.5,1)

# RUN THE NULL MODEL ------------------------------------------------------

#  first run null models with no covariates on transition probabilities
set.seed(0805)
stateNames <- c("Raft","Forage", "Travel")

m1 <- fitHMM(data=dat, nbStates=3,
             dist=list(step="gamma",angle="vm"),
             Par0=list(step=stepPar0, angle=anglePar0),
             estAngleMean = list(angle=TRUE),
             stateNames = stateNames)
states<-viterbi(m1)
m140wholeyear<-cbind(data,states)
write.csv(m140wholeyear,file="m140wholeyear.csv")

##Add day/night
library(tidyverse)
library(suncalc)
data<-read.csv(file="m140wholeyear.csv")
dat<-data%>%
  dplyr::select(date=timestamps,
                datetime=timestamps,
                lat,
                lon=long)

dat$date <- as.Date(dat$date)
sun <- getSunlightTimes(data = dat, tz = "Asia/Shanghai", 
                        keep = c("sunrise", "sunset", "night", "nightEnd"))
xy<-cbind(dat,sun)
#xy <- merge(x = dat, 
#            y = sun[, c("lat", "sunrise", "sunset", "night", "nightEnd")], 
#            by = "lat")

xy$period <- rep(" ", length.out = nrow(xy))
xy$period[xy$datetime > xy$sunrise & xy$datetime < xy$sunset] <- "day"
xy$period[xy$datetime > xy$sunset & xy$datetime < xy$night] <- "night"
xy$period[xy$datetime > xy$nightEnd & xy$datetime < xy$sunrise] <- "night"
xy$period[xy$period == " "] <- "night"

datafinal <- cbind(data,xy$period)
write.csv(datafinal,file="m140wholeyear.csv")
#Piechart
Ma<-read.csv(file="m140wholeyear.csv")
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



##################################################################################################
#straightness
load(file="RawData/sw_NB_22to24_envall.Rdata")
data<-as.data.frame(sw_NB_22to24_envall)
data<-subset(data,ID=="SYSUL140"&NBYear=="NB2324"&period=="night")
data <- data %>%
  mutate(time_diff = as.numeric(difftime(timestamps, lag(timestamps), units = "hours")),
         burst_id = cumsum(coalesce(time_diff, 0) > 1)+ 1)
write.csv(data,file="m1402324night.csv")

library(trajr)

coords<-data%>%
  select(long,
         lat,
         avg_rad_5,
         rad_log_5,
         burst_id)

#plot(trj, lwd = 1, lty = 1)

t.list <- vector(mode = "list", length =110)
d.list<- vector(mode = "list", length =110)
m.list<-vector(mode = "list", length =110)
s.list<-vector(mode = "list", length =110)
for (i in 1:length(t.list)) {
  print(i)
  d.list[[i]]<-subset(coords,burst_id == i )
  m.list[[i]]<-max(d.list[[i]]$avg_rad_5)
  t.list[[i]] <- TrajFromCoords(d.list[[i]],xCol = "long",yCol = "lat")
  s.list[[i]] <- TrajStraightness(t.list[[i]])
}

max<-as.data.frame(t(as.data.frame(m.list)))
straightness<-as.data.frame(t(as.data.frame(s.list)))


