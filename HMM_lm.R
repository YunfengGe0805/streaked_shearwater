#Clear environment and set working directory
rm(list=ls())
setwd("C:/ALAN_StreakedShearwater")
#Load packages
library(move)
library(momentuHMM)
library(stats)
library(tidyverse)
library(lubridate)
library(raster)
library(sf)
library(terra)
library(dplyr)
library(suncalc)
library(lme4)
library(ggplot2)
library(sjPlot)
#Data preparation
##Load dataset
cred <- movebankLogin(username="Yachang Cheng", password="welove107")
searchMovebankStudies(x="Qingdao", login=cred)
winter21_22 <- getMovebankData(study="Tracking seabirds in the Yellow Sea of Qingdao", login=cred,timestamp_start="20211101000000000",timestamp_end="20220401000000000")
save(winter21_22,file="winter21_22.Rdata")
winter22_23 <- getMovebankData(study="Tracking seabirds in the Yellow Sea of Qingdao", login=cred,timestamp_start="20221101000000000",timestamp_end="20230401000000000")
save(winter22_23,file="winter22_23.Rdata")
winter23_24 <- getMovebankData(study="Tracking seabirds in the Yellow Sea of Qingdao", login=cred,timestamp_start="20231101000000000",timestamp_end="20240401000000000")
save(winter23_24,file="winter23_24.Rdata")
##Already saved in Rdata
load(file="winter21_22.Rdata")
load(file="winter22_23.Rdata")
load(file="winter23_24.Rdata")

##If want to do individual model
F139<-winter22_23[['SYSUL139']]
#F139<-as.data.frame(F139)
#Interpolation - two different methods
##Move package
ip139 <- interpolateTime(F139, time=as.difftime(20, units="mins"), spaceMethod='greatcircle')
##Convert move object to dataframe
dat139<-as.data.frame(ip139)
#convert WGS84 EPSG4326 to EPSG32649 unit in meter
LatLong.proj <- "EPSG:4326"
UtmZone.proj <- "EPSG:32649"
birdsproj <- dat139 %>% 
  st_as_sf(coords = c('location_long.1', 'location_lat.1'), 
           crs = LatLong.proj) %>% 
  st_transform(UtmZone.proj)
proj139 <- birdsproj %>%
  as_tibble() %>%
  mutate(X = st_coordinates(birdsproj)[ ,1],
         Y = st_coordinates(birdsproj)[ ,2]) %>%
  dplyr::select(-geometry)
##Change time zone
proj139$timestamps<-with_tz(dat139$timestamps,"Asia/Shanghai")

##Clean the dataset
data139<-proj139 %>%
  dplyr::select(timestamps,
                ID=local_identifier,
                utmX=X,
                utmY=Y)
data139$long<-dat139$location_long.1
data139$lat<-dat139$location_lat.1
data139<-as.data.frame(data139)
#prep data for HMM
dat <- prepData(data139,type='LL',coordNames = c("long","lat"))
#Fit 3 states HMM models
set.seed(0805)
##Fit model for F139 
### initial step distribution natural scale parameters
stepPar0 <- c(0.78,2.41,10.68,0.78,2.41,10.68)
#guess based on Behavioural mapping of a pelagic seabird: combining multiple sensors and a hidden Markov model reveals the distribution of at-sea behaviour
### initial angle distribution natural scale parameters
#anglePar0 <- c(0,3,0,1,10,1)
anglePar0 <- c(0,pi,0,1,0.5,1)

##Null model
null_3states <- fitHMM(data = dat,
                       nbStates = 3,
                       dist = list(step = "gamma", angle = "vm"),
                       Par0 = list(step = stepPar0, angle = anglePar0),
                       formula = ~ 1,
                       stateNames = c('Rafting', 'Foraging', 'Travelling'),
                       estAngleMean = list(angle=TRUE)
)
null_3states
plot(null_3states)
stateprob<-stateProbs(null_3states)
states<-viterbi(null_3states)
f139states<-cbind(data139,states,stateprob)
write.csv(f139states,file = "f139states.csv")
data<-read.csv(file = "f139states.csv")
data$timestamps<-as.POSIXct(data$timestamps)
data<-data %>%
  mutate(Foraging = ifelse(states == 2, 1, Foraging))%>%
  mutate(Foraging = ifelse(states == 1 | states== 3, 0, Foraging))%>%
  mutate(Rafting = ifelse(states == 1, 1, Rafting))%>%
  mutate(Rafting = ifelse(states == 2 | states== 3, 0, Rafting))%>%
  mutate(Travelling = ifelse(states == 3, 1, Travelling))%>%
  mutate(Travelling = ifelse(states == 1 | states== 2, 0, Travelling))
data$Foraging<-as.factor(data$Foraging)
data$Rafting<-as.factor(data$Rafting)
data$Travelling<-as.factor(data$Travelling)

## environmental data annotation done in GEE
#Extract environmental data
##Extract monthly ALAN data for f139
sw01 <- filter(data, timestamps >= ymd_hms("2023-01-01 00:00:01") &timestamps <=ymd_hms ("2023-02-01 00:00:00"))
points<-sw01[5:6]
m01<-rast('ALAN_month/202301.tif')
A1<-raster::extract(m01,points,ID=FALSE)

sw02 <- filter(data, timestamps >= ymd_hms("2023-02-01 00:00:01") &timestamps <=ymd_hms ("2023-03-01 00:00:00"))
points<-sw02[5:6]
m02<-rast('ALAN_month/202302.tif')
A2<-raster::extract(m02,points,ID=FALSE)

sw03 <- filter(data, timestamps >= ymd_hms("2023-03-01 00:00:01") &timestamps <=ymd_hms ("2023-04-01 00:00:00"))
points<-sw03[5:6]
m03<-rast('ALAN_month/202303.tif')
A3<-raster::extract(m03,points,ID=FALSE)

sw11 <- filter(data, timestamps >= ymd_hms("2022-11-01 00:00:00") &timestamps <=ymd_hms ("2022-12-01 00:00:00"))
points<-sw11[5:6]
m11<-rast('ALAN_month/202211.tif')
A11<-raster::extract(m11,points,ID=FALSE)

sw12 <- filter(data, timestamps >= ymd_hms("2022-12-01 00:00:01") &timestamps <=ymd_hms ("2023-01-01 00:00:00"))
points<-sw12[5:6]
m12<-rast('ALAN_month/202212.tif')
A12<-raster::extract(m12,points,ID=FALSE)

ALANm<-rbind(A11,A12,A1,A2,A3)
ALANm$avg_rad<-ALANm$avg_rad*1000000000
data<-data%>%
  arrange(timestamps)
data<-data%>%
  mutate(ALANm)

#colnames(nf139)[7]<-"class"
#colnames(nf139)[8]<-"ALAN"
#nf139<-nf139%>%
#  select(-ID)
#colnames(nf139)[5]<-"ID"

##Add day/night
library(suncalc)
dat<-data%>%
  dplyr::select(date=timestamps,
                lat,
                lon=long)
##Get moon illumination
moon<-getMoonIllumination(dat$date, keep = c("fraction", "phase"))
dat$date <- as.Date(dat$date)
sun <- getSunlightTimes(data = dat, tz = "Asia/Shanghai", 
                        keep = c("sunrise", "sunset", "night", "nightEnd"))
xy<-cbind(dat,sun)
#xy <- merge(x = dat, 
#            y = sun[, c("lat", "sunrise", "sunset", "night", "nightEnd")], 
#            by = "lat")

xy$period <- rep(" ", length.out = nrow(xy))
xy$period[xy$datetime > xy$sunrise & xy$datetime < xy$sunset] <- "day"
xy$period[xy$datetime > xy$sunset & xy$datetime < xy$night] <- "dusk"
xy$period[xy$datetime > xy$nightEnd & xy$datetime < xy$sunrise] <- "dawn"
xy$period[xy$period == " "] <- "night"

datafinal <- cbind(data,xy$period)
datafinal<- cbind(datafinal,moon$fraction)
datafinal<- cbind(datafinal,moon$phase)
colnames(datafinal)[12]<-"period"
colnames(datafinal)[13]<-"fraction"
colnames(datafinal)[14]<-"phase"
write.csv(datafinal,file = "f139period.csv")


##Apply linear model
night<-filter(datafinal,period=="night")
night<-filter(night,avg_rad>0.2)
#scatterplot
ggplot(night,aes(x=states,y=log(avg_rad)))+geom_jitter()

#need to include moon light and chollA

#model for rafting
n <- glm(Rafting ~1, night,family = "binomial")
m1 <- glm(Rafting ~log(avg_rad), night,family = "binomial")
m2 <- glm(Rafting ~phase, night,family = "binomial")
m3 <- glm(Rafting ~log(avg_rad)+phase, night,family = "binomial")

#Model compare
library(AICcmodavg)
ls<-list(n,m1,m2,m3)
model.names <- c('n', 'm1', 'm2', 'm3')
aictab(cand.set = ls, modnames = model.names)
summary(n)
summary(m1)
summary(m2)
summary(m3)
##
plot_model(m1,type="pred")
plot_model(a2,type="pred")
plot_model(b3,type="pred")
plot_models(m1,a2,b3)
tab_model(m1,a2,b3,show.reflvl = TRUE, show.intercept = FALSE,p.style = "numeric_stars")
#model for foraging
n <- glm(Foraging ~1, night,family = "binomial")
a1 <- glm(Foraging ~log(avg_rad), night,family = "binomial")
a2 <- glm(Foraging ~phase, night,family = "binomial")
a3 <- glm(Foraging ~log(avg_rad)+phase, night,family = "binomial")

#Model compare
library(AICcmodavg)
ls<-list(n,a1,a2,a3)
model.names <- c('n', 'a1', 'a2', 'a3')
aictab(cand.set = ls, modnames = model.names)
summary(n)
summary(a1)
summary(a2)
summary(a3)

#model for Travelling
n <- glm(Travelling ~1, night,family = "binomial")
b1 <- glm(Travelling ~log(avg_rad), night,family = "binomial")
b2 <- glm(Travelling ~phase, night,family = "binomial")
b3 <- glm(Travelling ~log(avg_rad)+phase, night,family = "binomial")

#Model compare
library(AICcmodavg)
ls<-list(n,b1,b2,b3)
model.names <- c('n', 'b1', 'b2', 'b3')
aictab(cand.set = ls, modnames = model.names)
summary(n)
summary(b1)
summary(b2)
summary(b3)

############################
data<-read.csv(file="f139period.csv")
night$states<-as.factor(night$states)
night<-filter(data,period=="night")
night%>%
  ggplot(aes(x=as.character(states), y=log(avg_rad))) +
  geom_boxplot(position=position_dodge(0.75),width=0.1, color="grey", alpha=0.2) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("ALAN effect vs states") +
  xlab("")

##momentuHMM package crawlwrap track simulation
#convert WGS84 EPSG4326 to EPSG32649 unit in meter
LatLong.proj <- "EPSG:4326"
UtmZone.proj <- "EPSG:32649"
birdsproj <- F139 %>% 
  st_as_sf(coords = c('location_long', 'location_lat'), 
           crs = LatLong.proj) %>% 
  st_transform(UtmZone.proj)
dat139 <- birdsproj %>%
  as_tibble() %>%
  mutate(X = st_coordinates(birdsproj)[ ,1],
         Y = st_coordinates(birdsproj)[ ,2]) %>%
  dplyr::select(-geometry)
##Change time zone
dat139$timestamps<-with_tz(dat139$timestamps,"Asia/Shanghai")
#select ID, time, location coordinates for crawlwrap simulation
crwdat<-dat139%>%
  dplyr::select(ID=local_identifier,
                timestamps,
                lat=location_lat.1,
                long=location_long.1,
                utmX=X,
                utmY=Y)
crwOut<-crawlWrap(crwdat,timeStep = "20 mins", Time.name = "timestamps",attempts = 10,coord = c("utmX","utmY"),proj = "EPSG:32649",ncores = 3)
dat <- prepData(crwOut)
crw139<-dat%>%
  dplyr::select(x,
                y)
birdsproj <- crw139 %>% 
  st_as_sf(coords = c('x', 'y'), 
           crs = UtmZone.proj) %>% 
  st_transform(LatLong.proj)
crw139 <- birdsproj %>%
  as_tibble() %>%
  mutate(X = st_coordinates(birdsproj)[ ,1],
         Y = st_coordinates(birdsproj)[ ,2]) %>%
  dplyr::select(-geometry)
crw139$lat<-dat$lat
crw139$long<-dat$long
write.csv(crw139,file="crw139.csv")
#prep data for HMM
dat <- prepData(crwOut)
#Fit 3 states HMM models
set.seed(0805)

##Fit model for F139 
### initial step distribution natural scale parameters
stepPar0 <- c(0.78,2.41,10.68,0.78,2.41,10.68)
#guess based on Behavioural mapping of a pelagic seabird: combining multiple sensors and a hidden Markov model reveals the distribution of at-sea behaviour
### initial angle distribution natural scale parameters
anglePar0 <- c(0,3,0,1,10,1)

##Null model
null_3states <- fitHMM(data = dat,
                       nbStates = 3,
                       dist = list(step = "gamma", angle = "vm"),
                       Par0 = list(step = stepPar0, angle = anglePar0),
                       formula = ~ 1,
                       stateNames = c('Rafting', 'Foraging', 'Travelling'),
                       estAngleMean = list(angle=TRUE)
)
null_3states
plot(null_3states)
states<-viterbi(null_3states_f139)
f139states<-cbind(data,states)
write.csv(data,file = "f142states.csv")
