#Clear environment and set working directory
rm(list=ls())
setwd("C:/ALAN_StreakedShearwater")
#Load packages
library(move)
library(raster)
library(sf)
library(terra)
library(dplyr)
#library(momentuHMM)
library(tidyverse)
library(lubridate)
library(suncalc)
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
#load(file="winter21_22.Rdata")
load(file="winter22_23.Rdata")
load(file="winter23_24.Rdata")

##If want to do individual model
#rawdat<-winter22_23[['SYSUL138']]
#F139<-as.data.frame(F139)

#Interpolation for 2022-2023 non-breeding period
## Set up a list of raw data
swall<-list()
swall[['SYSUL128']]<-winter22_23[['SYSUL128']]
swall[['SYSUL132']]<-winter22_23[['SYSUL132']]
swall[['SYSUL135']]<-winter22_23[['SYSUL135']]
swall[['SYSUL137']]<-winter22_23[['SYSUL137']]
swall[['SYSUL138']]<-winter22_23[['SYSUL138']]
swall[['SYSUL139']]<-winter22_23[['SYSUL139']]
swall[['SYSUL140']]<-winter22_23[['SYSUL140']]

#set up a name vector
names<-c("SYSUL128","SYSUL132","SYSUL135","SYSUL137","SYSUL138","SYSUL139","SYSUL140")
# Specify output lists of length formula
sw.list <- vector(mode = "list")
#Create a for loop for a batched interpolation for 22-23 and 23-24 individuals and clean the datasets
for (i in names) {
  print(i)
  sw.list[[i]] <- interpolateTime(swall[[i]], time=as.difftime(20, units="mins"), spaceMethod='greatcircle')
  sw.list[[i]] <- as.data.frame(sw.list[[i]])
  sw.list[[i]]$timestamps<-with_tz(sw.list[[i]]$timestamps,"Asia/Shanghai")
  sw.list[[i]]<-sw.list[[i]] %>%
    dplyr::select(timestamps,
                  ID=local_identifier,
                  long=location_long.1,
                  lat=location_lat.1)%>%
    mutate(sex=ifelse(ID=="SYSUL137"|ID=="SYSUL139","F","M"))%>%
    mutate(NBYear=rep("NB2223"))
  #This can be used to output each individual data separately, not in use now
  #sw<-sw.list[[i]]
  #file.out <- paste0("./RawData/", paste0(i, ".csv"))
  #write.csv(sw, file = file.out)
}

#Combine individuals into one big dataframe
sw_NB_22to23 <- do.call("rbind", sw.list)
write.csv(sw_NB_22to23,file="RawData/sw_NB_22to23.csv")
save(sw_NB_22to23,file="RawData/sw_NB_22to23.Rdata")

#Interpolation for 2023-2024 non-breeding period
## Set up a list of raw data
swall[['SYSUL131']]<-winter23_24[['SYSUL131']]
swall[['SYSUL134']]<-winter23_24[['SYSUL134']]
swall[['SYSUL141']]<-winter23_24[['SYSUL141']]
swall[['SYSUL127']]<-winter23_24[['SYSUL127']]
swall[['SYSUL136']]<-winter23_24[['SYSUL136']]
swall[['SYSUL135']]<-winter23_24[['SYSUL135']]
swall[['SYSUL140']]<-winter23_24[['SYSUL140']]

#set up a name vector
names<-c("SYSUL131","SYSUL134","SYSUL141","SYSUL127","SYSUL136","SYSUL135","SYSUL140")
# Specify output lists of length formula
sw.list <- vector(mode = "list")
#Create a for loop for a batched interpolation for 22-23 and 23-24 individuals and clean the datasets
for (i in names) {
  print(i)
  sw.list[[i]] <- interpolateTime(swall[[i]], time=as.difftime(20, units="mins"), spaceMethod='greatcircle')
  sw.list[[i]] <- as.data.frame(sw.list[[i]])
  sw.list[[i]]$timestamps<-with_tz(sw.list[[i]]$timestamps,"Asia/Shanghai")
  sw.list[[i]]<-sw.list[[i]] %>%
    dplyr::select(timestamps,
                  ID=local_identifier,
                  long=location_long.1,
                  lat=location_lat.1)%>%
    mutate(sex=ifelse(ID=="SYSUL131"|ID=="SYSUL134"|ID=="SYSUL136","F","M"))%>%
    mutate(NBYear=rep("NB2324"))
  #This can be used to output each individual data separately, not in use now
  #sw<-sw.list[[i]]
  #file.out <- paste0("./RawData/", paste0(i, ".csv"))
  #write.csv(sw, file = file.out)
}

#Combine individuals into one big dataframe
sw_NB_23to24 <- do.call("rbind", sw.list)
write.csv(sw_NB_23to24,file="RawData/sw_NB_23to24.csv")
save(sw_NB_23to24,file="RawData/sw_NB_23to24.Rdata")

#Combine 2022 to 2024 into one dataframe
sw_NB_22to24<-rbind(sw_NB_22to23,sw_NB_23to24)
write.csv(sw_NB_22to24,file="RawData/sw_NB_22to24.csv")
save(sw_NB_22to24,file="RawData/sw_NB_22to24.Rdata")
#subset 22-23 data


##########################################
#Extract environmental data
##offshore infrastructure (oil and windfarm) within 5km buffer of gps location extracted use ArcGIS pro, infrastructures are extracted separately for year 22-23 and 23-24
#geoprocessing tools: select by attribute to filter out only 22-23 and 23-24 data->XY table to points->pairwise buffer->summarize within->export table as csv
#now combine the infrastructure count with rawdata and make it a binary categorical variable
load(file="RawData/sw_NB_22to23.Rdata")
load(file="RawData/sw_NB_23to24.Rdata")
inf2223<-read.csv(file="RawData/sw_NB_22to23_oilwind.csv")
inf2324<-read.csv(file="RawData/sw_NB_23to24_oilwind.csv")

sw_NB_22to23_infra<-cbind(sw_NB_22to23,inf2223$Oil,inf2223$Wind)
colnames(sw_NB_22to23_infra)[7]<-"Oil"
colnames(sw_NB_22to23_infra)[8]<-"Wind"
sw_NB_22to23_infra$Oil<-ifelse(sw_NB_22to23_infra$Oil==0,"No","Yes")
sw_NB_22to23_infra$Wind<-ifelse(sw_NB_22to23_infra$Wind==0,"No","Yes")
write.csv(sw_NB_22to23_infra,file="RawData/sw_NB_22to23_infra.csv")
save(sw_NB_22to23_infra,file="RawData/sw_NB_22to23_infra.Rdata")

sw_NB_23to24_infra<-cbind(sw_NB_23to24,inf2324$Oil,inf2324$Wind)
colnames(sw_NB_23to24_infra)[7]<-"Oil"
colnames(sw_NB_23to24_infra)[8]<-"Wind"
sw_NB_23to24_infra$Oil<-ifelse(sw_NB_23to24_infra$Oil==0,"No","Yes")
sw_NB_23to24_infra$Wind<-ifelse(sw_NB_23to24_infra$Wind==0,"No","Yes")
write.csv(sw_NB_23to24_infra,file="RawData/sw_NB_23to24_infra.csv")
save(sw_NB_23to24_infra,file="RawData/sw_NB_23to24_infra.Rdata")

sw_NB_22to24_infra<-rbind(sw_NB_22to23_infra,sw_NB_23to24_infra)
write.csv(sw_NB_22to24_infra,file="RawData/sw_NB_22to24_infra.csv")
save(sw_NB_22to24_infra,file="RawData/sw_NB_22to24_infra.Rdata")

##Extract 22-23 monthly ALAN data
data<-sw_NB_22to23_infra
sw01 <- filter(data, timestamps >= ymd_hms("2023-01-01 00:00:01") &timestamps <=ymd_hms ("2023-02-01 00:00:00"))
points<-sw01[3:4]
m01<-rast('ALAN_month/202301.tif')
A1<-raster::extract(m01,points,method='bilinear',ID=FALSE)
R1<-raster::extract(m01,points,buffer=5000,fun=mean,ID=FALSE)

sw02 <- filter(data, timestamps >= ymd_hms("2023-02-01 00:00:01") &timestamps <=ymd_hms ("2023-03-01 00:00:00"))
points<-sw02[3:4]
m02<-rast('ALAN_month/202302.tif')
A2<-raster::extract(m02,points,method='bilinear',ID=FALSE)
R2<-raster::extract(m02,points,buffer=5000,fun=mean,ID=FALSE)

sw03 <- filter(data, timestamps >= ymd_hms("2023-03-01 00:00:01") &timestamps <=ymd_hms ("2023-04-01 00:00:00"))
points<-sw03[3:4]
m03<-rast('ALAN_month/202303.tif')
A3<-raster::extract(m03,points,method='bilinear',ID=FALSE)
R3<-raster::extract(m03,points,buffer=5000,fun=mean,ID=FALSE)

sw11 <- filter(data, timestamps >= ymd_hms("2022-11-01 00:00:00") &timestamps <=ymd_hms ("2022-12-01 00:00:00"))
points<-sw11[3:4]
m11<-rast('ALAN_month/202211.tif')
A11<-raster::extract(m11,points,method='bilinear',ID=FALSE)
R11<-raster::extract(m11,points,buffer=5000,fun=mean,ID=FALSE)

sw12 <- filter(data, timestamps >= ymd_hms("2022-12-01 00:00:01") &timestamps <=ymd_hms ("2023-01-01 00:00:00"))
points<-sw12[3:4]
m12<-rast('ALAN_month/202212.tif')
A12<-raster::extract(m12,points,method='bilinear',ID=FALSE)
R12<-raster::extract(m12,points,buffer=5000,fun=mean,ID=FALSE)

ALANm<-rbind(A11,A12,A1,A2,A3)
ALAN5<-rbind(R11,R12,R1,R2,R3)
colnames(ALAN5)[1]<-"avg_rad_5"
data2223<-data%>%
  arrange(timestamps)%>%
  mutate(ALANm,
         ALAN5)

##Extract 23-24 monthly ALAN data
data<-sw_NB_23to24_infra

sw01 <- filter(data, timestamps >= ymd_hms("2024-01-01 00:00:01") &timestamps <=ymd_hms ("2024-02-01 00:00:00"))
points<-sw01[3:4]
m01<-rast('ALAN_month/202401.tif')
A1<-raster::extract(m01,points,method='bilinear',ID=FALSE)
R1<-raster::extract(m01,points,buffer=5000,fun=mean,ID=FALSE)

sw02 <- filter(data, timestamps >= ymd_hms("2024-02-01 00:00:01") &timestamps <=ymd_hms ("2024-03-01 00:00:00"))
points<-sw02[3:4]
m02<-rast('ALAN_month/202402.tif')
A2<-raster::extract(m02,points,method='bilinear',ID=FALSE)
R2<-raster::extract(m02,points,buffer=5000,fun=mean,ID=FALSE)

sw03 <- filter(data, timestamps >= ymd_hms("2024-03-01 00:00:01") &timestamps <=ymd_hms ("2024-04-01 00:00:00"))
points<-sw03[3:4]
m03<-rast('ALAN_month/202403.tif')
A3<-raster::extract(m03,points,method='bilinear',ID=FALSE)
R3<-raster::extract(m03,points,buffer=5000,fun=mean,ID=FALSE)

sw11 <- filter(data, timestamps >= ymd_hms("2023-11-01 00:00:00") &timestamps <=ymd_hms ("2023-12-01 00:00:00"))
points<-sw11[3:4]
m11<-rast('ALAN_month/202311.tif')
A11<-raster::extract(m11,points,method='bilinear',ID=FALSE)
R11<-raster::extract(m11,points,buffer=5000,fun=mean,ID=FALSE)

sw12 <- filter(data, timestamps >= ymd_hms("2023-12-01 00:00:01") &timestamps <=ymd_hms ("2024-01-01 00:00:00"))
points<-sw12[3:4]
m12<-rast('ALAN_month/202312.tif')
A12<-raster::extract(m12,points,method='bilinear',ID=FALSE)
R12<-raster::extract(m12,points,buffer=5000,fun=mean,ID=FALSE)

ALANm<-rbind(A11,A12,A1,A2,A3)
ALAN5<-rbind(R11,R12,R1,R2,R3)
colnames(ALAN5)[1]<-"avg_rad_5"
data2324<-data%>%
  arrange(timestamps)%>%
  mutate(ALANm,
         ALAN5)

data<-rbind(data2223,data2324)

##Add day/night
dat<-data%>%
  dplyr::select(date=timestamps,
                datetime=timestamps,
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
xy$period[xy$datetime > xy$sunset & xy$datetime < xy$night] <- "night"
xy$period[xy$datetime > xy$nightEnd & xy$datetime < xy$sunrise] <- "night"
xy$period[xy$period == " "] <- "night"

datafinal <- cbind(data,xy$period)
datafinal<- cbind(datafinal,moon$fraction)
datafinal<- cbind(datafinal,moon$phase)
colnames(datafinal)[11]<-"period"
colnames(datafinal)[12]<-"fraction"
colnames(datafinal)[13]<-"phase"
datafinal$avg_rad <- ifelse(datafinal$avg_rad < 0.00001, 0.00001,datafinal$avg_rad)
datafinal$rad_log <- log(datafinal$avg_rad)
datafinal$avg_rad_5 <- ifelse(datafinal$avg_rad_5 < 0.00001, 0.00001,datafinal$avg_rad_5)
datafinal$rad_log_5 <- log(datafinal$avg_rad_5)
#add time of day
hours <- as.numeric(format(datafinal$timestamp, "%H"))
minutes <- as.numeric(format(datafinal$timestamp, "%M"))
datafinal$tod <- hours + minutes / 60

#add distance to seashore
points<-datafinal[3:4]
dis<-rast('dist_seashore/distance.tif')
dis_sea<-raster::extract(dis,points,method='bilinear',ID=FALSE)
datafinal$dist<-dis_sea$distance

#add UTM projection
#convert WGS84 EPSG4326 to EPSG32649 unit in meter
LatLong.proj <- "EPSG:4326"
UtmZone.proj <- "EPSG:32649"
UTMproj <- datafinal %>% 
  st_as_sf(coords = c('long','lat'), 
           crs = LatLong.proj) %>% 
  st_transform(UtmZone.proj)

sw_NB_22to24_envall <- UTMproj %>%
  as_tibble() %>%
  mutate(UTMx = st_coordinates(UTMproj)[ ,1],
         UTMy = st_coordinates(UTMproj)[ ,2],
         long=datafinal$long,
         lat=datafinal$lat) %>%
  dplyr::select(-geometry)

#save the final dataset with all values
sw_NB_22to24_envall<-sw_NB_22to24_envall%>%
  arrange(ID,
          timestamps)
write.csv(sw_NB_22to24_envall,file="RawData/sw_NB_22to24_envall.csv")
save(sw_NB_22to24_envall,file="RawData/sw_NB_22to24_envall.Rdata")



#################################################################
#Fit 3 states HMM models
set.seed(0805)
#prep data for HMM
Fe<-subset(sw_NB_22to24_envall,sex=="F")
Fe<-as.data.frame(Fe)
dat <- prepData(Fe,type='UTM',coordNames = c("UTMx","UTMy"))
# Remove errorneous step lengths & only select night
dat <- subset(dat, step < 90000)
dat <- subset(dat,period=="night")
hist(dat$step)
##Fit model for F139 
### initial step distribution natural scale parameters
#stepPar0 <- c(0.78,2.41,10.68,0.78,2.41,10.68) This is in km for long/lat prepdata
stepPar0 <- c(780,2410,10680,780,2410,10680)
#guess based on Behavioural mapping of a pelagic seabird: combining multiple sensors and a hidden Markov model reveals the distribution of at-sea behaviour
### initial angle distribution natural scale parameters
#anglePar0 <- c(0,3,0,1,10,1)
anglePar0 <- c(0,pi,0,1,0.5,1)
##Null model
Fe$Oil<-as.factor(Fe$Oil)
null_3states <- fitHMM(data = dat,
                       nbStates = 3,
                       dist = list(step = "gamma", angle = "vm"),
                       Par0 = list(step = stepPar0, angle = anglePar0),
                       formula = ~ avg_rad+Oil,
                       stateNames = c('Rafting', 'Foraging', 'Travelling'),
                       estAngleMean = list(angle=TRUE)
)
null_3states
plot(null_3states)

##tod model
Par0 <- getPar0(null_3states,formula= ~ sin(3.141593*2*tod/24) + cos(3.141593*2*tod/24))
tod_3states <- fitHMM(data = dat,
                       nbStates = 3,
                       dist = list(step = "gamma", angle = "vm"),
                       Par0 = list(step=Par0$Par$step, angle=Par0$Par$angle,delta0 = Par0$delta),
                       beta0 = Par0$beta,
                       formula = ~ sin(3.141593*2*tod/24) + cos(3.141593*2*tod/24),
                       stateNames = c('Rafting', 'Foraging', 'Travelling'),
                       estAngleMean = list(angle=TRUE)
                       
)
tod_3states
plot(tod_3states,plotCI=TRUE,plotStationary=TRUE)

##ALAN model
Par0 <- getPar0(null_3states,formula=~rad_log+Oil)
alan_3states <- fitHMM(data = dat,
                       nbStates = 3,
                       dist = list(step = "gamma", angle = "vm"),
                       Par0 = list(step=Par0$Par$step, angle=Par0$Par$angle,delta0 = Par0$delta),
                       beta0 = Par0$beta,
                       formula = ~ rad_log+Oil,
                       stateNames = c('Rafting', 'Foraging', 'Travelling'),
                       estAngleMean = list(angle=TRUE)
                       
)
alan_3states
plot(alan_3states,plotCI=TRUE,plotStationary=TRUE)

#no log
Par0 <- getPar0(null_3states,formula=~avg_rad)
nolog_3states <- fitHMM(data = dat,
                       nbStates = 3,
                       dist = list(step = "gamma", angle = "vm"),
                       Par0 = list(step=Par0$Par$step, angle=Par0$Par$angle,delta0 = Par0$delta),
                       beta0 = Par0$beta,
                       formula = ~ avg_rad,
                       stateNames = c('Rafting', 'Foraging', 'Travelling'),
                       estAngleMean = list(angle=TRUE)
                       
)
nolog_3states
plot(nolog_3states,plotStationary=TRUE)
#stateprob<-stateProbs(alan_3states)
states<-viterbi(alan_3states)
f139states<-cbind(dat,states)

##moon fraction model
Par0 <- getPar0(null_3states,formula=~fraction)
f_3states <- fitHMM(data = dat,
                    nbStates = 3,
                    dist = list(step = "gamma", angle = "vm"),
                    Par0 = list(step=Par0$Par$step, angle=Par0$Par$angle,delta0 = Par0$delta),
                    beta0 = Par0$beta,
                    formula = ~ fraction,
                    stateNames = c('Rafting', 'Foraging', 'Travelling'),
                    estAngleMean = list(angle=TRUE)
                    
)
f_3states
plot(f_3states,plotCI=TRUE,plotStationary=TRUE)

##ALAN + moon fraction model
Par0 <- getPar0(null_3states,formula=~rad_log+fraction)
af_3states <- fitHMM(data = dat,
                     nbStates = 3,
                     dist = list(step = "gamma", angle = "vm"),
                     Par0 = list(step=Par0$Par$step, angle=Par0$Par$angle,delta0 = Par0$delta),
                     beta0 = Par0$beta,
                     formula = ~ rad_log+fraction,
                     stateNames = c('Rafting', 'Foraging', 'Travelling'),
                     estAngleMean = list(angle=TRUE)
                     
)
af_3states
plot(af_3states,plotCI=TRUE,plotStationary=TRUE)

##ALAN * moon fraction model
Par0 <- getPar0(null_3states,formula=~rad_log:fraction)
atf_3states <- fitHMM(data = dat,
                      nbStates = 3,
                      dist = list(step = "gamma", angle = "vm"),
                      Par0 = list(step=Par0$Par$step, angle=Par0$Par$angle,delta0 = Par0$delta),
                      beta0 = Par0$beta,
                      formula = ~ rad_log:fraction,
                      stateNames = c('Rafting', 'Foraging', 'Travelling'),
                      estAngleMean = list(angle=TRUE)
                      
)
atf_3states
plot(atf_3states,plotCI=TRUE,plotStationary=TRUE)

##moon phase model
Par0 <- getPar0(null_3states,formula=~phase)
p_3states <- fitHMM(data = dat,
                    nbStates = 3,
                    dist = list(step = "gamma", angle = "vm"),
                    Par0 = list(step=Par0$Par$step, angle=Par0$Par$angle,delta0 = Par0$delta),
                    beta0 = Par0$beta,
                    formula = ~ phase,
                    stateNames = c('Rafting', 'Foraging', 'Travelling'),
                    estAngleMean = list(angle=TRUE)
                    
)
p_3states
plot(p_3states,plotCI=TRUE,plotStationary=TRUE)

##ALAN + moon phase model
Par0 <- getPar0(null_3states,formula=~rad_log+phase)
ap_3states <- fitHMM(data = dat,
                     nbStates = 3,
                     dist = list(step = "gamma", angle = "vm"),
                     Par0 = list(step=Par0$Par$step, angle=Par0$Par$angle,delta0 = Par0$delta),
                     beta0 = Par0$beta,
                     formula = ~ rad_log+phase,
                     stateNames = c('Rafting', 'Foraging', 'Travelling'),
                     estAngleMean = list(angle=TRUE)
                     
)
ap_3states
plot(ap_3states,plotCI=TRUE,plotStationary=TRUE)




#####################################################################
#add number of offshore infrastructure within 1000m radius
oil<-read.csv(file="Offshore_infrastructure/Oil.csv")
windfarm<-read.csv(file="Offshore_infrastructure/windfarm.csv")
infra_all<-read.csv(file="Offshore_infrastructure/infrastructure_clip.csv")
oil_point <- st_as_sf(oil, coords = c("lon", "lat"), crs = 4326)
wind_point <- st_as_sf(windfarm, coords = c("lon", "lat"), crs = 4326)
infra_point <- st_as_sf(infra_all, coords = c("lon", "lat"), crs = 4326)
center_point <- st_as_sf(F139, coords = c("long", "lat"), crs = 4326)
radius <- 1000  # Radius in meters
buffer <- st_buffer(center_point, dist = radius)
oilpoints_within_radius <- st_intersects(oil_point, buffer, sparse = FALSE)
Oilcount <- sum(oilpoints_within_radius)
windpoints_within_radius <- st_intersects(wind_point, buffer, sparse = FALSE)
Windfarmcount <- sum(windpoints_within_radius)
infrapoints_within_radius <- st_intersects(infra_point, buffer, sparse = FALSE)
Infracount <- sum(infrapoints_within_radius)
