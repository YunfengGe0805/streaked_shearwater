# Remove items from memory/clean your workspace
rm(list=ls())
setwd("C:/ALAN_StreakedShearwater")

#Library packages
library(terra)
library(sf)
library(tidyverse)
library(move)
#Load dataset
cred <- movebankLogin(username="Yachang Cheng", password="welove107")
searchMovebankStudies(x="Qingdao", login=cred)
# case sensitive
birds <- getMovebankData(study="Tracking seabirds in the Yellow Sea of Qingdao", login=cred,timestamp_start="20230101000000000",timestamp_end="20231231235959000")

#birddf<-as.data.frame(birds)

#Check sampling interval
load(file="birds.Rdata")
timelag<-timeLag(birds,units="mins")
save(timelag,file="timelag.Rdata")
load(file="timelag.Rdata")
summary(timelag$SYSUL140)
summary(timelag$SYSUL128)
summary(timelag$SYSUL137)
summary(timelag$SYSUL132)
birds$timestamp<-with_tz(birds$timestamp,"Asia/Shanghai")
save(birds,file="birds.Rdata")
#birds2023 <- filter(birds, timestamp >= ymd_hms("2023-01-01 00:00:00") &timestamp <=ymd_hms ("2024-01-01 00:00:00"))



#Change timezone and select track data in 2023
load(file="birddf.Rdata")
birddf$timestamp<-with_tz(birddf$timestamp,"Asia/Shanghai")
sw2023 <- filter(birddf, timestamp >= ymd_hms("2023-01-01 00:00:00") &timestamp <=ymd_hms ("2024-01-01 00:00:00"))
#save(sw2023,file="sw2023.Rdata")

#Extract 2023 ALAN and ChollA data using simple point method
points<-sw2023[24:25]
ALAN2023<-rast('ALAN2023.tif')
ChollA2023<-rast('ChollA2023.tif')
ALANdata<-terra::extract(ALAN2023,points,method="simple",xy=TRUE,ID=FALSE)
ChollAdata<-terra::extract(ChollA2023,points,method="simple",ID=FALSE)
#Combine into one dataset
rsextract_simple<-ALANdata%>%
  mutate(ChollAdata,
         long=x,
         lat=y)
save(rsextract_simple,file="rsextract_simple.Rdata")
load(file="rsextract_simple.Rdata")


#Add environmental data to track data 2023 only
load(file="sw2023.Rdata")
load(file="rsextract_simple.Rdata")
sw2023_rs<-sw2023%>%
  select(
    x=location_lat,
    y=location_long,
    lat=location_lat,
    long=location_long,
    t = timestamp,
    temp=external_temperature,
    speed=ground_speed,
    ID=trackId)%>%
  mutate(ALAN=rsextract_simple$avg_rad,
         ChollA=rsextract_simple$CHLA_AVE)
#save(sw2023_rs,file="sw2023_rs.Rdata")
load(file="sw2023_rs.Rdata")
#convert WGS84 EPSG4326 to EPSG32649 unit in meter
LatLong.proj <- "EPSG:4326"
UtmZone.proj <- "EPSG:32649"
SW.sf <- sw2023_rs %>% 
  st_as_sf(coords = c('y', 'x'), 
           crs = LatLong.proj) %>% 
  st_transform(UtmZone.proj)
swUTM <- SW.sf %>%
  as_tibble() %>%
  mutate(X = st_coordinates(SW.sf)[ ,1],
         Y = st_coordinates(SW.sf)[ ,2]) %>%
  dplyr::select(-geometry)
#save(swUTM,file="swUTM.Rdata")


#assign NA to ALAN data at daytime
load(file="swUTM.Rdata")
swdata<-as.data.frame(swUTM)
# Add year-day (yday), and hour to dataset as potential covariates
swUTM$yday <- yday(swUTM$t)
swUTM$hr <- hour(swUTM$t)
plot(swUTM$yday, swUTM$ALAN)
#Filter out only nighttime track data
swNight<-filter(swUTM,hr==c(0,1,2,3,4,5,6,23,22,21,20))
plot(swNight$hr, swNight$ALAN)
#Filter out only daytime track data
swDay<-filter(swUTM,hr==c(7,8,9,10,11,12,13,14,15,16,17,18,19))
swDay$ALAN<-NA
swdat<-rbind(swNight,swDay)%>%
  arrange(t)
#save(swdat,file="swdat.Rdata")

#extract monthly 2023 ALAN data
sw01 <- filter(dat, timestamp >= ymd_hms("2023-01-01 00:00:00") &timestamp <=ymd_hms ("2023-02-01 00:00:00"))
points<-sw01[24:25]
m01<-rast('ALAN_month/202301.tif')
A1<-terra::extract(m01,points,method="simple",ID=FALSE)

sw02 <- filter(sw2023, timestamp >= ymd_hms("2023-02-01 00:00:01") &timestamp <=ymd_hms ("2023-03-01 00:00:00"))
points<-sw02[24:25]
m02<-rast('ALAN_month/202302.tif')
A2<-terra::extract(m02,points,method="simple",ID=FALSE)

sw03 <- filter(sw2023, timestamp >= ymd_hms("2023-03-01 00:00:01") &timestamp <=ymd_hms ("2023-04-01 00:00:00"))
points<-sw03[24:25]
m03<-rast('ALAN_month/202303.tif')
A3<-terra::extract(m03,points,method="simple",ID=FALSE)

sw04 <- filter(sw2023, timestamp >= ymd_hms("2023-04-01 00:00:01") &timestamp <=ymd_hms ("2023-05-01 00:00:00"))
points<-sw04[24:25]
m04<-rast('ALAN_month/202304.tif')
A4<-terra::extract(m04,points,method="simple",ID=FALSE)

sw05 <- filter(sw2023, timestamp >= ymd_hms("2023-05-01 00:00:01") &timestamp <=ymd_hms ("2023-06-01 00:00:00"))
points<-sw05[24:25]
m05<-rast('ALAN_month/202305.tif')
A5<-terra::extract(m05,points,method="simple",ID=FALSE)

sw06 <- filter(sw2023, timestamp >= ymd_hms("2023-06-01 00:00:01") &timestamp <=ymd_hms ("2023-07-01 00:00:00"))
points<-sw06[24:25]
m06<-rast('ALAN_month/202306.tif')
A6<-terra::extract(m06,points,method="simple",ID=FALSE)

sw07 <- filter(sw2023, timestamp >= ymd_hms("2023-07-01 00:00:01") &timestamp <=ymd_hms ("2023-08-01 00:00:00"))
points<-sw07[24:25]
m07<-rast('ALAN_month/202307.tif')
A7<-terra::extract(m07,points,method="simple",ID=FALSE)

sw08 <- filter(sw2023, timestamp >= ymd_hms("2023-08-01 00:00:01") &timestamp <=ymd_hms ("2023-09-01 00:00:00"))
points<-sw08[24:25]
m08<-rast('ALAN_month/202308.tif')
A8<-terra::extract(m08,points,method="simple",ID=FALSE)

sw09 <- filter(sw2023, timestamp >= ymd_hms("2023-09-01 00:00:01") &timestamp <=ymd_hms ("2023-10-01 00:00:00"))
points<-sw09[24:25]
m09<-rast('ALAN_month/202309.tif')
A9<-terra::extract(m09,points,method="simple",ID=FALSE)

sw10 <- filter(sw2023, timestamp >= ymd_hms("2023-10-01 00:00:01") &timestamp <=ymd_hms ("2023-11-01 00:00:00"))
points<-sw10[24:25]
m10<-rast('ALAN_month/202310.tif')
A10<-terra::extract(m10,points,method="simple",ID=FALSE)

sw11 <- filter(sw2023, timestamp >= ymd_hms("2023-11-01 00:00:01") &timestamp <=ymd_hms ("2023-12-01 00:00:00"))
points<-sw11[24:25]
m11<-rast('ALAN_month/202311.tif')
A11<-terra::extract(m11,points,method="simple",ID=FALSE)

sw12 <- filter(sw2023, timestamp >= ymd_hms("2023-12-01 00:00:01") &timestamp <=ymd_hms ("2024-01-01 00:00:00"))
points<-sw12[24:25]
m12<-rast('ALAN_month/202312.tif')
A12<-terra::extract(m12,points,method="simple",ID=FALSE)

ALANm<-rbind(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12)
save(ALANm,file="ALANm.Rdata")

#add monthly ALAN to track data
swall<-swUTM%>%
  mutate(ALANm)
colnames(swall)[11]<-"ALANm"

#assign NA to ALANm data at daytime
# Add year-day (yday), and hour to dataset as potential covariates
swall$yday <- yday(swall$t)
swall$hr <- hour(swall$t)
plot(swall$yday, swall$ALAN)
save(swall,file="swall.Rdata")
load(file="swall.Rdata")

#Filter out only nighttime track data
swNight<-filter(swall,hr==c(0,1,2,3,4,5,6,23,22,21,20))
plot(swNight$hr, swNight$ALAN)
#Filter out only daytime track data
swDay<-filter(swall,hr==c(7,8,9,10,11,12,13,14,15,16,17,18,19))
swDay$ALAN<-NA
swDay$ALANm<-NA
swdat<-rbind(swNight,swDay)%>%
  arrange(t)
save(swdat,file="swdat.Rdata")
