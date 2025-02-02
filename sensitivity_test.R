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
##########################################
#Extract environmental data
##offshore infrastructure (oil and windfarm) within 5km buffer of gps location extracted use ArcGIS pro, infrastructures are extracted separately for year 22-23 and 23-24
#geoprocessing tools: select by attribute to filter out only 22-23 and 23-24 data->XY table to points->pairwise buffer->summarize within->export table as csv
#now combine the infrastructure count with rawdata and make it a binary categorical variable
load(file="RawData/sw_NB_22to23.Rdata")
load(file="RawData/sw_NB_23to24.Rdata")

##Extract 22-23 monthly ALAN data
data<-sw_NB_22to23
sw01 <- filter(data, timestamps >= ymd_hms("2023-01-01 00:00:01") &timestamps <=ymd_hms ("2023-02-01 00:00:00"))
points<-sw01[3:4]
m01<-rast('ALAN_month/202301.tif')
A1<-raster::extract(m01,points,method='bilinear',ID=FALSE)
R1<-raster::extract(m01,points,buffer=5000,fun=mean,ID=FALSE)
S1<-raster::extract(m01,points,buffer=10000,fun=mean,ID=FALSE)
T1<-raster::extract(m01,points,buffer=15000,fun=mean,ID=FALSE)
U1<-raster::extract(m01,points,buffer=20000,fun=mean,ID=FALSE)
V1<-raster::extract(m01,points,buffer=20000,fun=mean,ID=FALSE)
W1<-raster::extract(m01,points,buffer=25000,fun=mean,ID=FALSE)
X1<-raster::extract(m01,points,buffer=30000,fun=mean,ID=FALSE)

sw02 <- filter(data, timestamps >= ymd_hms("2023-02-01 00:00:01") &timestamps <=ymd_hms ("2023-03-01 00:00:00"))
points<-sw02[3:4]
m02<-rast('ALAN_month/202302.tif')
A2<-raster::extract(m02,points,method='bilinear',ID=FALSE)
R2<-raster::extract(m02,points,buffer=5000,fun=mean,ID=FALSE)
S2<-raster::extract(m01,points,buffer=10000,fun=mean,ID=FALSE)
T2<-raster::extract(m01,points,buffer=15000,fun=mean,ID=FALSE)
U2<-raster::extract(m01,points,buffer=20000,fun=mean,ID=FALSE)
V2<-raster::extract(m01,points,buffer=20000,fun=mean,ID=FALSE)
W2<-raster::extract(m01,points,buffer=25000,fun=mean,ID=FALSE)
X2<-raster::extract(m01,points,buffer=30000,fun=mean,ID=FALSE)

sw03 <- filter(data, timestamps >= ymd_hms("2023-03-01 00:00:01") &timestamps <=ymd_hms ("2023-04-01 00:00:00"))
points<-sw03[3:4]
m03<-rast('ALAN_month/202303.tif')
A3<-raster::extract(m03,points,method='bilinear',ID=FALSE)
R3<-raster::extract(m03,points,buffer=5000,fun=mean,ID=FALSE)
S3<-raster::extract(m01,points,buffer=10000,fun=mean,ID=FALSE)
T3<-raster::extract(m01,points,buffer=15000,fun=mean,ID=FALSE)
U3<-raster::extract(m01,points,buffer=20000,fun=mean,ID=FALSE)
V3<-raster::extract(m01,points,buffer=20000,fun=mean,ID=FALSE)
W3<-raster::extract(m01,points,buffer=25000,fun=mean,ID=FALSE)
X3<-raster::extract(m01,points,buffer=30000,fun=mean,ID=FALSE)

sw11 <- filter(data, timestamps >= ymd_hms("2022-11-01 00:00:00") &timestamps <=ymd_hms ("2022-12-01 00:00:00"))
points<-sw11[3:4]
m11<-rast('ALAN_month/202211.tif')
A11<-raster::extract(m11,points,method='bilinear',ID=FALSE)
R11<-raster::extract(m11,points,buffer=5000,fun=mean,ID=FALSE)
S11<-raster::extract(m01,points,buffer=10000,fun=mean,ID=FALSE)
T11<-raster::extract(m01,points,buffer=15000,fun=mean,ID=FALSE)
U11<-raster::extract(m01,points,buffer=20000,fun=mean,ID=FALSE)
V11<-raster::extract(m01,points,buffer=20000,fun=mean,ID=FALSE)
W11<-raster::extract(m01,points,buffer=25000,fun=mean,ID=FALSE)
X11<-raster::extract(m01,points,buffer=30000,fun=mean,ID=FALSE)

sw12 <- filter(data, timestamps >= ymd_hms("2022-12-01 00:00:01") &timestamps <=ymd_hms ("2023-01-01 00:00:00"))
points<-sw12[3:4]
m12<-rast('ALAN_month/202212.tif')
A12<-raster::extract(m12,points,method='bilinear',ID=FALSE)
R12<-raster::extract(m12,points,buffer=5000,fun=mean,ID=FALSE)
S12<-raster::extract(m01,points,buffer=10000,fun=mean,ID=FALSE)
T12<-raster::extract(m01,points,buffer=15000,fun=mean,ID=FALSE)
U12<-raster::extract(m01,points,buffer=20000,fun=mean,ID=FALSE)
V12<-raster::extract(m01,points,buffer=20000,fun=mean,ID=FALSE)
W12<-raster::extract(m01,points,buffer=25000,fun=mean,ID=FALSE)
X12<-raster::extract(m01,points,buffer=30000,fun=mean,ID=FALSE)

ALANm<-rbind(A11,A12,A1,A2,A3)
ALAN5<-rbind(R11,R12,R1,R2,R3)
ALAN10<-rbind(R11,R12,R1,R2,R3)
ALAN15<-rbind(R11,R12,R1,R2,R3)
ALAN20<-rbind(R11,R12,R1,R2,R3)
ALAN25<-rbind(R11,R12,R1,R2,R3)
ALAN30<-rbind(R11,R12,R1,R2,R3)
colnames(ALAN5)[1]<-"avg_rad_5"
colnames(ALAN10)[1]<-"avg_rad_10"
colnames(ALAN15)[1]<-"avg_rad_15"
colnames(ALAN20)[1]<-"avg_rad_20"
colnames(ALAN25)[1]<-"avg_rad_25"
colnames(ALAN30)[1]<-"avg_rad_30"
data2223<-data%>%
  arrange(timestamps)%>%
  mutate(ALANm,
         ALAN5,
         ALAN10,
         ALAN15,
         ALAN20,
         ALAN25,
         ALAN30)

##Extract 23-24 monthly ALAN data
data<-sw_NB_23to24

sw01 <- filter(data, timestamps >= ymd_hms("2024-01-01 00:00:01") &timestamps <=ymd_hms ("2024-02-01 00:00:00"))
points<-sw01[3:4]
m01<-rast('ALAN_month/202401.tif')
A1<-raster::extract(m01,points,method='bilinear',ID=FALSE)
R1<-raster::extract(m01,points,buffer=5000,fun=mean,ID=FALSE)
S1<-raster::extract(m01,points,buffer=10000,fun=mean,ID=FALSE)
T1<-raster::extract(m01,points,buffer=15000,fun=mean,ID=FALSE)
U1<-raster::extract(m01,points,buffer=20000,fun=mean,ID=FALSE)
V1<-raster::extract(m01,points,buffer=20000,fun=mean,ID=FALSE)
W1<-raster::extract(m01,points,buffer=25000,fun=mean,ID=FALSE)
X1<-raster::extract(m01,points,buffer=30000,fun=mean,ID=FALSE)

sw02 <- filter(data, timestamps >= ymd_hms("2024-02-01 00:00:01") &timestamps <=ymd_hms ("2024-03-01 00:00:00"))
points<-sw02[3:4]
m02<-rast('ALAN_month/202402.tif')
A2<-raster::extract(m02,points,method='bilinear',ID=FALSE)
R2<-raster::extract(m02,points,buffer=5000,fun=mean,ID=FALSE)
S2<-raster::extract(m01,points,buffer=10000,fun=mean,ID=FALSE)
T2<-raster::extract(m01,points,buffer=15000,fun=mean,ID=FALSE)
U2<-raster::extract(m01,points,buffer=20000,fun=mean,ID=FALSE)
V2<-raster::extract(m01,points,buffer=20000,fun=mean,ID=FALSE)
W2<-raster::extract(m01,points,buffer=25000,fun=mean,ID=FALSE)
X2<-raster::extract(m01,points,buffer=30000,fun=mean,ID=FALSE)

sw03 <- filter(data, timestamps >= ymd_hms("2024-03-01 00:00:01") &timestamps <=ymd_hms ("2024-04-01 00:00:00"))
points<-sw03[3:4]
m03<-rast('ALAN_month/202403.tif')
A3<-raster::extract(m03,points,method='bilinear',ID=FALSE)
R3<-raster::extract(m03,points,buffer=5000,fun=mean,ID=FALSE)
S3<-raster::extract(m01,points,buffer=10000,fun=mean,ID=FALSE)
T3<-raster::extract(m01,points,buffer=15000,fun=mean,ID=FALSE)
U3<-raster::extract(m01,points,buffer=20000,fun=mean,ID=FALSE)
V3<-raster::extract(m01,points,buffer=20000,fun=mean,ID=FALSE)
W3<-raster::extract(m01,points,buffer=25000,fun=mean,ID=FALSE)
X3<-raster::extract(m01,points,buffer=30000,fun=mean,ID=FALSE)

sw11 <- filter(data, timestamps >= ymd_hms("2023-11-01 00:00:00") &timestamps <=ymd_hms ("2023-12-01 00:00:00"))
points<-sw11[3:4]
m11<-rast('ALAN_month/202311.tif')
A11<-raster::extract(m11,points,method='bilinear',ID=FALSE)
R11<-raster::extract(m11,points,buffer=5000,fun=mean,ID=FALSE)
S11<-raster::extract(m01,points,buffer=10000,fun=mean,ID=FALSE)
T11<-raster::extract(m01,points,buffer=15000,fun=mean,ID=FALSE)
U11<-raster::extract(m01,points,buffer=20000,fun=mean,ID=FALSE)
V11<-raster::extract(m01,points,buffer=20000,fun=mean,ID=FALSE)
W11<-raster::extract(m01,points,buffer=25000,fun=mean,ID=FALSE)
X11<-raster::extract(m01,points,buffer=30000,fun=mean,ID=FALSE)

sw12 <- filter(data, timestamps >= ymd_hms("2023-12-01 00:00:01") &timestamps <=ymd_hms ("2024-01-01 00:00:00"))
points<-sw12[3:4]
m12<-rast('ALAN_month/202312.tif')
A12<-raster::extract(m12,points,method='bilinear',ID=FALSE)
R12<-raster::extract(m12,points,buffer=5000,fun=mean,ID=FALSE)
S12<-raster::extract(m01,points,buffer=10000,fun=mean,ID=FALSE)
T12<-raster::extract(m01,points,buffer=15000,fun=mean,ID=FALSE)
U12<-raster::extract(m01,points,buffer=20000,fun=mean,ID=FALSE)
V12<-raster::extract(m01,points,buffer=20000,fun=mean,ID=FALSE)
W12<-raster::extract(m01,points,buffer=25000,fun=mean,ID=FALSE)
X12<-raster::extract(m01,points,buffer=30000,fun=mean,ID=FALSE)

ALANm<-rbind(A11,A12,A1,A2,A3)
ALAN5<-rbind(R11,R12,R1,R2,R3)
ALAN10<-rbind(R11,R12,R1,R2,R3)
ALAN15<-rbind(R11,R12,R1,R2,R3)
ALAN20<-rbind(R11,R12,R1,R2,R3)
ALAN25<-rbind(R11,R12,R1,R2,R3)
ALAN30<-rbind(R11,R12,R1,R2,R3)
colnames(ALAN5)[1]<-"avg_rad_5"
colnames(ALAN10)[1]<-"avg_rad_10"
colnames(ALAN15)[1]<-"avg_rad_15"
colnames(ALAN20)[1]<-"avg_rad_20"
colnames(ALAN25)[1]<-"avg_rad_25"
colnames(ALAN30)[1]<-"avg_rad_30"
data2324<-data%>%
  arrange(timestamps)%>%
  mutate(ALANm,
         ALAN5,
         ALAN10,
         ALAN15,
         ALAN20,
         ALAN25,
         ALAN30)
data<-rbind(data2223,data2324)

data$avg_rad <- ifelse(data$avg_rad < 0.00001, 0.00001,data$avg_rad)
data$rad_log <- log(data$avg_rad)
data$avg_rad_5 <- ifelse(data$avg_rad_5 < 0.00001, 0.00001,data$avg_rad_5)
data$rad_log_5 <- log(data$avg_rad_5)
data$avg_rad_10 <- ifelse(data$avg_rad_10 < 0.00001, 0.00001,data$avg_rad_10)
data$rad_log_10 <- log(data$avg_rad_10)
data$avg_rad_15 <- ifelse(data$avg_rad_15 < 0.00001, 0.00001,data$avg_rad_15)
data$rad_log_15 <- log(data$avg_rad_15)
data$avg_rad_20 <- ifelse(data$avg_rad_20 < 0.00001, 0.00001,data$avg_rad_20)
data$rad_log_20 <- log(data$avg_rad_20)
data$avg_rad_25 <- ifelse(data$avg_rad_25 < 0.00001, 0.00001,data$avg_rad_25)
data$rad_log_25 <- log(data$avg_rad_25)
data$avg_rad_30 <- ifelse(data$avg_rad_30 < 0.00001, 0.00001,data$avg_rad_30)
data$rad_log_30 <- log(data$avg_rad_30)

##Add day/night
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

colnames(datafinal)[21]<-"period"



#add UTM projection
#convert WGS84 EPSG4326 to EPSG32649 unit in meter
LatLong.proj <- "EPSG:4326"
UtmZone.proj <- "EPSG:32649"
UTMproj <- datafinal %>% 
  st_as_sf(coords = c('long','lat'), 
           crs = LatLong.proj) %>% 
  st_transform(UtmZone.proj)

sw_NB_22to24_senstest <- UTMproj %>%
  as_tibble() %>%
  mutate(UTMx = st_coordinates(UTMproj)[ ,1],
         UTMy = st_coordinates(UTMproj)[ ,2],
         long=datafinal$long,
         lat=datafinal$lat) %>%
  dplyr::select(-geometry)

#save the final dataset with all values
sw_NB_22to24_senstest<-sw_NB_22to24_senstest%>%
  arrange(ID,
          timestamps)
write.csv(sw_NB_22to24_senstest,file="sensitivity_test/sw_NB_22to24_senstest.csv")
save(sw_NB_22to24_senstest,file="sensitivity_test/sw_NB_22to24_senstest.Rdata")
#######################################################################################
#Sensitivity test
#Clear environment and set working directory
rm(list=ls())
setwd("C:/ALAN_StreakedShearwater")

#load packages
library(momentuHMM); library(ggplot2); library(dplyr)

### LOAD IN TRACKS
load(file="sensitivity_test/sw_NB_22to24_senstest.Rdata")
data<-as.data.frame(sw_NB_22to24_senstest)
dat <- prepData(data,type='UTM',coordNames = c("UTMx","UTMy"))
# Remove errorneous step lengths & only select night
dat <- subset(dat, step < 90000)
dat <- subset(dat,period=="night")
dat <- subset(dat,ID=="SYSUL139")
#hist(dat$step)

# INITIALISE HMM DATA ---------------------------------------------------------
#Note: initial value guessed based on the paper Behavioural mapping of a pelagic seabird: combining multiple sensors and a hidden Markov model reveals the distribution of at-sea behaviour
### initial step distribution natural scale parameters
#stepPar0 <- c(0.78,2.41,10.68,0.78,2.41,10.68) This is in km for long/lat prepdata
stepPar0 <- c(780,2410,10680,780,2410,10680)
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
save(m1, file = "sensitivity_test/mod_1.rda")
## Set up personality formulae 

formula <- list()	

formula[[2]] <- ~ avg_rad
formula[[3]] <- ~ avg_rad_5
formula[[4]] <- ~ avg_rad_10
formula[[5]] <- ~ avg_rad_15
formula[[6]] <- ~ avg_rad_20
formula[[7]] <- ~ avg_rad_25
formula[[8]] <- ~ avg_rad_30
formula[[9]] <- ~ rad_log
formula[[10]] <- ~ rad_log_5
formula[[11]] <- ~ rad_log_10
formula[[12]] <- ~ rad_log_15
formula[[13]] <- ~ rad_log_20
formula[[14]] <- ~ rad_log_25
formula[[15]] <- ~ rad_log_30
# this function gets starting values for each model from existing null model fit for a specified covariate formula
Par <- list()
for (i in 2:length(formula)){
  Par[[i]] <- getPar0(model=m1, nbStates=3, formula = formula[[i]])
}



# RUN ALL THE MODELS ------------------------------------------------------

## the following code iterates through and runs all 40 models, pasting each out in turn
stateNames <- c("Raft","Forage", "Travel")
# Specify output lists of length formula (Female)
m.list <- vector(mode = "list", length =length(formula))

for (i in 2:length(formula)) {
  print(i)
  m.list[[i]] <- fitHMM(data=dat, nbStates=3,
                        dist=list(step="gamma",angle="vm"),
                        Par0=list(step=Par[[i]]$Par$step, angle=Par[[i]]$Par$angle,delta0 = Par[[i]]$delta),
                        estAngleMean = list(angle=TRUE), beta0 = Par[[i]]$beta,
                        stateNames = stateNames, 
                        formula = formula[[i]])
  model <- m.list[[i]]
  file.out <- paste0("./sensitivity_test/", paste0("mod_", i, ".rda"))
  save(model, file = file.out)
}

#Find the best model with AIC comparison
# Specifying output lists of length formula
m.list <- vector(mode = "list", length =length(formula))
out.df <- vector(mode = "list", length =length(formula))

for (i in 1:length(formula)) {
  print(i)
  
  file.in <- paste0("./sensitivity_test/", paste0("mod_", i, ".rda"))
  load(file = file.in)
  if (i == 1) { m.list[[i]] <- m1} else { m.list[[i]] <- model}
  
  ## Extract AIC of model
  if (i == 1) { form_out <- 1} else { form_out <- as.character(formula[[i]])[2]}
  out.df[[i]] <- data.frame(Model = paste0("m", i),
                            Formula = form_out, AIC = AIC(m.list[[i]]))

  # extracting and plotting model coefficients for each transition
  beta.full <- CIbeta(m.list[[i]])$beta
  beta.full.est <- as.data.frame(beta.full$est)
  beta.full.upr <- as.data.frame(beta.full$upper)
  beta.full.lwr <- as.data.frame(beta.full$lower)
  beta.df <- data.frame(Est = c(beta.full.est$`1 -> 2`, beta.full.est$`1 -> 3`,
                                beta.full.est$`2 -> 1`, beta.full.est$`2 -> 3`,
                                beta.full.est$`3 -> 1`, beta.full.est$`3 -> 2`), 
                        Upr = c(beta.full.upr$`1 -> 2`, beta.full.upr$`1 -> 3`,
                                beta.full.upr$`2 -> 1`, beta.full.upr$`2 -> 3`,
                                beta.full.upr$`3 -> 1`, beta.full.upr$`3 -> 2`), 
                        Lwr = c(beta.full.lwr$`1 -> 2`, beta.full.lwr$`1 -> 3`,
                                beta.full.lwr$`2 -> 1`, beta.full.lwr$`2 -> 3`,
                                beta.full.lwr$`3 -> 1`, beta.full.lwr$`3 -> 2`), 
                        Transitions = rep(colnames(beta.full.est), each = nrow(beta.full.est)),
                        Covariates = rep(rownames(beta.full.est), 3))
  beta.df$Covariates <- as.factor(as.character(beta.df$Covariates))
  beta.df$Transitions <- as.factor(as.character(beta.df$Transitions))
  pd <- position_dodge(width=0.7)
  
  ## Removing intercept to plot
  beta.df2 <- subset(beta.df, Covariates != "(Intercept)",)
  
  pl <- ggplot(beta.df2, aes(Covariates, Est)) + geom_hline(yintercept=0, linetype="dashed", size=1)+
    geom_point(aes(colour = Transitions),position=pd)+
    geom_errorbar(aes(ymin=Lwr, ymax=Upr, colour = Transitions), width=.8, position=pd) +theme_bw()
  print(pl)
  name.plot <- paste0("./sensitivity_test/", paste0("mod_", i, "_coefficients.png"))
  dev.copy(png, name.plot, width = 800, height = 500)
  dev.off()
}

# Warning messages appear ("removed containing missing values") due to upper and lower CIs which are sometimes "NA"

all.out <- do.call(rbind, out.df)
all.out <- all.out[order(all.out$AIC),]
out.path <- "./sensitivity_test/AIC_table.csv"
write.csv(all.out, out.path, row.names=T)