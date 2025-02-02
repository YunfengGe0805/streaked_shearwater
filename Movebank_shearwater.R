###################
# IMPORT AND VISUALIZE
###################
library(move)
library(ctmm)
library(tidyverse)
library(lubridate)
cred <- movebankLogin(username="Yachang Cheng", password="welove107")
searchMovebankStudies(x="Qingdao", login=cred)
# case sensitive
birds <- getMovebankData(study="Tracking seabirds in the Yellow Sea of Qingdao", login=cred)
birddf<-as.data.frame(birds)
#change timezone to Beijing time
birddf$timestamp<-with_tz(birddf$timestamp,"Asia/Shanghai")
Shearwater <- filter(birddf, timestamp >= hms("00:00:00") &timestamp <=hms ("00:00:00"))

Shearwater <- filter(birddf, timestamp >= ymd_hms("2023-09-01 00:00:00") &timestamp <=ymd_hms ("2023-10-01 00:00:00"))

#get movebank animal metadata
#refbird<-getMovebankReferenceTable(study="Tracking seabirds in the Yellow Sea of Qingdao", login=cred)
#write.csv(refbird,file="C:/Movement Ecology/refbird.csv")

names(birddf)
head(birddf)
SYSUL130<-birddf[which(birddf$trackId=="SYSUL130"),]
SYSUL131<-birddf[which(birddf$trackId=="SYSUL131"),]
SYSUL105<-birddf[which(birddf$trackId=="SYSUL105"),]
SYSUL140<-birddf[which(birddf$trackId=="SYSUL140"),]
SYSUL134<-birddf[which(birddf$trackId=="SYSUL134"),]

SYSUL105selected <- filter(SYSUL105, timestamp >= ymd_hms("2023-09-01 00:00:00") &timestamp <=ymd_hms ("2023-10-01 00:00:00"))

SYSUL130selected <- filter(SYSUL130, timestamp >= ymd_hms("2023-09-01 00:00:00") &timestamp <=ymd_hms ("2023-10-01 00:00:00"))

SYSUL131selected <- filter(SYSUL131, timestamp >= ymd_hms("2023-09-01 00:00:00") &timestamp <=ymd_hms ("2023-10-01 00:00:00"))

SYSUL134selected <- filter(SYSUL134, timestamp >= ymd_hms("2023-09-01 00:00:00") &timestamp <=ymd_hms ("2023-10-01 00:00:00"))

SYSUL140selected <- filter(SYSUL140, timestamp >= ymd_hms("2023-09-01 00:00:00") &timestamp <=ymd_hms ("2023-10-01 00:00:00"))

all<-rbind(SYSUL105selected,SYSUL130selected,SYSUL131selected,SYSUL134selected,SYSUL140selected)
SW130134140<-rbind(SYSUL130selected,SYSUL134selected,SYSUL140selected)

SW130134140import<-as.telemetry(SW130134140)
#SW130import<-as.telemetry(SYSUL130selected)
#SW134import<-as.telemetry(SYSUL134selected)
#SW140import<-as.telemetry(SYSUL140selected)

names(SW140import)


class(SW130134140import)
length(SW130134140import)
summary(SW130134140import)

# plot buffalo with spatially-separated rainbow of colors
#individual closer were assigned with more distinct color
COL <- color(SW130134140import,by='individual')
plot(SW130134140import,col=COL,main="Spatial color separation")

####################
# PROJECTIONS
####################
ctmm::projection(SW130134140import)
ctmm::projection(SW130134140import) <- median(SW130134140import)

plot(SW130134140import,col=COL,main="Azimuthal-equidistant projection")
compass()


###################
# VARIOGRAM
#Autocorrelation model (acf)
###################
DATA130 <- SW130134140import$SYSUL130
DATA134 <- SW130134140import$SYSUL134
DATA140 <- SW130134140import$SYSUL140
# plot telemetry object
plot(DATA130,main="SYSUL130")
plot(DATA134,main="SYSUL134")
plot(DATA140,main="SYSUL140")
# color by time
COL <- color(DATA130,by='time')
plot(DATA130,col=COL)
# easier to see migrations/dispersals
#calculate a variogram object (named SVF) from the telemetry object
SVF130 <- variogram(DATA130)
plot(SVF130,main="Variogram130")
SVF134 <- variogram(DATA134)
plot(SVF134,main="Variogram134")
SVF140 <- variogram(DATA140)
plot(SVF140,main="Variogram140")

# on average how far apart (in distance^2) given a time lag between any two points
#variogram gives an unbiased estimation of autocorrelation compared with acf this is more suitable
# help file for variogram
#help("variogram")
# there are some options in here if you have very irregular data:
#   fast, dt, res
#vignette('variogram')
# Sec. "Irregular Sampling Schedules"

# This Gauss gives more accurate CIs, too slow for larger datasets
SVF <- variogram(DATA,CI="Gauss")

# frequently you want to zoom in to the beginning of the variogram
# plot with zoom slider
zoom(SVF,main="Variogram with good CIs")
#SVF previously used in geospatial science to test for spatial autocorrelation, here we use for temporal autocorrelation
# things to look for
# * the asymptote (if any)(the sdymptote is space autocorrelation time scale)
# if the animal disperse no asymptote
# * how long does it take to asymptote
# * initial curvature or initial linear?
#if curved as shown here, brownian motion, can estimate velocity; linear too coarse data too discrete for measuring velocity


###################
# MODEL SELECTION
###################

# model guesstimate function
help("ctmm.guess")
# variogram will be calculated automatically (with default arguments)
# this is interactive mode
#ctmm.guess(DATA130,variogram=SVF)
# notice how much work I spent automating the units of every plot

# this is noninteractive mode
GUESS130 <- ctmm.guess(DATA130,interactive=FALSE)

# automated model selection
#help("ctmm.select")

# fit a bunch of models, tell me what models are being fit, return all models, and use all but one CPU core
#FITS <- ctmm.select(DATA,GUESS130,trace=3,verbose=TRUE,cores=-1)
#summary(FITS)
FIT <- ctmm.select(DATA130,GUESS130,trace=3)
summary(FIT)
# candidate models: OUF, OUf, OUΩ, IOU, BM, IID, inactive

#to remove extra sigfig for a neat result
#sigfig(summary(FITS$`OUF anisotropic`,unit=T)$CI)
# Not KDE model yet, simply a gaussian autocorrelation model
# compare mean and covariance to data
#plot(DATA,FITS$`OUF anisotropic`,main="OUF ani Gaussian Distribution")

# compare empirical variogram to that of model
zoom(SVF130,FIT,main="OUF ani Variogram")

# calculate residuals
RES <- residuals(DATA130,FIT)

# scatter plot of residuals
plot(RES,main="OUF ani Residuals")

# calculate correlogram of residuals
ACF <- correlogram(RES,res=10)
# res=10 is for drifting sampling rate
#increase temporal resolution to account for unequal sampling interval artifact
# alternatively, fast=FALSE provide a better residual plot

zoom(ACF,main='ACF of "OUF ani" Residuals')

#figure out effective sample size manually, otherwise can compare the mean
#summary(DATA)
#(2.1715 %#% 'months') / (8.086 %#% 'days')
#help("%#%")
#months is sampling period of the DATA, days is the average time the animal needed to go across the home range
#Thus this equation calculated the smallest independent sample size needed to calculate home range of this animal

##########
# AKDE
##########
#If need help
#help("akde")
#help("bandwidth")
# regular KDE
KDE <- akde(DATA130,FIT)

# default AKDE
AKDE <- akde(DATA130,FIT)

# optimally weighted AKDE
wAKDE <- akde(DATA130,FIT,weights=TRUE)
# optimal weights
plot(DATA130$timestamp,wAKDE$weights,xlab="time",ylab="weight",main="Optimal Weights")
#data at the edge of sampling interval gaps has a higher weight and data with longer sampling interval have a higher weight

# matching extent for plotting
EXT <- extent(list(KDE,AKDE,wAKDE))

plot(DATA130,KDE,ext=EXT,main="KDE")
# note CIs, grid, etc...
summary(KDE)

plot(DATA130,AKDE,ext=EXT,main="AKDE")
summary(AKDE)

plot(DATA130,wAKDE,ext=EXT,main="optimally weighted AKDE")
summary(wAKDE)

###########################
# Home-range meta-analysis
###########################
#help("meta")

FITSmeta <- list()
for(i in 1:length(SW130134140import))
{
  GUESS <- ctmm.guess(SW130134140import[[i]],interactive=FALSE)
  FITSmeta[[i]] <- ctmm.select(SW130134140import[[i]],GUESS,trace=3)
}
names(FITSmeta) <- names(SW130134140import)
#Fits wAKDE model
wAKDES <- akde(SW130134140import,FITSmeta,weights=TRUE)
#save(wAKDES,file="wAKDESmeta.rda")
# color to be spatially distinct
COL <- color(wAKDES,by='individual')
# plot AKDEs
plot(wAKDES,col.UD=COL,col.level=COL,col.grid=NA,level=NA,main="Shearwater wAKDEs")

# meta-analysis of shearwater home-range areas
meta(wAKDES,col=c(COL,'black'),sort=TRUE)
# model selection: Dirac-delta > inverse-Gaussian
# comparing sub-groups
SHEARWATER <- list(sw130=AKDES[1],sw134=AKDES[2],sw140=AKDES[3])
META<-meta(SHEARWATER)
#This compared the difference in area
META['sw130/','/sw134',]
META['sw130/','/sw140',]

MEAN <- mean(wAKDES,sample=FALSE)
plot(SW130134140import,MEAN,col=COL,main="Mean shearwater AKDE")

PKDE <- pkde(SW130134140import,wAKDES)
plot(SW130134140import,PKDE,col=COL,main="shearwater PKDE")
#more general meta-analytic regressions
#help("Log")
#Log(FITS,variable="area")
#normalizing the data so the data can be applied to other package that only accept normal distribution

#look at individual difference and population mean of home range crossing interval
meta(FITSmeta,variable="tau_position",sort=TRUE)
############
#Occurrence distribution model
############
library(ctmm)

# range distribution - using the selected model
RD <- akde(DATA,FITS[[1]])

# occurrence distribution - using the selected model
OD <- occurrence(DATA,FITS[[1]])

# conventional (non-dynamic) Brownian bridge (conventional occurence distribution)
BB <- occurrence(DATA,FITS$BM)

# plot them
EXT <- extent(list(DATA,OD,RD))
plot(RD,col.level=NA,col.grid=NA,ext=EXT)
title("OUF AKDE")
# plot OUF occurrence distribution
plot(OD,col.level=NA,ext=EXT)
title("OUF Krige")