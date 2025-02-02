#Clear environment and set working directory
rm(list=ls())
setwd("C:/ALAN_StreakedShearwater")

#Load packages
library(move)
#library(move2)
library(momentuHMM)
library(stats)
library(tidyverse)
library(lubridate)
library(terra)
library(sf)

#Data preparation
##Load dataset
cred <- movebankLogin(username="Yachang Cheng", password="welove107")
searchMovebankStudies(x="Qingdao", login=cred)
birds <- getMovebankData(study="Tracking seabirds in the Yellow Sea of Qingdao", login=cred,timestamp_start="20230101000000000",timestamp_end="20231231235959000")
save(birds,file="birds.Rdata")
##Already saved in Rdata
load(file="birds.Rdata")
##Select F139 and M140
F139<-birds[['SYSUL139']]
M140<-birds[['SYSUL140']]
##Check time lag
tl139<-timeLag(F139,units="mins")
tl140<-timeLag(M140,units="mins")
summary(tl139)
summary(tl140)
plot(tl139)
plot(tl140)
###Mainly 20min but some large gaps exist

#Interpolation
##Move package
ip139 <- interpolateTime(F139, time=as.difftime(20, units="mins"), spaceMethod='greatcircle')
ip140 <- interpolateTime(M140, time=as.difftime(20, units="mins"), spaceMethod='greatcircle')
##Check time lag
tlip139<-timeLag(ip139,units="mins")
tlip140<-timeLag(ip140,units="mins")
summary(tlip139)
summary(tlip140)
###Equal 20min interval achieved

##Move2 package

##momentuHMM package crawlwrap

#Bursting by day (using yday as burst)
##Interpolation from move
##Convert move object to dataframe
dat139<-as.data.frame(ip139)
dat140<-as.data.frame(ip140)
##Change time zone
dat139$timestamps<-with_tz(dat139$timestamps,"Asia/Shanghai")
dat140$timestamps<-with_tz(dat140$timestamps,"Asia/Shanghai")
##Clean the dataset
f139<-dat139%>%
  dplyr::select(long=location_long.1,
         lat=location_lat.1,
         timestamps,
         ID=local_identifier)

m140<-dat140%>%
  filter(timestamps<ymd_hms("2023-12-31 00:00:00"))%>%
  dplyr::select(long=location_long.1,
         lat=location_lat.1,
         timestamps,
         ID=local_identifier)

##Add hour to datasets
f139$hr <- hour(f139$timestamps)
m140$hr <- hour(m140$timestamps)

#Filter out only day/night track data
nf139<-filter(f139,hr==0|hr==1|hr==2|hr==3|hr==4|hr==5|hr==6|hr==23|hr==22|hr==21|hr==20|hr==19)
nf139$class<-"night"
df139<-filter(f139,hr==7|hr==8|hr==9|hr==10|hr==11|hr==12|hr==13|hr==14|hr==15|hr==16|hr==17|hr==18)
df139$class<-"day"
nm140<-filter(m140,hr==0|hr==1|hr==2|hr==3|hr==4|hr==5|hr==6|hr==23|hr==22|hr==21|hr==20|hr==19)
nm140$class<-"night"
dm140<-filter(m140,hr==7|hr==8|hr==9|hr==10|hr==11|hr==12|hr==13|hr==14|hr==15|hr==16|hr==17|hr==18)
dm140$class<-"day"
#Extract environmental data
##Extract monthly ALAN data for f139
sw01 <- filter(nf139, timestamps >= ymd_hms("2023-01-01 00:00:00") &timestamps <=ymd_hms ("2023-02-01 00:00:00"))
points<-sw01[1:2]
m01<-rast('ALAN_month/202301.tif')
A1<-terra::extract(m01,points,method="simple",ID=FALSE)

sw02 <- filter(nf139, timestamps >= ymd_hms("2023-02-01 00:00:01") &timestamps <=ymd_hms ("2023-03-01 00:00:00"))
points<-sw02[1:2]
m02<-rast('ALAN_month/202302.tif')
A2<-terra::extract(m02,points,method="simple",ID=FALSE)

sw03 <- filter(nf139, timestamps >= ymd_hms("2023-03-01 00:00:01") &timestamps <=ymd_hms ("2023-04-01 00:00:00"))
points<-sw03[1:2]
m03<-rast('ALAN_month/202303.tif')
A3<-terra::extract(m03,points,method="simple",ID=FALSE)

sw04 <- filter(nf139, timestamps >= ymd_hms("2023-04-01 00:00:01") &timestamps <=ymd_hms ("2023-05-01 00:00:00"))
points<-sw04[1:2]
m04<-rast('ALAN_month/202304.tif')
A4<-terra::extract(m04,points,method="simple",ID=FALSE)

sw05 <- filter(nf139, timestamps >= ymd_hms("2023-05-01 00:00:01") &timestamps <=ymd_hms ("2023-06-01 00:00:00"))
points<-sw05[1:2]
m05<-rast('ALAN_month/202305.tif')
A5<-terra::extract(m05,points,method="simple",ID=FALSE)

sw06 <- filter(nf139, timestamps >= ymd_hms("2023-06-01 00:00:01") &timestamps <=ymd_hms ("2023-07-01 00:00:00"))
points<-sw06[1:2]
m06<-rast('ALAN_month/202306.tif')
A6<-terra::extract(m06,points,method="simple",ID=FALSE)

sw07 <- filter(nf139, timestamps >= ymd_hms("2023-07-01 00:00:01") &timestamps <=ymd_hms ("2023-08-01 00:00:00"))
points<-sw07[1:2]
m07<-rast('ALAN_month/202307.tif')
A7<-terra::extract(m07,points,method="simple",ID=FALSE)

sw08 <- filter(nf139, timestamps >= ymd_hms("2023-08-01 00:00:01") &timestamps <=ymd_hms ("2023-09-01 00:00:00"))
points<-sw08[1:2]
m08<-rast('ALAN_month/202308.tif')
A8<-terra::extract(m08,points,method="simple",ID=FALSE)

ALANm<-rbind(A1,A2,A3,A4,A5,A6,A7,A8)
nf139<-nf139%>%
  mutate(ALANm)
colnames(nf139)[7]<-"ALAN"
## Combine day/might dataset
df139$ALAN<-rep(0.0001,times=nrow(df139))
f139rs<-rbind(nf139,df139)
##Extract ChollA data for f139
points<-f139rs[1:2]
ChollA2023<-rast('ChollA2023.tif')
ChollAdata<-terra::extract(ChollA2023,points,method="simple",ID=FALSE)
f139rs<-f139rs%>%
  mutate(ChollAdata)%>%
  arrange(timestamps)



##Extract monthly ALAN data for m140
sw01 <- filter(nm140, timestamps >= ymd_hms("2023-01-01 00:00:00") &timestamps <=ymd_hms ("2023-02-01 00:00:00"))
points<-sw01[1:2]
m01<-rast('ALAN_month/202301.tif')
A1<-terra::extract(m01,points,method="simple",ID=FALSE)

sw02 <- filter(nm140, timestamps >= ymd_hms("2023-02-01 00:00:01") &timestamps <=ymd_hms ("2023-03-01 00:00:00"))
points<-sw02[1:2]
m02<-rast('ALAN_month/202302.tif')
A2<-terra::extract(m02,points,method="simple",ID=FALSE)

sw03 <- filter(nm140, timestamps >= ymd_hms("2023-03-01 00:00:01") &timestamps <=ymd_hms ("2023-04-01 00:00:00"))
points<-sw03[1:2]
m03<-rast('ALAN_month/202303.tif')
A3<-terra::extract(m03,points,method="simple",ID=FALSE)

sw04 <- filter(nm140, timestamps >= ymd_hms("2023-04-01 00:00:01") &timestamps <=ymd_hms ("2023-05-01 00:00:00"))
points<-sw04[1:2]
m04<-rast('ALAN_month/202304.tif')
A4<-terra::extract(m04,points,method="simple",ID=FALSE)

sw05 <- filter(nm140, timestamps >= ymd_hms("2023-05-01 00:00:01") &timestamps <=ymd_hms ("2023-06-01 00:00:00"))
points<-sw05[1:2]
m05<-rast('ALAN_month/202305.tif')
A5<-terra::extract(m05,points,method="simple",ID=FALSE)

sw06 <- filter(nm140, timestamps >= ymd_hms("2023-06-01 00:00:01") &timestamps <=ymd_hms ("2023-07-01 00:00:00"))
points<-sw06[1:2]
m06<-rast('ALAN_month/202306.tif')
A6<-terra::extract(m06,points,method="simple",ID=FALSE)

sw07 <- filter(nm140, timestamps >= ymd_hms("2023-07-01 00:00:01") &timestamps <=ymd_hms ("2023-08-01 00:00:00"))
points<-sw07[1:2]
m07<-rast('ALAN_month/202307.tif')
A7<-terra::extract(m07,points,method="simple",ID=FALSE)

sw08 <- filter(nm140, timestamps >= ymd_hms("2023-08-01 00:00:01") &timestamps <=ymd_hms ("2023-09-01 00:00:00"))
points<-sw08[1:2]
m08<-rast('ALAN_month/202308.tif')
A8<-terra::extract(m08,points,method="simple",ID=FALSE)

sw09 <- filter(nm140, timestamps >= ymd_hms("2023-09-01 00:00:01") &timestamps <=ymd_hms ("2023-10-01 00:00:00"))
points<-sw09[1:2]
m09<-rast('ALAN_month/202309.tif')
A9<-terra::extract(m09,points,method="simple",ID=FALSE)

sw10 <- filter(nm140, timestamps >= ymd_hms("2023-10-01 00:00:01") &timestamps <=ymd_hms ("2023-11-01 00:00:00"))
points<-sw10[1:2]
m10<-rast('ALAN_month/202310.tif')
A10<-terra::extract(m10,points,method="simple",ID=FALSE)

sw11 <- filter(nm140, timestamps >= ymd_hms("2023-11-01 00:00:01") &timestamps <=ymd_hms ("2023-12-01 00:00:00"))
points<-sw11[1:2]
m11<-rast('ALAN_month/202311.tif')
A11<-terra::extract(m11,points,method="simple",ID=FALSE)

sw12 <- filter(nm140, timestamps >= ymd_hms("2023-12-01 00:00:01") &timestamps <=ymd_hms ("2024-01-01 00:00:00"))
points<-sw12[1:2]
m12<-rast('ALAN_month/202312.tif')
A12<-terra::extract(m12,points,method="simple",ID=FALSE)

ALANm<-rbind(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12)
nm140<-nm140%>%
  mutate(ALANm)
colnames(nm140)[7]<-"ALAN"
## Combine day/might dataset
dm140$ALAN<-rep(0.0001,times=nrow(dm140))
m140rs<-rbind(nm140,dm140)
##Extract ChollA data for m140
points<-m140rs[1:2]
ChollA2023<-rast('ChollA2023.tif')
ChollAdata<-terra::extract(ChollA2023,points,method="simple",ID=FALSE)
m140rs<-m140rs%>%
  mutate(ChollAdata)%>%
  arrange(timestamps)


#Implement HMM in momentuHMM
##Prepare data (angle and step length calculated)
datf139 <- prepData(f139rs, type = 'LL', coordNames = c("long","lat"),covNames="ALAN")
datm140 <- prepData(m140rs, type = 'LL', coordNames = c("long","lat"),covNames="ALAN")


##Check if there are 0 in step (if yes need zeromass parameter)
summary(datm140$step)
summary(datf139$step)
###No zero step


#Fit 3 states HMM models
set.seed(0805)

##Fit model for F139 
### initial step distribution natural scale parameters
stepPar0 <- c(0.78,2.41,10.68,0.78,2.41,10.68)
#guess based on Behavioural mapping of a pelagic seabird: combining multiple sensors and a hidden Markov model reveals the distribution of at-sea behaviour
### initial angle distribution natural scale parameters
anglePar0 <- c(0,3,0,1,10,1)
##Set up model
set.seed(0805)
##Null model
null_3states_f139 <- fitHMM(data = datf139,
                            nbStates = 3,
                            dist = list(step = "gamma", angle = "vm"),
                            Par0 = list(step = stepPar0, angle = anglePar0),
                            formula = ~ 1,
                            stateNames = c('Resting', 'Foraging', 'Travelling'),
                            estAngleMean = list(angle=TRUE)
)
null_3states_f139
plot(null_3states_f139)
states<-viterbi(null_3states_f139)
f139states<-cbind(f139rs,states)
write.csv(f139states,file = "f139states.csv")
##ALAN model
ALAN_3states_f139 <- fitHMM(data = datf139,
                            nbStates = 3,
                            dist = list(step = "gamma", angle = "vm"),
                            Par0 = list(step = stepPar0, angle = anglePar0),
                            formula = ~ ALAN,
                            estAngleMean = list(angle=TRUE),
                            stateNames = c('Resting', 'Foraging', 'Travelling')
)

ALAN_3states_f139
tp139<-getTrProbs(ALAN_3states_f139,getCI=TRUE)
tp139dat<-t(as.data.frame(tp139$est))
tp139dat<-as.data.frame(tp139dat)
save(tp139dat,file="tp139dat.Rdata")
load(file="tp139dat.Rdata")
load(file="f139rs.Rdata")
tp139dat$class<-rep(c("Resting","Foraging","Travelling"),times=3)
torest<-subset(tp139dat,class=="Resting")
torest$ALAN<-f139rs$ALAN
toforage<-subset(tp139dat,class=="Foraging")
toforage$ALAN<-f139rs$ALAN
#Make a transition prob plot
#Scatter plot for resting
a<-ggplot(torest, aes(x=ALAN, y=Foraging)) + 
  geom_point()+
  geom_smooth(color="orange")+
  ggtitle("F139 Transition Probability from Foraging to Resting") +
  ylab("Probability")
a + theme_bw()

a<-ggplot(torest, aes(x=ALAN, y=Resting)) + 
  geom_point()+
  geom_smooth(color="orange")+
  ggtitle("F139 Stationary Probability in Resting") +
  ylab("Probability")
a + theme_bw()

a<-ggplot(torest, aes(x=ALAN, y=Travelling)) + 
  geom_point()+
  geom_smooth(color="orange")+
  ggtitle("F139 Transition Probability from Travelling to Resting") +
  ylab("Probability")
a + theme_bw()

#Scatter plot for foraging
b<-ggplot(toforage, aes(x=ALAN, y=Resting)) + 
  geom_point()+
  geom_smooth(color="orange")+
  ggtitle("F139 Transition Probability from Resting to Foraging") +
  ylab("Probability")
b + theme_bw()

b<-ggplot(toforage, aes(x=ALAN, y=Foraging)) + 
  geom_point()+
  geom_smooth(color="orange")+
  ggtitle("F139 Stationary Probability in Foraging") +
  ylab("Probability")
b + theme_bw()

b<-ggplot(toforage, aes(x=ALAN, y=Travelling)) + 
  geom_point()+
  geom_smooth(color="orange")+
  ggtitle("F139 Transition Probability from Travelling to Foraging") +
  ylab("Probability")
b + theme_bw()


plot(ALAN_3states_f139,plotCI=TRUE)
states<-viterbi(ALAN_3states_f139)
f139ALANstates<-cbind(f139rs,states)
#write.csv(f139ALANstates,file = "f139ALANstates.csv")

##CHLA model
CHLA_3states_f139 <- fitHMM(data = datf139,
                            nbStates = 3,
                            dist = list(step = "gamma", angle = "vm"),
                            Par0 = list(step = stepPar0, angle = anglePar0),
                            formula = ~ CHLA_AVE,
                            estAngleMean = list(angle=TRUE),
                            stateNames = c('Resting', 'Foraging', 'Travelling')
)

CHLA_3states_f139
plot(CHLA_3states_f139,plotCI=TRUE)
states<-viterbi(CHLA_3states_f139)
f139CHLAstates<-cbind(f139rs,states)
write.csv(f139CHLAstates,file = "f139CHLAstates.csv")


##ALAN with fixed effect model
ALANfix_3states_f139 <- fitHMM(data = datf139,
                            nbStates = 3,
                            dist = list(step = "gamma", angle = "vm"),
                            Par0 = list(step = stepPar0, angle = anglePar0),
                            formula = ~class+ALAN,
                            estAngleMean = list(angle=TRUE),
                            stateNames = c('Resting', 'Foraging', 'Travelling')
)

plot(ALANfix_3states_f139)


##Fit model for M140 
### initial step distribution natural scale parameters
stepPar0 <- c(0.78,2.41,10.68,0.78,2.41,10.68)#guess based on McClintock et al,2017
### initial angle distribution natural scale parameters
anglePar0 <- c(0,3,0,1,10,1)
##Set up model
set.seed(0805)
##Null model
null_3states_m140 <- fitHMM(data = datm140,
                            nbStates = 3,
                            dist = list(step = "gamma", angle = "vm"),
                            Par0 = list(step = stepPar0, angle = anglePar0),
                            formula = ~ 1,
                            stateNames = c('Resting', 'Foraging', 'Travelling'),
                            estAngleMean = list(angle=TRUE)
)
null_3states_m140
plot(null_3states_m140)
states<-viterbi(null_3states_m140)
m140states<-cbind(m140rs,states)
write.csv(m140states,file = "m140states.csv")
##ALAN model
ALAN_3states_m140 <- fitHMM(data = datm140,
                            nbStates = 3,
                            dist = list(step = "gamma", angle = "vm"),
                            Par0 = list(step = stepPar0, angle = anglePar0),
                            formula = ~ ALAN,
                            stateNames = c('Resting', 'Foraging', 'Travelling'),
                            estAngleMean = list(angle=TRUE)
)

ALAN_3states_m140
plot(ALAN_3states_m140,plotCI=TRUE)
states<-viterbi(ALAN_3states_m140)
m140ALANstates<-cbind(m140rs,states)
write.csv(m140ALANstates,file = "m140ALANstates.csv")

tp140<-getTrProbs(ALAN_3states_m140,getCI=TRUE)
tp140dat<-t(as.data.frame(tp140$est))
tp140dat<-as.data.frame(tp140dat)
save(tp140dat,file="tp140dat.Rdata")
load(file="tp140dat.Rdata")
load(file="m140rs.Rdata")
tp140dat$class<-rep(c("Resting","Foraging","Travelling"),times=3)
torest<-subset(tp140dat,class=="Resting")
torest$ALAN<-m140rs$ALAN
toforage<-subset(tp140dat,class=="Foraging")
toforage$ALAN<-m140rs$ALAN
#Make a transition prob plot
#Scatter plot for resting
a<-ggplot(torest, aes(x=ALAN, y=Foraging)) + 
  geom_point()+
  geom_smooth(color="orange")+
  ggtitle("M140 Transition Probability from Foraging to Resting") +
  ylab("Probability")
a + theme_bw()

a<-ggplot(torest, aes(x=ALAN, y=Resting)) + 
  geom_point()+
  geom_smooth(color="orange")+
  ggtitle("M140 Stationary Probability in Resting") +
  ylab("Probability")
a + theme_bw()

a<-ggplot(torest, aes(x=ALAN, y=Travelling)) + 
  geom_point()+
  geom_smooth(color="orange")+
  ggtitle("M140 Transition Probability from Travelling to Resting") +
  ylab("Probability")
a + theme_bw()

#Scatter plot for foraging
b<-ggplot(toforage, aes(x=ALAN, y=Resting)) + 
  geom_point()+
  geom_smooth(color="orange")+
  ggtitle("M140 Transition Probability from Resting to Foraging") +
  ylab("Probability")
b + theme_bw()

b<-ggplot(toforage, aes(x=ALAN, y=Foraging)) + 
  geom_point()+
  geom_smooth(color="orange")+
  ggtitle("M140 Stationary Probability in Foraging") +
  ylab("Probability")
b + theme_bw()

b<-ggplot(toforage, aes(x=ALAN, y=Travelling)) + 
  geom_point()+
  geom_smooth(color="orange")+
  ggtitle("M140 Transition Probability from Travelling to Foraging") +
  ylab("Probability")
b + theme_bw()

##CHLA model
CHLA_3states_m140 <- fitHMM(data = datm140,
                            nbStates = 3,
                            dist = list(step = "gamma", angle = "vm"),
                            Par0 = list(step = stepPar0, angle = anglePar0),
                            formula = ~ CHLA_AVE,
                            estAngleMean = list(angle=TRUE),
                            stateNames = c('Resting', 'Foraging', 'Travelling')
)

CHLA_3states_m140
plot(CHLA_3states_m140,plotCI=TRUE)
states<-viterbi(CHLA_3states_m140)
m140CHLAstates<-cbind(m140rs,states)
write.csv(m140CHLAstates,file = "m140CHLAstates.csv")










##backup notes##
##Assess models
null_3states_f139
plot(null_3states_f139)  
plotStates(null_3states_f139)  
timeInStates(null_3states_f139) 
plotPR(null_3states_f139, ncores = 5) 
trProbs <- getTrProbs(m)

##Compare among models

AIC(fit_hmm_2states, fit_hmm_3states, fit_hmm_2states_covar1, fit_hmm_2states_covar2,
    fit_hmm_3states_covar1)
AICweights(fit_hmm_2states, fit_hmm_3states, fit_hmm_2states_covar1, fit_hmm_2states_covar2,
           fit_hmm_3states_covar1)

#Trial of using burst(no use)
nf139$burst<-c(rep(1:235,each=36),rep(236,times=6))
# the 235 is calculated as 8466(total # of obs)-6(obs of the last night)=8460;each whole night has 36 obs
#Since the last night only has 6 obs, we delete that night
nf139<-subset(nf139,burst!=236)