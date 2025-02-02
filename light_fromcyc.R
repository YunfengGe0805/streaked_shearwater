library(move)
library(ggmap)
library(ggplot2)
library(raster)
library(lubridate)
library(viridis)
library(moveVis)
library(moonlit)
# below not working-----
#remotes::install_github("psavoy/StreamLight")
#remotes::install_github("psavoy/StreamLightUtils")
#library(StreamLightUtils)

devtools::install_github("ateucher/lutz")
library(lutz)


# 0 data pre -----


df<- read.csv(file = "D:/SYSU/Seabird/analysis202310/data/L141.csv", header = TRUE,fileEncoding = "GBK")
colnames(df)<-c("ID","IMEID","Time","East","Long","North","Lat","speed", "direction","altitude","temperture", "battery","acctivity","satellite",   "HDOP","VDOP", "accuracy","valiade")

df$Time<-as.POSIXct(df$Time,tz="Asia/Taipei",formats=c("%Y/%m/%d %H:%M:%OS"))
df$accuracy[which(!df$accuracy %in% c("A","B","C","D"))]<-"Null"
table(df$accuracy)
df<-df[which(!df$accuracy=="Null"),]
class(df$Time)

df$tz<-tz_lookup_coords(df$Lat,df$Long,method = "accurate")
#  Determines timezone from Latitude and Longitude
table(df$tz)



# for movebank data, from.zone is UTC
# https://stackoverflow.com/questions/41648912/invalid-tz-value-problems-with-time-zone
# can't use loop here, only accept stupid way and convert to local timezone manually
unique(df$tz)
#could be more



df$Time_local[which(df$tz=="Asia/Shanghai" )]<-as.POSIXct(df$Time[which(df$tz=="Asia/Shanghai" )],tz="Asia/Shanghai" )
df$Time_local[which(df$tz=="Etc/GMT-8" )]<-as.POSIXct(df$Time[which(df$tz=="Etc/GMT-8" )],tz="Etc/GMT-8" )
df$Time_local[which(df$tz=="Etc/GMT-9" )]<-as.POSIXct(df$Time[which(df$tz=="Etc/GMT-9" )],tz="Etc/GMT-9" )
df$Time_local[which(df$tz=="Asia/Taipei" )]<-as.POSIXct(df$Time[which(df$tz=="Asia/Taipei" )],tz="Asia/Taipei" )

df$Time_local[which(df$tz=="Etc/GMT-9" )]<-with_tz(df$Time[which(df$tz=="Etc/GMT-9" )],tz="Etc/GMT-9" )
       
    
View(df[which(df$tz=="Etc/GMT-9"),c("Time","Time_local","tz")])


df<-split(df,f=df$tz)  



df<-lapply(df, function(x){
  x<-cbind(x,as.data.frame(with_tz(x$Time,tz=x$tz[1])))
  colnames(x)[20]<-"Time_local"
  # warning, be careful with the colname number  
  return(x)
})

###############################
#####   warning!!!!!   #######
################################
# dataframe cannot accept multiple time zone in the same column, so rbind a list or other way, will force different local time zone to one or make other errors
# so calculate moon light first and then merge the data at very end
# AND! remeber to reoder the dataframe by timestampe!!!!
#df<-do.call(rbind,df)
#df<-df[order(df$Time),]

library(moonlit)
x<-df[[3]]
result<-calculateMoonlightIntensity(x$Lat,x$Long,x$Time_local,e=0.28)





# 10 save and load----
library(rgdal)
writeOGR(bird.spl,dsn = "D:/SYSU/Seabird/analysis202310",layer="birds.spl",driver="ESRI Shapefile")



save.image("D:/SYSU/Seabird/analysis202310/map_seabirds.Rdata")
load("D:/SYSU/Seabird/analysis202310/map_seabirds.Rdata")
