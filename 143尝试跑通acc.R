rm(list=ls())
my.options<-options(digits.secs = 3)  
library(ggplot2)
library(R.utils)
library(lubridate)
library(tidyverse)
#0 convert to binary-----
# binary to g reference table
dec_bin_g<-as.data.frame(matrix(ncol=5,nrow =  (32768*2)))
colnames(dec_bin_g)<-c("bin15","bin16","bin_1","bin","g")
dec_bin_g$bin<-c(seq(32767,0),seq(65535,32768))
which(dec_bin_g$bin==0)
dec_bin_g$bin16<-intToBin(dec_bin_g$bin)

unique(nchar(dec_bin_g$bin16))
dec_bin_g$bin1<-substr(dec_bin_g$bin16,1,1)
dec_bin_g$bin15<-substr(dec_bin_g$bin16,2,16)
dec_bin_g$g<-seq(1.99994,-2,length=nrow(dec_bin_g))

head(dec_bin_g)
tail(dec_bin_g)
dec_bin_g[32766:32770,]

setwd("C:/ALAN_StreakedShearwater/acc")
#1 raw data----

temp.function<-function(x){
  x$Time<-as.POSIXct(x$Time,tz="Asia/Taipei",formats=c("%Y-%m-%d %H:%M:%OS"))
  x<-x[order(x$Time),]
  x$Time.round<-lubridate::round_date(x$Time,unit="20 minutes")
  
  tmp<-dec_bin_g[,c("bin","g")]
  colnames(tmp)[1]<-"X"
  tmp<-dplyr::left_join(x,tmp,by="X")
  x$X.g<-tmp$g
  
  tmp<-dec_bin_g[,c("bin","g")]
  colnames(tmp)[1]<-"Y"
  tmp<-dplyr::left_join(x,tmp,by="Y")
  x$Y.g<-tmp$g
  
  tmp<-dec_bin_g[,c("bin","g")]
  colnames(tmp)[1]<-"Z"
  tmp<-dplyr::left_join(x,tmp,by="Z")
  x$Z.g<-tmp$g
  
  
  x<-split(x,f=x$Time.round)
  x<-lapply(x, function(y){
    y$timestamp<-paste0(as.character(y$Time),".",seq(0,nrow(y)-1,nrow(y)/10)*100+1)
    y$temp.index<-seq(0,nrow(y)-1,nrow(y)/10)
    y$timestamp[1]<-paste0(y$Time[1],".001")
    
    tmp<-y[,c("ID","IMEID","Time","Time.round","X.g","timestamp","temp.index","X")]
    colnames(tmp)[5]<-"g.value"
    colnames(tmp)[8]<-"value"
    tmp$axis<-"X"
    
    tmp2<-y[,c("ID","IMEID","Time","Time.round","Y.g","timestamp","temp.index","Y")]
    colnames(tmp2)[5]<-"g.value"
    colnames(tmp2)[8]<-"value"
    tmp2$axis<-"Y"
    
    tmp3<-y[,c("ID","IMEID","Time","Time.round","Z.g","timestamp","temp.index","Z")]
    colnames(tmp3)[5]<-"g.value"
    colnames(tmp3)[8]<-"value"
    tmp3$axis<-"Z"
    y<-rbind(tmp,tmp2,tmp3)
    
    return(y)
  })
  
  return(x)
}

data<-read.csv(file="C:/ALAN_StreakedShearwater/acc/acc100new.csv",row.names = NULL,fill=TRUE)

colnames(data)<-c("ID","IMEID","Time","X","Y","Z","No")
data<-data%>%
  select(-No)

data$ID<-"SYSUL100"
write.csv(data,file="data100.csv")
data<-temp.function(data)
x<-data[[1]]
data<-lapply(data, function(x){
  x$time_tmp<- c(seq(1:(nrow(x)/3)),seq(1:(nrow(x)/3)),seq(1:(nrow(x)/3)))
  return(x)
})

library(tagtools)
odba_mean<-function(data,sampling_rate,fh,method,n){
  # n need further check
  df<-lapply(data,function(x){
    tmp<-array(data=cbind(cbind(x[1:(nrow(x)/3),"g.value"],x[nrow(x)/3+1:2*(nrow(x)/3),"g.value"]),x[(2*(nrow(x)/3)+1):nrow(x),"g.value"]),dim=(c(nrow(x),3)))
    tmp<-odba(A = tmp, sampling_rate = sampling_rate, fh = fh ,method=method,n=n)
    #sampling_rate = 10, fh = 0.5 ,method="wilson",n=2
    x$dba<-mean(tmp,na.rm = T)
    x$duration<-nrow(x)/30
    return(x)
  })
  
  result<-do.call(rbind,df)
  result<-result[!duplicated(result$Time.round),]
  
  return(result)
}

vedba<-odba(data,10,0.5,"vedba",2)

vedba_result<-odba_mean(data,10,0.5,"vedba",2)
vedba_result<-odba_mean(data[1:19],10,0.5,"vedba",2)
vedba_result<-odba_mean(data[c(1:19,21:56)],10,0.5,"vedba",2)
write.csv(vedba_result,"129trip10vedba.csv")

ggplot(data = data[[30]], mapping = aes(time_tmp/10, y = g.value,group=axis,col=axis)) + geom_line()+ggtitle ("2021-08-31 04:00:01-04:00:07") +xlab("时间(s)")+ylab("加速度（g）")+labs(colour="轴")+coord_cartesian(ylim = c(-2, 2))+theme_classic()


#####没跑
pdf("D:/SYSU/Seabird/analysis2023/figure/L108_last_acc.pdf",width = 5, height =5)
for (i in 1:length(data)){
  tmp<-data[[i]]
  print(ggplot(data = tmp, mapping = aes(x = time_tmp, y = g.value,group=axis,col=axis)) + geom_line()+ggtitle (paste0(tmp$ID[1]," ",tmp$Time[1]))+coord_cartesian(ylim = c(-2, 2))+theme_bw())}

dev.off()
#####没跑结束

# 2 calculate veDBA and ODBA-----
library(tagtools)

# 2.1 ODBA-----
###没跑
BW <- beaked_whale
View(BW$A$sampling_rate)
e <- odba(A = BW$A$data, sampling_rate = BW$A$sampling_rate, fh = 0.05)
ba <- list(e = e)
plott(ba, BW$A$sampling_rate)
###没跑结束

# convert HQXS data to tagtools format
x<-data[[2]]

odba_mean<-function(data,sampling_rate,fh,method,n){
  # n need further check
  df<-lapply(data,function(x){
    tmp<-array(data=cbind(cbind(x[1:(nrow(x)/3),"g.value"],x[nrow(x)/3+1:2*(nrow(x)/3),"g.value"]),x[(2*(nrow(x)/3)+1):nrow(x),"g.value"]),dim=(c(nrow(x),3)))
    tmp<-odba(A = tmp, sampling_rate = sampling_rate, fh = fh ,method=method,n=n)
    #sampling_rate = 10, fh = 0.5 ,method="wilson",n=2
    x$dba<-mean(tmp,na.rm = T)
    x$duration<-nrow(x)/30
    return(x)
  })
  
  result<-do.call(rbind,df)
  result<-result[!duplicated(result$Time.round),]
  
  return(result)
  }


# 假设 databases 是包含400个数据库的列表
# 首先，检查每个数据库的行数
row_counts <- sapply(data, nrow)
# 然后，创建一个逻辑向量，用于筛选出行数大于或等于210的数据库
indexes_to_keep <- row_counts >= 210
# 使用逻辑索引来筛选数据库
filtered_databases <- data[indexes_to_keep]
# filtered_databases 现在是一个只包含行数大于或等于210的数据库的列表
datanew <- filtered_databases
tmp<-array(data=cbind(cbind(datanew[1:(nrow(datanew)/3),"g.value"],datanew[nrow(datanew)/3+1:2*(nrow(datanew)/3),"g.value"]),datanew[(2*(nrow(datanew)/3)+1):nrow(datanew),"g.value"]),dim=(c(nrow(x),3)))
tmp<-odba(A = tmp, sampling_rate = 10, fh = 0.5 ,method="vedba",n=2)
    
#10 save and load-----
load("D:/SYSU/Seabird/analysis2023/HQXS_acc_v2.Rdata")



