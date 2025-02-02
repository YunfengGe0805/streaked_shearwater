###计算鸟和船的最短距离

#先把海域里的船筛选一下
library(dplyr)
for (i in 10){
  name1 <- paste0("data10",i,".csv")
  setwd("E:/wps/428212633/WPS云盘/刘阳/9白额鹱/啊啊啊我要跑代码/mission4-AIS/2023/GFWdata")
  ship <- read.csv(name1)
  filtered_ship <- ship %>%
    filter(ship$Lat >= 35 & ship$Lat <= 37 & ship$Lon >= 119 & ship$Lon <= 121)
  setwd("E:/wps/428212633/WPS云盘/刘阳/9白额鹱/啊啊啊我要跑代码/mission4-AIS/2023/narrowGFWdata")
  name2 <- paste0("cleandata10",i,".csv")
  filtered_ship <- filtered_ship[,c(2:7)]
  write.csv(filtered_ship,name2)
  i <- i+1
}

#按照date计算距离
for(r in 1){
  setwd("E:/wps/428212633/WPS云盘/刘阳/9白额鹱/啊啊啊我要跑代码/mission4-AIS/2023/narrowGFWdata")
  name1 <- paste0("cleandata11",r,".csv")
  ship <- read.csv(name1)
  ship <- ship[,c(2:7)]
  setwd("E:/wps/428212633/WPS云盘/刘阳/9白额鹱/啊啊啊我要跑代码/mission4-AIS/2023/date_trip/")
  name2 <- paste0("2023-11-0",r,".csv")
  bird <- read.csv(name2)
  # 遍历bird的每一行
  for (n in 1:nrow(bird)) {
    km30AFE <- 0
    km5AFE <- 0
    km2AFE <- 0
    coord1 <- as.numeric(bird[n, c(2, 3)])  # 提取并转换bird中的坐标
    # 遍历ship的每一行
    for (m in 1:nrow(ship)) {
      coord2 <- as.numeric(ship[m, c(2, 1)])  # 提取并转换trip中的坐标
      distance_haversine <- distHaversine(coord1, coord2, r = 6371)  # 计算哈弗辛距离单位是km
      # 如果距离小于或等于30公里
      if (distance_haversine <= 30) {
        km30FE <- ship[m, 6]  # 获取ship中相应行的第6列的值
        km30AFE <- km30AFE + km30FE  # 更新AFE的值 AFE是对于每一个n而言的
      }
      # 如果距离小于或等于5公里
      if (distance_haversine <= 5) {
        km5FE <- ship[m, 6]  # 获取ship中相应行的第6列的值
        km5AFE <- km5AFE + km5FE  # 更新AFE的值 AFE是对于每一个n而言的
      }
      # 如果距离小于或等于2公里
      if (distance_haversine <= 2) {
        km2FE <- ship[m, 6]  # 获取ship中相应行的第6列的值
        km2AFE <- km2AFE + km2FE  # 更新AFE的值 AFE是对于每一个n而言的
      }
    }
    # 将累计的km30AFE值添加到bird数据框的新列中
    bird[n, "km30AFE"] <- km30AFE
    bird[n, "km5AFE"] <- km5AFE
    bird[n, "km2AFE"] <- km2AFE
  }
  setwd("E:/wps/428212633/WPS云盘/刘阳/9白额鹱/啊啊啊我要跑代码/mission4-AIS/2023/AFE_date")
  name3 <- paste0("202311",r,".csv")
  write.csv(bird,name3)
  print(r)
}

