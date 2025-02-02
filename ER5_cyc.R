library(move)
library(ggmap)
library(ggplot2)
library(raster)
library(lubridate)
library(viridis)
library(ecmwfr)
library(terra)

setwd("D:/map data/ER5")

#ERA5 hourly data on single levels 
#https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels?tab=download


# ECMWF
# registered email: chengyachang1990@gmail.com
# API key: 80cdb72de3a585f51dba9736ee859bf2 (valid until Nov. 7, 2025, 4:37 p.m.)

# set a key to the keychain
wf_set_key(key = "9941a053-150c-4ca6-a563-ef3c747c6e98")
wf_set_key(
  key = "01b34f4c-b725-4791-9c48-c55cb9276c23",
  user = "a575df32-b627-4d71-8f9f-42d48c561172"
)
# you can retrieve the key using
wf_get_key()
# the output should be the key you provided


# Alternatively you can input your login info with an interactive request
# if you do not put in the key directly
wf_set_key()

request <- list(
  dataset_short_name = "reanalysis-era5-single-levels",
  product_type = "reanalysis",
  variable = "total_cloud_cover",
  year = "2023",
  month = "04",
  day = "04",
  time = c("00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00"),
  data_format = "netcdf",
  download_format = "unarchived",
  area = c(42, 95, -15, 145),
  target = "era5-demo.nc"
)

# must go to the website to accept the term of used for the first time
file <- wf_request(
  request  = request,  # the request
  transfer = TRUE,     # download the file
  path     = "."       # store data in current working directory
)

r <- terra::rast(file)
terra::plot(r[[1]], main = "ERA-5 Reanalysis (total cloud cover single level) 20230404 00:00")
maps::map("world", add = TRUE)