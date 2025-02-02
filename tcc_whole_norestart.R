library(move)
library(ggmap)
library(ggplot2)
library(raster)
library(lubridate)
library(viridis)
library(ecmwfr)
library(terra)

setwd("C:/ALAN_StreakedShearwater/cloud_cover_data")

# Set up the API key in your environment
wf_set_key(
  key = "01b34f4c-b725-4791-9c48-c55cb9276c23",
  user = "a575df32-b627-4d71-8f9f-42d48c561172"
)

# Function to download and save data for each hour of each day
download_and_save_data <- function(year, month, day, hour) {
  tiff_filename <- paste0("cloud_cover_", year, "_", month, "_", day, "_", hour, ".tiff")
  
  # Skip if the TIFF file already exists
  if (file.exists(tiff_filename)) {
    cat("File already exists: ", tiff_filename, "\n")
    return()
  }
  
  request <- list(
    dataset_short_name = "reanalysis-era5-single-levels",
    product_type = "reanalysis",
    variable = "total_cloud_cover",
    year = year,
    month = month,
    day = day,
    time = hour,
    data_format = "netcdf",
    download_format = "unarchived",
    area =  c(42, 88, -15, 155),
    target = paste0("era5_cloud_cover_", year, month, day, "_", hour, ".nc")
  )
  
  # Request data from ECMWF API
  file <- wf_request(
    request = request,
    transfer = TRUE,
    path = ".", 
    time_out = 172800 #48 hours
  )
  
  # Load the netCDF file and convert it to a raster
  r <- terra::rast(file)
  
  # Save the raster as a TIFF file
  terra::writeRaster(r, tiff_filename)
  
  cat("Downloaded and saved: ", tiff_filename, "\n")
}

# Loop through all days and hours of 2023
for (month in 1:12) {
  # Handle month with different number of days
  days_in_month <- days_in_month(ymd(paste(2023, month, 1, sep = "-")))
  
  for (day in 1:days_in_month) {
    for (hour in sprintf("%02d", 0:23)) {
      # Download and save data for each hour, if not already done
      download_and_save_data(2023, sprintf("%02d", month), sprintf("%02d", day), hour)
    }
  }
}
