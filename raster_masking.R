## This script clips a raster such as a Sentinel 2 scene by polygons within a polygon shapefile.
## it is much quicker to crop and then mask rather than vice versa
## temporary directory needs to be set to somewhere with plenty of disk space and memory limit set high.
## Polygons for clipping are identified by the 'ML_ID' attribute which is hardcoded.
## Duncan Blake 21 February 2020


library(raster)
library(sf)
library(dplyr)

setwd("B:/92279_Bare_peat_Duncan/Heavy_data")

# input image
mosaic <- brick("S2A_20190515_lat60lon202_T30VWM_ORB037_utm30n_osgb_vmsk_sharp_rad_srefdem_stdsref.tif")

# input clip file
regions <- st_read("Peatland_mask_ML_simplify_singlepart.shp", quiet = TRUE)

# set temporary file directory with enough space for rasters, otherwise it uses C:\Users etc
rasterOptions(tmpdir = "B:/92279_Bare_peat_Duncan/Heavy_data")
memory.limit(size = 24499 )

# set up a loop to process polygon numbers required
region_numbers <- c(1)

for (region in region_numbers) {
  region_i <- regions %>%
    dplyr::filter(ML_ID == region)
  
  #Convert to SpatialPolygonsDataFrame
  region_i_sp <- as(region_i, 'Spatial')
  
  startTime <- Sys.time()
  cat("Starting cropping region ", region ,"at", as.character.Date(startTime), "\n")
  
  region_i_crop <- raster::crop(mosaic, region_i_sp)
  
  endTime <- Sys.time()
  cat("Finished cropping region ", region ,"at", as.character.Date(endTime), "\n")
  
  startTime <- Sys.time()
  cat("Starting masking region ", region ,"at", as.character.Date(startTime), "\n")
  
  region_i_mask <- raster::mask(region_i_crop, region_i_sp)
  writeRaster(region_i_mask, filename = paste('S2_MLarea',region,'_20190515.tif', sep=""), datatype='INT2U', overwrite=TRUE)
  
  # clean up
  remove(region_i_mask)
  remove(region_i_crop)
  
  endTime <- Sys.time()
  cat("Finished processing region ", region ,"at", as.character.Date(endTime), "\n")
  
}


