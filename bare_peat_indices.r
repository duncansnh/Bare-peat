# calculates various indices for bare peat mapping 
# vectors too large to apply in one script, therefore carried out with single raster at a time, controlled
# input by moving rasters between folders on remote sensing drive.

library(raster)
library(stringr)

setwd("B:/92279_Bare_peat_Duncan/R_classification")
inRaster <- "S2_MLarea4.tif"
img_br <- brick(inRaster)

###### functions to calculate various ndvi indices, water indices and blue:SWIR1 ratio
red <- img_br[[3]]
green <- img_br[[2]]
blue <- img_br[[1]]
Re5 <- img_br[[4]]
Re6 <- img_br[[5]]
Re7 <- img_br[[6]]
Re8A <- img_br[[8]]
NIR <- img_br[[7]]
SWIR1 <- img_br[[9]]
SWIR2 <- img_br[[10]]

  
ndvi <- function(NIR, red) {
  (NIR - red) / (NIR+red)
}
ndvi_ <- ndvi(NIR,red)
  
Gndvi <- function(NIR, green) {
  (NIR - green) / (NIR+green)
}
Gndvi_ <- Gndvi(NIR,green)
  
  
Bndvi <- function(NIR, blue) {
  (NIR - blue) / (NIR+blue)
}
Bndvi_ <- Bndvi(NIR,blue)
  
  
Re5ndvi <- function(Re5, red) {
  (Re5 - red) / (Re5+red)
}
Re5ndvi_ <-Re5ndvi(Re5,red)


Re6ndvi <- function(Re6, red) {
  (Re6 - red) / (Re6+red)
}
Re6ndvi_<- Re6ndvi(Re6,red)

Re7ndvi <- function(Re7, red) {
  (Re7 - red) / (Re7+red)
}
Re7ndvi_<- Re7ndvi(Re7,red)

Re8Andvi <- function(Re8A, red) {
  (Re8A - red) / (Re8A+red)
}
Re8Andvi_<- Re8Andvi(Re8A, red)

Re6Gndvi <- function(Re6, green) {
  (Re6 - green) / (Re6+green)
}
Re6Gndvi_<- Re6Gndvi(Re6,green)


Re7Gndvi <- function(Re7, green) {
  (Re7 - green) / (Re7+green)
}
Re7Gndvi_ <-Re7Gndvi(Re7, green)

Re8AGndvi <- function(Re8A, green) {
  (Re8A - green) / (Re8A+green)
}
Re8AGndvi_ <-Re8AGndvi(Re8A, green)

NDWI <- function (green, NIR){
  (green-NIR)/(green+NIR)
}
NDWI_ <-NDWI(green,NIR)

mDNWI <- function (green,SWIR1){
  (green-SWIR1)/(green+SWIR1)
}
mDNWI_ <-mDNWI(green,SWIR1)

mNDVI <- function(Re7,Re5){
  (Re7-Re5)/(Re7+Re5)
}
mNDVI_ <-mNDVI(Re7,Re5)

darkness <- function(red,green,blue){
  (red+green+blue)/3
}
darkness_ <- darkness(red,green,blue)

ratioblueSWIR <- function(blue,SWIR1){
  (blue/SWIR1)
}
ratioblueSWIR_ <- ratioblueSWIR(blue,SWIR1)
# rm(BLUEbandVector)

ratioNIRgreen <- function(NIR,green){
  (NIR/green)
}
ratioNIRgreen_ <- ratioNIRgreen(NIR, green)


ratioredSWIR1 <- function(red,SWIR1){
  (red/SWIR1)
}
ratioredSWIR1_ <-ratioredSWIR1(red,SWIR1)
  
ratioNIRSWIR1 <- function(NIR,SWIR1){
  (NIR/SWIR1)
}
ratioNIRSWIR1_ <-ratioNIRSWIR1(NIR,SWIR1)

ratioRe5SWIR1 <- function(Re5,SWIR1){
  (Re5/SWIR1)
}
ratioRe5SWIR1_ <-ratioRe5SWIR1(Re5,SWIR1)

ratioRe6SWIR1 <- function(Re6,SWIR1){
  (Re6/SWIR1)
}
ratioRe6SWIR1_ <-ratioRe6SWIR1(Re6,SWIR1)

ratioRe7SWIR1 <- function(Re7,SWIR1){
  (Re7/SWIR1)
}
ratioRe7SWIR1_ <-ratioRe7SWIR1(Re7,SWIR1)

#-----------------------  stacking of outputs--------------------------#
filename <- unlist(strsplit(inRaster, ".", fixed = TRUE))
    
outStack <- paste(filename[1], "_Indices_Extra", ".tif", sep="")
print (outStack)

newStack <- stack(img_br, ndvi_,Gndvi_ ,Bndvi_, Re5ndvi_, Re6ndvi_, Re7ndvi_,Re8Andvi_,Re6Gndvi_, Re7Gndvi_, Re8AGndvi_, NDWI_, mDNWI_, mNDVI_, darkness_ , ratioblueSWIR_, ratioNIRgreen_ , ratioredSWIR1_, ratioNIRSWIR1_, ratioRe5SWIR1_, ratioRe6SWIR1_, ratioRe7SWIR1_)
writeRaster(newStack, filename= outStack, format='GTiff', datatype='FLT4S', overwrite=TRUE)
















