#############################################################################
# Modifed script, original from CEH for peatland condition classification, then 
#joblog 88131 , 88296 and currently for bare peat 92279. Original is in A2567145
#
# The script reads an ESRI Shapefile (defined by the "shapefile" variable) with
# training polygons and then selects a
# user-determined number of samples (defied by the "numsamps" variable) from
# each land cover type. A multilayer image that contains spectral, other
# continuous data or categorical data is also input (defined by the inImage
# variable). For each randomly selected sample the data values for that pixel
# are determined and these data are used to run the Random Forest model.
#
# After building the model the multilayer image is read, and up to three output
# layers (classImage, probImage, threshImage) can be selected for the output image.
# "classImage" classifies all of the pixels.
#
# "probImage" outputs the class probability of the class that got the most votes
# (i.e., the class that was selected for the classImage layer).
#
# "threshImage" is the same as "classImage" except all pixels with a class probability
# of the class that got the most votes below the "probThreshold" parameter are set to 0.
# This is useful to identify pixels with inter-class confusion.
#
# The image is written out (name and location is defined by the "outImage variable)
# using the GeoTIFF format. A variable importance plot is displayed to provide information
# about the influence of each variable. An error rate estimate and confusion matrix are also
# printed to provide information about classification accuracy.
#
# There is an option to assess the quality of the training data. The metric for this
# is the "margin". The margin of a training point is the proportion of votes for the correct
# class minus maximum proportion of votes for the other classes for that segment. Positive margin
# values represent correct classification, and vice versa. The margin data are written to a
# point ESRI Shapefile so they can be overlaid on the image and training polygons to assess which
# points need to be removed and relabeled in the training data and it can help determine which
# classes needs additional training segments. If this output is not needed you can enter two
# double or single-quotes ("" or '') for the variable outPointsFile.
#
# There is also an option to output a feature space plot using two bands of your choice.
# If a feature space plot is not needed then enter "0" for the variables xBand and/or yBand.
#
# Set the variables below in the "SET VARIABLES HERE" section of the script.
#
# This script was written by Ned Horning [horning@amnh.org]
# Support for writing and maintaining this script comes from The John D. and
# Catherine T. MacArthur Foundation and Google.org.
#
# This script is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software Foundation
# either version 2 of the License, or (at your option) any later version.
#
#############################################################################

#Load libraries
library(rgdal)
library(sp)
library(randomForest)
library(raster)
library(caret)
library(dplyr)

# set working directory using forward slashes

setwd("B:/92279_Bare_peat_Duncan/R_classification") 

cat("Set variables and start processing\n")

# set temporary file directory with enough space for rasters, otherwise it uses C:\Users etc
rasterOptions(tmpdir = "B:/92279_Bare_peat_Duncan/R_classification")
memory.limit(size = 500000 )

############################# SET VARIABLES HERE ###################################

# Name and path for the Shapefile (don't need the .shp extension)
shapefile <- 'Training_ML5.shp'

# Approximate number of training pixels to be randomly selected for each land cover class
numsamps <- 300

# Name of the attributes that hold the integer land cover type code and the polygon id
attName <- 'ID'
polyId <- 'Poly_ID'

# No-data value for the input image
nd <- -3.39999995214e+038

# Iteration name for filenames and log file
it <- "National_Mlarea5_comparison"

# Name and path for the input satellite images
inImage<-"S2_MLarea5_indices_only.tif"

# Name and path of the output GeoTiff image
outImageName <- paste('RF_',it,'.tif',sep="")

# Name and location of the output Shapefile point file that will be created. If this output
# is not needed you can enter two double or single-quotes ("" or '')
# Note that if this file exists the write will fail with the message "Creation of output file failed"
outMarginFile <- paste('Margin_training_data',it,'.shp',sep="")

# DB - Name of output sample points used
outSamplePoints <- paste('Sample_points_',it,'.shp',sep="")

#Name of log file for outputs
logFile <- file(paste('R_log_',it,'.txt',sep=""), open ="a")

# Output classification layer without applying threshold (enter TRUE or FALSE)
classImage <- TRUE
# Output probability image layer (enter TRUE or FALSE)
probImage <- TRUE
# Output classification layer and set pixels with probability less than "probThreshold" to 0 (enter TRUE or FALSE)
threshImage <- TRUE
# Enter threshold probability in percent (values must be between 0 and 100) only used if threshImage=TRUE
probThreshold <- 70

# Layer number (band number) for the X and Y axis of the feature space plot.
# If you do not want to calculate a feature plot enter 0 as the layer number
xBand <- 0
yBand <- 0
#######################################################################################

# Start processing
cat("Pixel based classification for",it,"\n","\n", file = logFile)
startTime <- Sys.time()
cat("Start time", format(startTime),"\n")
cat("Start time", format(startTime),"\n", file = logFile)

# Read the Shapefile
vec <- readOGR(shapefile)

# Load the image then flag all no-data values (nd) so they are not processed
#DB - to speed process just load in 1 band for now.
satImageBand1 <- raster(inImage, band=1)
satImageBand1@file@nodatavalue <- nd 

# Create vector of unique land cover attribute values
# DB - returns the data component, as a dataframe, of the SpatialPolygonsDataFrame
allAtt <- slot(vec, "data")

#DB -  create a table with the number of occurrences per habitat class
tabAtt <- table(allAtt[[attName]]) 

# Check if there are data for each class - na means not available
uniqueAtt <- as.character(names(tabAtt))

if (is.na(uniqueAtt[1])) {
  cat("\n*************************No attributes were found**************************** \n")
  stop("Check the attName variable in the variable settings\n", call.=FALSE)
}

# Create input data from a Shapefile by sampling training data
cat("Create training data by sampling", numsamps, "pixels for each class\n", file = logFile)

# DB - first speed up process by clipping input raster for training data to extent and geometry of shapefile
# cropping and then masking is 5x quicker than vice versa

cropRaster <- crop (x=satImageBand1, y=extent(vec))
maskRaster <- mask(cropRaster, vec, inverse = FALSE)

# DB - Grenerate points for raster cell centres into a SpatialPointDataFrame
cat("Creating centroids from raster pixels","\n")
rasterPttrain <- rasterToPoints(maskRaster[[1]], spatial = TRUE)
rasterPttrain@proj4string <- vec@proj4string

# DB - retrieve polygon ids and tidy training dataset
# use over again to return a data frame containing polygon id only
polyid.df <- over(rasterPttrain, vec)[[polyId]]
polyid.spdf <- cbind(rasterPttrain, polyid.df)
# rename second column to polyId as it has a long meaningless name!
colnames(polyid.spdf@data)[2] = polyId
# remove the first column that contains the value of the first raster in the stack
rasterPttrain <- polyid.spdf[,-1]

# Create dataset of training sample points
for (x in 1:length(uniqueAtt)) {
  cat("Processing class", x ," of", length(uniqueAtt), "\n")
  # Get the metadata for all polygons for a particular class (based on the uniqueAtt variable)
  class_data <- vec[vec[[attName]]==uniqueAtt[x],]
  # Get the area of each polygon for a particular class
  areas <- sapply(slot(class_data, "polygons"), slot, "area")
  # Calculate the number of samples for each polygon based on the area in proportion to total area for a class
  nsamps <- ceiling(numsamps*(areas/sum(areas)))
  
  # Use random sampling to select training points (proportial based on area) from each polygon for a given class
  for (i in 1:dim(class_data)[1]) {
    
    # DB - select points that are within the polygon
    cat("Selecting points from habitat ", x," polygon ", i, "\r")
    polyPoints <- rasterPttrain[!is.na(over(rasterPttrain, class_data[i,])[[attName]]),]
    
    # DB - then select an appropriate number of those points - don't duplicate points unless sample size is greater than the number of potential sample points
    if (nsamps[i] > length(polyPoints)) {
      # get all the points
      xy_class_all <- polyPoints
      # get extra set of duplicate points chosen at random to make up the numbers - replace is TRUE in case number of samples > 2x number of points
      xy_class_extra <- polyPoints[sample(1:length(polyPoints), replace = TRUE, size = (nsamps[i] - length(polyPoints))),]
      xy_class <- rbind(xy_class_all, xy_class_extra)
      
    } else {
      xy_class <- polyPoints[sample(1:length(polyPoints), replace = FALSE, size = nsamps[i]),]
      
    }
    # Add coordinates to create a list of random points for all polygons
    if (i == 1) cpts <- xy_class
    else cpts <- rbind(cpts, xy_class)
  }
  # The number of points might not match numsamps exactly.
  # DB - it is often more because ceiling rounds up!
  classpts <- cpts
  if (x == 1) {
    xy_allClasses<- classpts
  } else {
    xy_allClasses<- rbind(xy_allClasses, classpts)
  }
}

#DB - EXPORT SAMPLED COORDINATES TO A SHAPEFILE
# It's only possible to export a SpatialPointsDataFrame whereas xy_allClasses is a 
# spatial point object.  The data frame is  created from the coordinates and the data which contains polyId
xyDataFrame <- cbind(as.data.frame(xy_allClasses@coords), as.data.frame(xy_allClasses@data))
xySpatialDataFrame <- SpatialPointsDataFrame(xy_allClasses,xyDataFrame)
writeOGR(xySpatialDataFrame, 
         outSamplePoints,
         "layer",
         driver="ESRI Shapefile",
         overwrite=TRUE)

# Get class code for each sample point from the polygon it falls in for response variable
response <- over(xy_allClasses, vec)[[attName]]

# DB - Load in full image now and get pixel DNs for each sample point
# reduce processing time by creating brick rather than stack
satImage <- brick(inImage)
satImage@file@nodatavalue <- nd

cat("Extracting values from the raster","\n")
trainvals <- cbind(response,xy_allClasses@data$Poly_ID, extract(satImage, xy_allClasses))
colnames(trainvals)[2] = polyId
cat("Number of training points = ",nrow(trainvals),"\n", file = logFile)

# Remove NA values
trainvals <- na.omit(trainvals)
cat("Number of training points after no data values removed =", nrow(trainvals),"\n", file = logFile)

# Check to make sure Shapefile and input image are in the same projection
if (nrow(trainvals) == 0) {
  cat("\n*************************No training data found**************************** \n")
  stop("It is possible the projection of the Shapefile with training data and input image are different\nCheck projections and run again", call.=FALSE)
}

# Run Random Forest in Caret package for cross validation, testing against 20% of the training data and optionally tuning the mtry parameter
set.seed(123)
dataset <- as.data.frame(trainvals)

# Hold back 20% for testing  - creates a stratified random sample (as long as response is a factor) of the data into traing and test datasets
train_index <- createDataPartition(as.factor(dataset$response), p=0.8, list = FALSE)
training_data <-dataset[train_index,]
test_data <- dataset[-train_index,]

# Do cross fold validation on the 80% training dataset
startTime <- Sys.time()
cat("Starting cross-fold validation at",format(startTime), "\n", file = logFile)

# use groupKfolds to ensure that pixels from any training polygon are retained within the same fold of the data.
# This will make accuracy predictions a bit more realistic - i.e. can we predict the habitat in a geographically separate
# polygon rather than another pixel within the same polygon.
group_folds <- groupKFold(training_data$Poly_ID, k = 10)
train_control <- trainControl(index = group_folds, method="cv", search = "grid")

# reduce the data to only the predictor variables
training_data_predictors <- select(training_data, - Poly_ID, - response)

# Optionally create a grid of mtry parameters to tune but the default, the sqrt of the number of columns is usually ok
# and is what the random forest package would use.  It needs to be set explicitly here though as 'train' would by default use
# 3 parameters which are unlikely to be near the default value!!
# e.g. rf_grid <- expand.grid(mtry= seq(1,ncol(training_data_predictors), by=10))

# set mtry to the sqrt of the number of predictors
mtry <- sqrt(ncol(training_data_predictors))
rf_grid <- expand.grid(.mtry=mtry)

# Train the model - as.factor ensures classification rather than regression rf
rf_model <- train(as.factor(response)~., data = select(training_data, - polyId), method="rf", trControl=train_control, tuneGrid = rf_grid, na.action=na.omit)
capture.output(rf_model, file = logFile, append = TRUE)
capture.output(rf_model$finalModel, file = logFile, append = TRUE)

endTime <- Sys.time()
cat("Finished cross-fold validation at",format(endTime), "\n", file = logFile)

# test on the 20% left out of training and print confusion matrix
cat("Test data prediction", "\n", file = logFile)
test_pred <- predict(rf_model, test_data)
responseFactor <- as.factor(test_data$response)
capture.output(confusionMatrix(test_pred, responseFactor), file = logFile, append = TRUE)

# Plotting variable importance plot
importance<-varImp(rf_model, scale=FALSE, n.var=10)
plot(importance)

# rebuild the random forest model using all the training data.
# NB the caret implementation cannot be used here as it fails to handle the no data parts of the raster.
randfor <- randomForest(as.factor(response) ~., data= select(dataset, - polyId), importance=TRUE, na.action=na.omit)

# Start predictions
cat("Predicting unknown data","\n")
# Calculate how many bands the output image should have
numBands <- classImage + probImage + threshImage
# Calculate the image block size for processing
bs <- blockSize(satImage)
# Create the output raster block DB - a raster brick is a multilayer raster which processes facter than a RasterStack
outImage <- brick(satImage, values=FALSE, nl=numBands)
outImage <- writeStart(outImage, filename=outImageName, progress='text', format='GTiff', datatype='INT1U', overwrite=TRUE)
# Loop though each of the image blocks to calculate the output layers selected in the variables section
for (i in 1:bs$n) {
  cat("processing block", i, "of", bs$n, "\n")
  imageBlock <- getValuesBlock(satImage, row=bs$row[i], nrows=bs$nrows[i])
  predValues <- predict(randfor, imageBlock, type='response')
  classValues <- as.numeric(levels(predValues))[predValues]
  outMatrix <- matrix(nrow=nrow(imageBlock), ncol=0)
  if (classImage) {
    outMatrix <- cbind(outMatrix, classValues)
  }
  if (probImage || threshImage) {
    predProbs <- as.data.frame(predict(randfor, imageBlock, type='prob'))
    maxProb <- round(apply(predProbs, 1, max) * 100)
    if (probImage) {
      outMatrix <- cbind(outMatrix, maxProb)
    }
    if (threshImage) {
      threshValues <- classValues
      threshValues[which(maxProb <= probThreshold)] <- 0
      outMatrix <- cbind(outMatrix,threshValues)
    }
  }
  writeValues(outImage, outMatrix, bs$row[i])
}
# Stop writing and close the file
outImage <- writeStop(outImage)

# DB - Create 3 separate rasters from output image
writeRaster(outImage, outImageName, format='GTiff', datatype='INT1U', bylayer=TRUE, suffix='numbers', overwrite= TRUE)

# Calculate margin (proportion of votes for correct class minus maximum proportion of votes for other classes)
marginData <- randomForest::margin(randfor)
trainingAccuracy <- cbind(marginData[order(marginData)], trainvals[order(marginData),1])
#Add column names to attributes table
colnames(trainingAccuracy) <- c("margin", "classNum")
#Calculate X and Y coordinates for training data points
xyCoords <- xy_allClasses@coords
xyCoords <- xyCoords[order(marginData),]
# Create and write point Shapefile with margin information to help improve training data

pointVector <- SpatialPointsDataFrame(xyCoords, as.data.frame(trainingAccuracy), coords.nrs = numeric(0), proj4string = satImage@crs)
writeOGR(pointVector, outMarginFile, "layer", driver="ESRI Shapefile", check_exists=TRUE)

endTime <- Sys.time()
cat("End time", format(endTime),"\n", file = logFile)
close(logFile)
