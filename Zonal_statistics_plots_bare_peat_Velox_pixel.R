### Script to take in raster brick,  calculate  statistics of pixels within overlain polygon dataset
### output is csv file of values and boxplots of statistics.
### note that polys@data needs altering to run the script in one go - at the moment it is run separateley for each selection
### to allow resize of plots
### (ie run individual code lines for mean , copy boxplot , then run again for the next selection/ plot)

## check raster and polygon datsaet are in BNG and overlap

### polygon dataset needs a field with categories of interest (ID, format numeric-short) and unique id field (Poly_ID, format numeric-short)
### the ID is shown in the y label of output plots. alter text in bwplotmean if required
### currently this is set to (ylab = "BarePeat= 10, other= 20, rock_stone = 30, shadow=40, water=50"))

### an additional column needs to be added to the polygon file (hardcoded as UIDfromFID) - this needs to be calculated
### as equal to the FID, which is what the output of velox contains to identify the original polygons. 
### reading FID directly might be possible using 'special field' 

## input the name and location of the input raster at 'inputraster' variable
## format should be e.g.("K:/Analysis_projects/92279_bare_peat/data/Indices/20190826_MultipleIndices.tif")
## file raster name must be in format YYYYMMDD_MultipleIndices.tif for name of output csv files and plots to correspond to date of image

## layers should be in following order, otherwise update the 'layernames' variable:
##'blue', 'green', 'red', 'rededge5', 'rededge6', 'rededge7', 'NIR', 'NIR8A', 'SWIR1','SWIR2', 'ndvi','Gndvi' ,'Bndvi', 'Re5ndvi',
##' 'Re6ndvi', 'Re7ndvi','Re8Andvi','Re6Gndvi', 'Re7Gndvi', 'Re8AGndvi', 'NDWI', 'mDNWI', 'mNDVI', 'darkness','ratioblueSWIR',
##' 'ratioNIRgreen','ratioredSWIR1')

## 30 September 2019


#### read in packages, set parameters and paths

library(rgdal)
library(raster)
library(reshape2)
library(lattice)
library(velox)
library(stringr)
library(ggplot2)

setwd("B:/92279_Bare_peat_Duncan/Data")

# read in raster and polygon datasets

inputraster <- "S2A_20160605_clip_Indices_Extra.tif"
polys <- readOGR("Shetland_samples_confusion.shp","Shetland_samples_confusion",useC=T)

#  create name for output csv and determine date of raster
datename <- str_sub(inputraster,-31,-24)
outfilename <- paste("Zonal_stats_pixel_extra_confusion_", datename, ".csv", sep="")

# track time taken 

start_time<- Sys.time()

layernames<- c('blue', 'green', 'red', 'rededge5', 'rededge6', 'rededge7', 'NIR', 'NIR8A', 'SWIR1','SWIR2', 'ndvi','Gndvi' ,'Bndvi', 'Re5ndvi', 'Re6ndvi', 'Re7ndvi','Re8Andvi','Re6Gndvi', 'Re7Gndvi', 'Re8AGndvi', 'NDWI', 'mDNWI', 'mNDVI', 'darkness','ratioblueSWIR','ratioNIRgreen','ratioredSWIR1', 'ratioNIRSWIR1','ratioRe5SWIR1','ratioRe6SWIR1', 'ratioRe7SWIR1')

# try and speed up process by using brick - read in the raster and rename layers
layerStack <- brick(inputraster)

names(layerStack) <- layernames

# check number of layers is as expected
totrasters <- nlayers(layerStack)
print (totrasters)

###### calculate zonal statistcs (velox is at least 7X faster than extract)

###### Additional info : another way to calculate zonal statistcs is based on using getvalues rather than extract, this is  
###### meant to also be faster than extract - sample script at 
###### https://gis.stackexchange.com/questions/130522/increasing-speed-of-crop-mask-extract-raster-by-many-polygons-in-r


vx <- velox(layerStack)
extractedvalues <-vx$extract(polys, df=TRUE)

end_time <-Sys.time()
print( end_time - start_time )

# output is a df, id of polygon is the FID from shapefile, this is first (un-named) column in polys@data
# first column of output df is FID, the rest correspond to raster layers.


# rename columns of output df
dfcolumnnames <- c("polyFID", layernames)
colnames(extractedvalues) <- dfcolumnnames



####### join  shapefile data to statistics for pixels and write out new table (extract output is same order as input polygons)
# 'merge' (arcmap would call this a join) tables based on FID column
extractedvalues_classInfo <- merge(extractedvalues, polys@data, by.x= "polyFID", by.y = "UIDfromFID")
write.csv(extractedvalues_classInfo, file = outfilename)

###### if needing to start from a csv file produced previously import using the code below #######################

setwd("B:/92279_Bare_peat_Duncan/Data")
inputraster <- "S2A_20160605_clip_Indices_Extra.tif"
datename <- str_sub(inputraster,-31,-24)
outfilename <- paste("Zonal_stats_pixel_extra_confusion_", datename, ".csv", sep="")
extractedvalues_classInfo <- read.csv(outfilename)
layernames<- c('blue', 'green', 'red', 'rededge5', 'rededge6', 'rededge7', 'NIR', 'NIR8A', 'SWIR1','SWIR2', 'ndvi','Gndvi' ,'Bndvi', 'Re5ndvi', 'Re6ndvi', 'Re7ndvi','Re8Andvi','Re6Gndvi', 'Re7Gndvi', 'Re8AGndvi', 'NDWI', 'mDNWI', 'mNDVI', 'darkness','ratioblueSWIR','ratioNIRgreen','ratioredSWIR1', 'ratioNIRSWIR1','ratioRe5SWIR1','ratioRe6SWIR1', 'ratioRe7SWIR1')
#################################################################################################################


# create 'stacked' format from wide column table
categories <- "Id"
plottable <- melt(extractedvalues_classInfo,id.vars=c("polyFID",categories), measure.vars= layernames)

# create 'vertical' boxpolot with all layers
# only output subset of data - for viewing at scales that are comparable. 


selection1 <- c("blue", "green", "red", "rededge5", "rededge6", "rededge7", "SWIR1", "SWIR2" )
selection2<- c("ndvi","Gndvi", "Bndvi", "Re5ndvi", "Re6ndvi", "Re6Gndvi", "Re7ndvi", "Re8Andvi", "Re8AGndvi", "Re7Gndvi")
selection3<- c("NDWI", "mDNWI", "mNDVI")
selection4<- c('darkness', "NIR","NIR8A")
selection5 <- c('ratioblueSWIR','ratioNIRgreen','ratioredSWIR1')
selection6 <- c('ratioblueSWIR','ratioredSWIR1')
selection7 <- c('ratioNIRgreen')
ratioShortlist <- c("Re6ndvi", "Re7ndvi","mDNWI", "ratioblueSWIR")
extraRatioList <- c("ratioNIRSWIR1","ratioRe5SWIR1","ratioRe6SWIR1", "ratioRe7SWIR1")

###############################################################################################################################
####XXXXXXXXXMANUALLY RUN THE NEXT SECTION OF CODE ONCE FOR EACH PLOT , UPDATING THE SELECTION IN 'tableselected' #############
####EACH TIME, EG RUN FIRST ON selection1, THEN REPLACE WITH selection2. THIS ALLOWS MANUAL RE-SIZE OF OUTPUT PLOTS############
###############################################################################################################################
tableselected <- plottable[plottable$variable %in% selection1,]
# change to factor to allow colouring of bars
tableselected$Id <- as.factor(tableselected$Id)

#use ggplot as easier to change colours. can amend to include 5% and 95% percentile, but stuck with default
#https://stackoverflow.com/questions/4765482/changing-whisker-definition-in-geom-boxplot

minaxis <- round_any(min(tableselected$value), 10)
maxaxis <- round_any(max(tableselected$value),10, f = ceiling)

plot1 <- ggplot(tableselected, aes(x = paste(variable,Id), y = value, fill=Id)) + 
  geom_boxplot() + 
  labs(title=paste("Sentinel 2 - ",datename),x="Peat= 10, other= 20, rock= 30, shadow=40, water=50", y = "Value") +
  scale_y_continuous(breaks = seq(minaxis,maxaxis, by = 0.2))+
  coord_flip() 
plot1

# print out stats on selected indices for bare peat to get hinge and whisker values
index <- "darkness"
focuslayerstatsBO <- plottable[(plottable$variable == index) & (plottable$Id == 10),]
boxplot.stats(focuslayerstatsBO$value)


