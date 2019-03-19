# 03_satellite imagery analysis intro.
# Mike Shewring
# 24 1 19

# install.packages("sf")
# install.packages('landsat8')
# install.packages("landsat")

library(gdalUtils) 
library(raster)
library(rasterVis)
library(dplyr)
library(tidyr)
library(sf)
library(MODIS) # access modis sat data ~250m resolution
library(RStoolbox) # multiple functions designed to aid in RS image processing
library(landsat) # access landsat data
library(MODIStools) 
library(hsdar)
library(cluster)
library(landsat8) # access landsat 8 data - 30m resolution

# Satellite data - ####
# Sentinel 2 data. 10m resolution. Passes every 7 days.
# see https://sentinel.esa.int/web/sentinel/sentinel-data-access
# files come as J2 files types. need to convert these to GTiff. The code below will do this using gdal.

# process images to tiffs ####
#images=list.files('filepath', full.names=T)
#for (file in images) { 
#out_file <- extension(file, 'tif') 
#gdal_translate(src_dataset = file, dst_dataset = out_file, ot = 
#                 "UInt16", of = "GTiff") }

# explore tiff files ####
#list processed tif files
getwd()
layers<-list.files('sentinel_data', full.names = T)
layers 
#Sentinel-2 bands	Sentinel-2A	Sentinel-2B	
#band and resolution
# TCI - True colour image - 10m
#Band 1 - Coastal aerosol-	60m
#Band 2 -Blue - 10m
#Band 3 - Green	-	10m
#Band 4 - Red- 10m
#Band 5 - Vegetation red edge	- 20m
#Band 6 - Vegetation red edge	- 20m
#Band 7 - Vegetation red edge	 -20m
#Band 8 - NIR-10m
#Band 8A - Narrow NIR	- 20m
#Band 9 - Water vapour	- 60m
#Band 10 - SWIR	60m
#Band 11 - SWIR	-20m
#Band 12 - SWIR	20m

# TCI is true colour image others are different bands from the satellite image. Bands 2,3,4 are B,G,R respectively so used to make RGB images. Bands 5,6,7, 8, 8a are infra red and red edge bands useful for vegetation indices etc
# https://sentinels.copernicus.eu/web/sentinel/user-guides/sentinel-2-msi/resolutions/spatial
# let select the bands we want ###
sat1<-raster(layers[3]) #Band 2 Blue - 10m
sat2<-raster(layers[4]) #Band 3 Green	-	10m
sat3<-raster(layers[5]) #Band 4 red- 10m
sat4<-raster(layers[9]) #Band 8 NIR-10m

# creat RGB
satrgb<-stack(sat1,sat2,sat4)
satrgb

# plot as RGB image
plotRGB(satrgb,r=3,g=2,b=1, stretch = "lin", axes=T)
# stretch	= character. Option to stretch the values to increase the contrast of the image: "lin" or "hist"

# As data comes in tiles for regions multplie tiles are needed to cover larger regions. This can be done using the mosaic command from the raster package.

# mosaic
# Merge Raster* Objects Using A Function For Overlapping Areas i.e. mean, median etc
#Mosaic Raster* objects to form a new object with a larger spatial extent. A function is used to compute cell values in areas where layers overlap (in contrast to the merge function which uses the values of the 'upper' layer). All objects must have the same origin, resolution, and coordinate reference system. Impiortant to note the point about origin. To mosaic you will to create a new raster for each data source that encompasses the areas on the relevant tiles you want. a good example here - https://www.nceas.ucsb.edu/scicomp/usecases/createrasterimagemosaic

# However, its much easier and quicket to obtain imagery mosaics from Google earth engine so lets work with some data from GEE instead

# 
## GEE data ####
# GEE provides a great way to process satellite imagery and obtain mosaics for your areas of interest.
# its also a very powerful analysis tool...

# GEE data analysis ###
# this is S2 data downloaded using the GEE
l<-list.files("GEE_S2_data", full.names = T, pattern = 'tif')
l
# lets load in PyC 2018 data - this is data from the pen y cymoedd windfarm and neath valley area of south wales collected between may and august 2018
rast<-brick(l[1])

# inspect it
str(rast)

# get number of layers names - these are non-informative following download from GEE but follow the layer order as set out in GEE var
rast@data@nlayers
rast@data@names

# change names to correct layer vars
names(rast) <- c('B1', 'B2', 'B3','B4','B5','B6','B7','B8','B8a', 'B9','B10','B11','B12','cloudMask', 'MSAVI','BSI','NVDI')

#subset raster to keep the bands we want
rast<-rast[[2:9]]
#check this worked
rast

# stack RGB layers and plot to make a colour image
RGB<-stack(rast[[1]],rast[[2]],rast[[3]])
RGB
plotRGB(RGB,r=3,g=2,b=1, stretch = "lin", axes=T)

# change to BNG using project raster if your data are in a different CRS you will need to reproject the raster to use extract
#OSRGB<-projectRaster(RGB, res=10, crs='+init=epsg:27700')
#OSRGB
#plotRGB(OSRGB,r=3,g=2,b=1, stretch = "lin", axes=T)

# create false colour composite with NIR band.  use of bands, such as near infrared, increases spectral separation and can enhance the interpretability of data i.e. pull out vegetation
nRGB<-stack(rast[[1]],rast[[2]],rast[[7]])
nRGB
plotRGB(nRGB,r=3,g=2,b=1, stretch = "lin", axes=T)
#pairs(nRGB) = can be useful to review data

# calculating NDVI ####
# normalized difference vegetation index (NDVI) is a numerical indicator that uses the visible (RGB/ S2 - B2,B3,B4) and near-infrared bands (NIR/S2-B8) of the electromagnetic spectrum, and is adopted to analyze remote sensing measurements and assess whether the target being observed contains live green vegetation or not. Photosynthetically active vegetation, in particular, absorbs most of the red light that hits it while reflecting much of the near infrared light
# NDVI = (NIR - RED) / (NIR + RED), where RED is B4 and NIR is B8
# Values of 1 show vegetation, 0 bare and -1 water
ndvi <- overlay(rast[[7]], rast[[3]], fun=function(x,y){(x-y)/(x+y)})
plot(ndvi)

# we can then use this to extract those areas that are definetly vegetation i.e. >0.4 ndvi
veg <- reclassify(ndvi, cbind(-Inf, 0.4, NA)) # usually anything >0.4 = vegetation
plot(veg, main = 'Veg cover')
rm(veg)

# MSAVI ####
# modified soil-adjusted vegetation index (MSAVI) and MSAVI2, are soil adjusted vegetation indices that address some of the limitations of NDVI when applied to areas with a high degree of exposed soil surface.
#Light from the soil surface can influence the NDVI values by a large degree-  concern in any areas with high % of bare ground and/or exposed rock . Heute and Jackson (1988) found that the soil surface impact on NDVI values was greatest in areas with between 45% and 70% vegetative cover.  
# MSAVI = (2*NIR+1-√((2*NIR+1)^2 - 8*(NIR-RED)))/2 
# MSAVI2 = (B08 + 1) - 0.5 * sqrt((2 * B08 - 1) ^ 2 + 8 * B04)) other Formula
# http://wiki.landscapetoolbox.org/doku.php/remote_sensing_methods:modified_soil-adjusted_vegetation_index
MSAVI <- overlay(rast[[7]], rast[[3]], fun=function(x,y){(x+1)-0.5*sqrt((2*x-1)^2+8*y)})
plot(MSAVI)


# view hist of ndvi and MSAVI
par(mfrow=c(1,2))
hist(ndvi,
     main = "Distribution of NDVI values",
     xlab = "NDVI",
     ylab= "Frequency",
     col = "wheat",
     xlim = c(-1, 1),
     breaks = 50,
     xaxt = 'n')
axis(side=1, at = seq(-1,1, 0.1), labels = seq(-1,1, 0.1))
hist(MSAVI,
     main = "Distribution of MSAVI values",
     xlab = "MSAVI",
     ylab= "Frequency",
     col = "wheat",
     xlim = c(-1, 1),
     breaks = 50,
     xaxt = 'n')
axis(side=1, at = seq(-1,1, 0.1), labels = seq(-1,1, 0.1))
par(mfrow=c(1,1))

# Random Forest supervised classification ####
# In supervised classification, we have prior knowledge about some of the land-cover types i.e. through fieldwork, reference spatial data or interpretation of high resolution imagery . Specific sites in the study area that represent homogeneous examples of these known land-cover types are identified. These  training sites and their spectral properties are used to train the classification algorithm.

# general workflow follows -
# 1. Generate sample sites based on a prior knowledge
# 2. Extract cell values from satellite data for the sample sites
# 3. Train the classifier using training samples
# 4. Classify the satellite data using the trained model
# 5. Evaluate the accuracy of the model

# 1. load prepared training data polys
pcw_train<-st_read("GEE_S2_data/PCW_train.shp")
open_train<-st_read("GEE_S2_data/Open_train.shp")

# 2. Data extract
# first generate random points from within our polys to extract training data from satellite images for known habitats. This will form our training data set
# using 500 points here to try and speed things up. You probably want to use more than this
samp_pcw<- st_sample(pcw_train, 500, type = "random")
dat=data.frame(ID=1:length(samp_pcw), class="1")
samp_pcw<-st_sfc(samp_pcw)%>%st_sf(cbind(dat, samp_pcw))
samp_pcw$geometry=NULL

samp_open<- st_sample(open_train, 500, type = "random")
dat=data.frame(ID=1:length(samp_open), class="0")
samp_open<-st_sfc(samp_open)%>%st_sf(cbind(dat, samp_open)) # 
samp_open$geometry=NULL

# bind training points into single df to extract from raster
samp_habs<-rbind(samp_pcw, samp_open)

#clean up
rm(samp_open, samp_pcw)

# select bands to use for the classification
rast1<-stack(rast[[1:3]],rast[[7]], MSAVI, ndvi) # selecting RGB, NIR, MSAVI and NDVI
names(rast1) <- c('B2', 'B3','B4','B8','MSAVI','NDVI')
rast1

# Extract the layer values for the sample locations. This is the data matrix that will be used to train the RF model.
# use `sp = TRUE` argument in `extract` function to retain spatial information
trainvals <- raster::extract(rast1, samp_habs, df = TRUE, sp=T)%>%st_as_sf()

# drop the ID column
trainvals <- trainvals[, -1]
head(trainvals)

#3. Lets train our classifier - lost of options for image classification. SVM and RF are often used due to their good performance
# load modelling packages and snow to allow use of multicores should we need to speed things up
library(snow)
library(caret)
library(randomForest)

# Two packages that implement RF in R, Caret and randomForest. Both allow tuning of critical parameters and caret has nice comparison features to allow this but it does not allow variation of ntree without some wrapping code.
# need to experiment with your data and identify what tuning parameters suit your data best.
# More details on this function can be found at http://topepo.github.io/caret/model-training-and-tuning.html.

# mtry: Number of variables randomly sampled as candidates at each split.
# ntree: Number of trees to grow.
# recommend defaults = mtry=floor(sqrt(ncol(x))) and ntree=500.
# fit RF in Caret using default setup and repeated cross validation sampling.
# due to the randomisation in RF if you want to repat and tune parameters it is crucial you set the seed prior to doing this.
# ntree - can also influence output
control <- trainControl(method="repeatedcv", number=10, repeats=3) #?train control for many options
seed <- 7
metric <- "Accuracy"
set.seed(seed)
mtry <- sqrt((ncol(trainvals)-1))
tunegrid <- expand.grid(.mtry=mtry)
# tunegrid <- expand.grid(.mtry=c(1:15), .ntree=c(1000, 1500, 2000, 2500))
modFit_rf <- train(as.factor(class) ~ B2 + B3 + B4 + B8 + MSAVI + NDVI, method = "rf", data = trainvals,metric=metric, tuneGrid=tunegrid, trControl=control)
print(modFit_rf) #check accuracy and kappa value
?train
# Kappa statistic (or value) is a metric that compares an Observed Accuracy with an Expected Accuracy (random chance)

# lets test against training data
# first predict to using our model against our training data
rf_test <-   predict(modFit_rf,
                     trainvals)

# then calculate a confusion matrix showing accuracy of the prediction for each point
rf_cm<-confusionMatrix(data=rf_test,
                       reference=trainvals$class)
print(rf_cm)

# now lets predict to our region- use cluster to speed things up
beginCluster()
preds_rf <- clusterR(rast1, raster::predict, args = list(model = modFit_rf))
endCluster()
preds_rf<-ratify(preds_rf)

# lets view alongside RGB image
par(mfrow=c(1,2))
plot(preds_rf)
plotRGB(RGB,r=3,g=2,b=1, stretch = "lin", axes=T)
par(mfrow=c(1,1))
# unsupervised raster classification using kmeans#####
# k-means clustering is a method of vector quantization, originally from signal processing, that is popular for cluster analysis. k-means clustering aims to partition n observations into k clusters in which each observation belongs to the cluster with the nearest mean, serving as a prototype of the cluster. This results in a partitioning of the data space into Voronoi cells.]
# K-Means calculates initial class means evenly distributed in the data space then iteratively clusters the pixels into the nearest class using a minimum distance technique. Each iteration recalculates class means and reclassifies pixels with respect to the new means. All pixels are classified to the nearest class unless a standard deviation or distance threshold is specified, in which case some pixels may be unclassified if they do not meet the selected criteria

## get the values of the raster dataset and write them in a matrix. 
v <- getValues(rast)
i <- which(!is.na(v))
v <- na.omit(v) # need to get rid of NA's as this breaks the clustering process

## kmeans classification  ####
# k-means is a clustering method. It separates n observations into  k clusters. Each observation belongs to the cluster with the nearest mean.
# formula in R = kmeans(x, centers, iter.max = 10, nstart = 1, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen"), trace=FALSE)
# x =	numeric matrix of data, or an object that can be coerced to such a matrix (such as a numeric vector or a data frame with all numeric columns).
# centers	= either the number of clusters, say k, or a set of initial (distinct) cluster centres. If a number, a random set of (distinct) rows in x is chosen as the initial centres.
# iter.max = the maximum number of iterations allowed.
# nstart = if centers is a number, how many random sets should be chosen?
# algorithm =	character: may be abbreviated.
# method =	character: may be abbreviated. "centers" causes fitted to return cluster centers (one for each input point) and "classes" causes fitted to return a vector of class assignments.
# trace	= logical or integer number, currently only used in the default method ("Hartigan-Wong"): if positive (or true), tracing information on the progress of the algorithm is produced. Higher values may produce more tracing information.
# picking K or expected number of clusters is tricky...# see this post for some more detail https://www.r-bloggers.com/finding-optimal-number-of-clusters/
E <- kmeans(v, 3, iter.max = 500, nstart = 10)
E #note the sum of squares by cluster this is = (between_SS / total_SS). This ratio accounts for the amount of total sum of squares of the data points which are between the clusters.

# creat empty raster to hold the values
kmeans_raster <- raster(rast)
# assign values from the cluster to teh empty raster
kmeans_raster[i] <- E$cluster
kmeans_raster<-ratify(kmeans_raster) # convert to factor in raster package
# plot the new raster with RGB
par(mfrow=c(1,2))
plot(kmeans_raster)
plotRGB(RGB,r=3,g=2,b=1, stretch = "lin", axes=T)
par(mfrow=c(1,1))

## clara kmeans- classification ####
# Clustering for Large Applications works by clustering only a sample of the dataset and then assigns all object in the dataset to the clusters
?clara
clus <- clara(v,6,samples=500,metric="manhattan",pamLike=T)
clus
clara_raster <- raster(rast1)
clara_raster[i] <- clus$clustering
clara
# plot
par(mfrow=c(1,2))
plot(clara_raster)
plotRGB(RGB,r=3,g=2,b=1, stretch = "lin", axes=T)
par(mfrow=c(1,1))
filter
# GEE tutorials ####
# https://geohackweek.github.io/GoogleEarthEngine/03-load-imagery/
# https://blog.webkid.io/analysing-satellite-images-with-google-earth-engine/
# https://geohackweek.github.io/GoogleEarthEngine/05-classify-imagery/
# https://geoscripting-wur.github.io/Earth_Engine/
# https://geohackweek.github.io/GEE-Python-API/02-feature-and-raster-data/

# satellite imagery tutorials using R ####
# https://rspatial.org/rs/3-basicmath.html
# https://geoscripting-wur.github.io/AdvancedRasterAnalysis/#WUR_Geoscripting_
# https://shekeine.github.io/visualization/2014/09/27/sfcc_rgb_in_R
# https://geoscripting-wur.github.io/IntroToRaster/
# https://www.r-bloggers.com/deep-learning-in-satellite-imagery/
# https://appsilon.com/ship-recognition-in-satellite-imagery-part-i/
# http://remote-sensing.eu/unsupervised-classification-with-r/

# Finds Bare Soil Line (BSL) and maximum vegetation point.
red<-sampleRandom(rast[[3]], 10000)
NIR<-sampleRandom(rast[[4]], 10000)
plot(red~NIR)
nbsl <- BSL(red, NIR)
plot(as.vector(as.matrix(red)), as.vector(as.matrix(NIR)))
abline(nbsl$BSL, col="red")
points(nbsl$top[1], nbsl$top[2], col="green", cex=2, pch=16)