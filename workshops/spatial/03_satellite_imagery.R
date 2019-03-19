#03_satellite imagery analysis intro.
# Mike Shewring
# 14 1 19
install.packages("sf")
install.packages('landsat8')
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
# files come as J2 files types. need to convert these to GTiff. This code below will do this.

# process images to tiffs ####
length(getOption("gdalUtils_gdalPath"))
images=list.files('filepath', full.names=T)

for (file in images) { 
out_file <- extension(file, 'tif') 
gdal_translate(src_dataset = file, dst_dataset = out_file, ot = 
                 "UInt16", of = "GTiff") }

# explore tiff files ####
#list processed tif files
layers<-list.files('raster/raster/sentinel_data', full.names = T)
layers 
#Sentinel-2 bands	Sentinel-2A	Sentinel-2B	
#band and resolution
#Band 1 – Coastal aerosol-	60m
#Band 2 – Blue - 10m
#Band 3 – Green	-	10m
#Band 4 – Red- 10m
#Band 5 – Vegetation red edge	- 20m
#Band 6 – Vegetation red edge	- 20m
#Band 7 – Vegetation red edge	 -20m
#Band 8 – NIR-10m
#Band 8A – Narrow NIR	- 20m
#Band 9 – Water vapour	- 60m
#Band 10 – SWIR –	60m
#Band 11 – SWIR	-20m
#Band 12 – SWIR	20m

# TCI is true colour image others are different bands from the satellite image. bands 2,3,4 are B,G,R respectively so used to make RGB images. Bands 5,6,7, 8, 8a are infra red and red edge bands useful for vegetation indices etc
# https://sentinels.copernicus.eu/web/sentinel/user-guides/sentinel-2-msi/resolutions/spatial
# lets subset to the bands we want ###
layers1<-layers[3:10]
sat1<-raster(layers1[1]) #Band 2 – Blue - 10m
sat2<-raster(layers1[2]) #Band 3 – Green	-	10m
sat3<-raster(layers1[3]) #Band 4 – Red- 10m
sat4<-raster(layers1[4]) #Band 5 – Vegetation red edge	- 20m
sat4<-raster(layers1[5]) #Band 6 – Vegetation red edge	- 20m
sat5<-raster(layers1[6]) #Band 7 – Vegetation red edge	- 20m
sat7<-raster(layers1[7]) #Band 8 – NIR-10m
sat8<-raster(layers1[8]) #Band 8A – Narrow NIR	- 20m
sat<-stack(sat1,sat2,sat3,sat7)
satrgb<-stack(sat1,sat2,sat3)
satrgb

# plot as RGB image
plotRGB(satrgb,r=3,g=2,b=1, stretch = "hist", axes=T)

# as data comes in tiles for regions multplie tiled will be needed to cover larger regions. This can be done using the mosaic command from the raster package.

# mosaic
# Merge Raster* Objects Using A Function For Overlapping Areas i.e. mean, median etc
#Mosaic Raster* objects to form a new object with a larger spatial extent. A function is used to compute cell values in areas where layers overlap (in contrast to the merge function which uses the values of the 'upper' layer). All objects must have the same origin, resolution, and coordinate reference system.

# satellite imagery tutorials
# https://rspatial.org/rs/3-basicmath.html
# https://geoscripting-wur.github.io/AdvancedRasterAnalysis/#WUR_Geoscripting_
# https://shekeine.github.io/visualization/2014/09/27/sfcc_rgb_in_R
# https://geoscripting-wur.github.io/IntroToRaster/
# https://www.r-bloggers.com/deep-learning-in-satellite-imagery/
# https://appsilon.com/ship-recognition-in-satellite-imagery-part-i/
# http://remote-sensing.eu/unsupervised-classification-with-r/
# 
## GEE ####
# GEE provides a great way to process satellite imagery and obtain mosaics for your areas of interest.
# its also a very powerful analysis tool...
# https://geohackweek.github.io/GoogleEarthEngine/03-load-imagery/
# https://blog.webkid.io/analysing-satellite-images-with-google-earth-engine/
# https://geohackweek.github.io/GoogleEarthEngine/05-classify-imagery/
# https://geoscripting-wur.github.io/Earth_Engine/
# https://geohackweek.github.io/GEE-Python-API/02-feature-and-raster-data/

# GEE data analysis ####
# this is S2 data downloaded using the GEE
l<-list.files("C:/Users/C1751157/Documents/PhD/workshops/spatial/raster/GEE", full.names = T, pattern = 'tif')
l
# lets load in PyC 2018 data
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

# stack RGB and plot
RGB<-stack(rast[[1]],rast[[2]],rast[[3]])
RGB
plotRGB(RGB,r=3,g=2,b=1, stretch = "lin", axes=T)

#hist of visible spectrum
par(mfrow=c(1,3))
hist(RGB,lim = c(0, 5000), ylim = c(0, 20000), breaks = seq(-0.2, 0.4, by = 0.005))
par(mfrow=c(1,1))

# change to BNG using project raster
OSRGB<-projectRaster(RGB, res=10, crs='+init=epsg:27700')
OSRGB
plotRGB(OSRGB,r=3,g=2,b=1, stretch = "lin", axes=T)

# create false colour composite with NIR
nRGB<-stack(rast[[1]],rast[[2]],rast[[7]])
nRGB
plotRGB(nRGB,r=3,g=2,b=1, stretch = "lin", axes=T)
#pairs(nRGB)

# NDVI = (NIR - RED) / (NIR + RED), where RED is B4 and NIR is B8
# Values of 1 show vegetation, 0 bare and -1 water
ndvi <- overlay(rast[[7]], rast[[3]], fun=function(x,y){(x-y)/(x+y)})
plot(ndvi)
veg <- reclassify(ndvi, cbind(-Inf, 0.4, NA)) # usually anything >0.4 = vegetation
plot(veg, main = 'Veg cover')
rm(veg)

# MSAVI = (2*NIR+1-√((2*NIR+1)^2 - 8*(NIR-RED)))/2 
# MSAVI2 = (B08 + 1) - 0.5 * sqrt((2 * B08 - 1) ^ 2 + 8 * B04)) other Formula
# http://wiki.landscapetoolbox.org/doku.php/remote_sensing_methods:modified_soil-adjusted_vegetation_index
MSAVI <- overlay(rast[[7]], rast[[3]], fun=function(x,y){(x+1)-0.5*sqrt((2*x-1)^2+8*y)})
plot(MSAVI)

# view hist of ndvi
hist(ndvi,
     main = "Distribution of NDVI values",
     xlab = "NDVI",
     ylab= "Frequency",
     col = "wheat",
     xlim = c(-1, 1),
     breaks = 50,
     xaxt = 'n')
axis(side=1, at = seq(-1,1, 0.1), labels = seq(-1,1, 0.1))

# unsupervised raster classification #####
# pick the bands we want to use. here i have selected RGB, NIR, MSAVI and NDVI
rast1<-stack(rast[[1:3]],rast[[7]], MSAVI, ndvi)

## get the values of the raster dataset and write them in a matrix. 
v <- getValues(rast1)
i <- which(!is.na(v))
v <- na.omit(v) # need to get rid of NA's as this breaks the clustering process

## kmeans classification  ####
# k-means method separates n observations into  k clusters. Each observation belongs to the cluster with the nearest mean.
# kmeans(x, centers, iter.max = 10, nstart = 1, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen"), trace=FALSE)

# x =	numeric matrix of data, or an object that can be coerced to such a matrix (such as a numeric vector or a data frame with all numeric columns).

# centers	= either the number of clusters, say k, or a set of initial (distinct) cluster centres. If a number, a random set of (distinct) rows in x is chosen as the initial centres.
# iter.max = the maximum number of iterations allowed.
# nstart = if centers is a number, how many random sets should be chosen?
# algorithm =	character: may be abbreviated.
# method =	character: may be abbreviated. "centers" causes fitted to return cluster centers (one for each input point) and "classes" causes fitted to return a vector of class assignments.
# trace	= logical or integer number, currently only used in the default method ("Hartigan-Wong"): if positive (or true), tracing information on the progress of the algorithm is produced. Higher values may produce more tracing information.

E <- kmeans(v, 5, iter.max = 500, nstart = 10)

# creat empty raster to hold the values
kmeans_raster <- raster(rast1)
# assign values from the cluster to teh empty raster
kmeans_raster[i] <- E$cluster
# plot the new raster
plot(kmeans_raster)



## clara classification ####
# Clustering for Large Applications works by clustering only a sample of the dataset and then assigns all object in the dataset to the clusters
?clara
clus <- clara(v,5,samples=500,metric="manhattan",pamLike=T)
clara_raster <- raster(rast1)
clara_raster[i] <- clus$clustering
plot(clara_raster)


# Random Forest supervised classification
pcw_train<-st_read("C:/Users/C1751157/Documents/PhD/workshops/spatial/raster/GEE/PCW_train.shp")
open_train<-st_read("C:/Users/C1751157/Documents/PhD/workshops/spatial/raster/GEE/Open_train.shp")
habs<-rbind(pcw_train, open_train)
?n()
?length
# Lets get randowm points from within our polys
samp_pcw<- st_sample(pcw_train, 5000, type = "random")
dat=data.frame(ID=1:length(samp_pcw), class="1")
samp_pcw<-st_sfc(samp_pcw)%>%st_sf(cbind(dat, samp_pcw))
samp_pcw$geometry=NULL

samp_open<- st_sample(open_train, 5000, type = "random")
dat=data.frame(ID=1:length(samp_open), class="0")
samp_open<-st_sfc(samp_open)%>%st_sf(cbind(dat, samp_open)) # 
samp_open$geometry=NULL

# bind into single df to extract from raster
samp_habs<-rbind(samp_pcw, samp_open)

#clean up
rm(samp_open, samp_pcw)

# select bands to use for the classification
rast1<-stack(rast[[1:3]],rast[[7]], MSAVI, ndvi)
names(rast1) <- c('B2', 'B3','B4','B8','MSAVI','NDVI')
rast1
# Extract the layer values for the sample locations. This is the data matrix that will be used to train the RF model.
# use `sp = TRUE` argument in `extract` function.
trainvals <- raster::extract(rast1, samp_habs, df = TRUE, sp=T)%>%st_as_sf()

# drop the ID column
trainvals <- trainvals[, -1]
head(trainvals)

# rename cols to informative names
names(trainvals)[6] <- "MSAVI"
names(trainvals)[7] <- "NDVI"

# load modelling packages and snow to allow use of multicores should we need to speed things up
library(snow)
library(caret)
library(randomForest)

# Two packages that implement RF in R, Caret and randomForest. Both allow tuning of critical parameters and caret has nice comparison features to allow this but it does not allow variation of ntree without some wrapping code.
# need to experiment with your data and identify what tuning parameters suit your data best.

# mtry: Number of variables randomly sampled as candidates at each split.
# ntree: Number of trees to grow.
# recommend defaults = mtry=floor(sqrt(ncol(x))) and ntree=500.
# fit RF in Caret using default setup and repeated cross validation sampling.
# due to the randomisation in RF if youwant to repat and tune parameters it is crucial you set the seed prior to doing this.
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"
set.seed(seed)
mtry <- sqrt((ncol(trainvals)-1))
tunegrid <- expand.grid(.mtry=mtry)
rast1
modFit_rf <- train(as.factor(class) ~ B2 + B3 + B4 + B8 + MSAVI + NDVI, method = "rf", data = trainvals,metric=metric, tuneGrid=tunegrid, trControl=control)
print(modFit_rf)

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
plot(preds_rf)


# my stuff - next steps - use the output of rf to mask the input and remove sitka stands then use kmeans to estimate the variety of habitats around nest sites etc