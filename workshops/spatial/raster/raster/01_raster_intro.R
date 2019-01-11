# Rasters#
# Mike S. 
# 06/01/2018

library(raster)
library(rasterVis)
library(sf)
library(rgdal)
library(dplyr)
library(tidyr)
library(ggplot2)
library(adehabitat)
library(adehabitatHR)
library(adehabitatHS)
#Raster data is commonly used to represent spatially continuous data i.elevationA raster divides the world into a grid of equally sized rectangles (cells/pixels) that all have one or more values (or missing values) for the variables of interest. A raster cell can represent the average  value for the area it covers or estimates for the center of the cell.

#In raster data the geometry is implicitly set by knowing the spatial extent and the number or rows and columns in which the area is divided. From the extent and number of rows and columns, the size of the raster cells (spatial resolution) can be computed.

#example
?getData
dem<-getData(name='alt', country='GBR', mask=T)
dem
plot(dem)

# The raster package is used in R for raster data. The raster package is built around a number of classes of which the RasterLayer, RasterBrick, and RasterStack classes are most often used/encountered.

#The raster package has functions for creating, reading, manipulating, and writing raster data. 

#A RasterLayer object represents single-layer (variable) raster data. 

# The RasterStack and the RasterBrick are two classes for mulitilayer data. The principal difference between these two classes is that a RasterBrick can only be linked to a single (multi-layer) file. Whilst a RasterStack can be formed from separate files and/or from a few layers (‘bands’) from a single file. 

# lets crop to Wales. We can use a shapefile or extent object to do this to crop a raster object.
#get some geographic boundary data
country_sf_gbr <- sf::st_as_sf(raster::getData(name = "GADM", country = 'GBR', level = 1))
wales=country_sf_gbr%>%filter(NAME_1 =="Wales")

clip<-crop(dem, wales, snap="in")

# plot the clipped raster
plot(clip) # but we still have english values? 

#mask raster with the shapefile to create NoData where we have retained values
mask<-mask(clip, wales)
rm(clip)

#lets have a look at it
plot(mask)
drawExtent()
# lets have a look at the distribution of values
hist(mask)

#There are several functions to modify the spatial extent of Raster objects
# Crop function lets you take a geographic subset. An easy way to get an extent object is to plot a RasterLayer and then use drawExtent to visually determine the new extent (bounding box) to provide to the crop function.
#lets have a look at it
plot(mask)
r=drawExtent()

# trim crops a RasterLayer by removing the outer rows and columns that only contain NA values. 
mask1=trim(mask)
mask
mask1
par(mfrow=(c(1,2)))
plot(mask)
plot(mask1)

# Extend adds new rows and/or columns with NA values. 
mask2=extend(mask,10, value= 1500)
mask
mask2
par(mfrow=(c(1,2)))
plot(mask)
plot(mask2)

rm(mask1, mask2)
# These can be useful to make spatial extents match up 

# aggregate and disaggregate allow for changing the resolution (cell size) of a Raster* object. In the case of aggregate, you need to specify a function determining what to do with the grouped cell values (e.g. mean). 
# aggregate
mask1<-aggregate(mask, fact=10,fun=mean) # can use a filename argument to output direct to file
mask
mask1
par(mfrow=(c(1,2)))
plot(mask)
plot(mask1)

# disaggregate
?disaggregate
mask2<-disaggregate(mask1, fact=10) # can use a filename argument to output direct to file
mask
mask2
par(mfrow=(c(1,2)))
plot(mask)
plot(mask2)
rm(mask1, mask2)
#Resample is a similar function useful for when you want the same cell size, while shifting the cell centers. It can do either nearest neighbor assignments (for categorical data) or bilinear interpolation (for numerical data). 
?resample

# projectRaster function you can transform values of Raster object to a new object with a different coordinate reference system.

mask
gbmask<-projectRaster(mask, res=1000, crs='+init=epsg:27700')#method used to compute values for the new RasterLayer. Either 'ngb' (nearest neighbor), which is useful for categorical variables, or 'bilinear' (bilinear interpolation; the default value), which is appropriate for continuous variables.
gbmask
mask
par(mfrow=(c(1,2)))
plot(mask)
plot(gbmask)
par(mfrow=c(1,1))
#extract data as tif. See writeFormats for file write options
writeRaster(x=gbmask, filename='walesdem.tif', format='GTiff',overwrite=T)

# extract
#Extract values from a Raster* object at the locations of other spatial data. You can use coordinates (points), lines, polygons or an Extent (rectangle) object. You can also use cell numbers to extract values.
# http://lle.gov.wales/catalogue/item/AirQualityData/?lang=en
# http://lle.gov.wales/Catalogue/Item/ProtectedSitesSpecialAreasofConservation/?lang=en

# nox and SAC's
nox<-st_read('mapnox2016g_Grid.shp')

#change polygons into raster
noxrast<-rasterize(nox, gbmask, field='nox')

# visualise raster
plot(noxrast)

# load sac data
sac<-st_read('nrw_nonmarine_sac.shp') # filtered to non-marine sac's

# transform to OSGB36 CRS
sac<-st_transform(sac, crs = 27700)

#remove unwanted variables
sac<-sac%>%select("SAC_name","geometry")
head(sac)

# extract values for sites
df<-raster::extract(noxrast, sac, df=T, fun=mean, na.rm=T, sp=T)%>%st_as_sf(df)%>%filter(layer>=10)%>%filter(SAC_name =='Aberbargoed Grasslands'|SAC_name =='Blackmill Woodlands'|SAC_name =='Caeau Mynydd Mawr'|SAC_name =='Cardiff Beech Woods'|SAC_name =='Crymlyn Bog / Cors Crymlyn')
head(df)
df$sitenox=df$layer

# whats in surrounding area

sac2km<-sf::st_buffer(sac, 2000)
df1<-raster::extract(noxrast, sac2km, df=T, fun=mean, na.rm=T, sp=T)%>%st_as_sf(df)%>%filter(SAC_name =='Aberbargoed Grasslands'|SAC_name =='Blackmill Woodlands'|SAC_name =='Caeau Mynydd Mawr'|SAC_name =='Cardiff Beech Woods'|SAC_name =='Crymlyn Bog / Cors Crymlyn')
df$twokmnox=df1$layer

sac5km<-sf::st_buffer(sac, 5000)
df1<-raster::extract(noxrast, sac5km, df=T, fun=mean, na.rm=T, sp=T)%>%st_as_sf(df)%>%filter(SAC_name =='Aberbargoed Grasslands'|SAC_name =='Blackmill Woodlands'|SAC_name =='Caeau Mynydd Mawr'|SAC_name =='Cardiff Beech Woods'|SAC_name =='Crymlyn Bog / Cors Crymlyn')
df$fivekmnox=df1$layer

sac10km<-sf::st_buffer(sac, 10000)
df1<-raster::extract(noxrast, sac10km, df=T, fun=mean, na.rm=T, sp=T)%>%st_as_sf(df)%>%filter(SAC_name =='Aberbargoed Grasslands'|SAC_name =='Blackmill Woodlands'|SAC_name =='Caeau Mynydd Mawr'|SAC_name =='Cardiff Beech Woods'|SAC_name =='Crymlyn Bog / Cors Crymlyn')
df$tenkmnox=df1$layer
df


# plot data
(sac_nox<-ggplot(data = df)+ geom_point(aes(x=SAC_name, y=sitenox, colour=sitenox))+geom_point(aes(x=SAC_name, y=twokmnox, colour=twokmnox))+geom_point(aes(x=SAC_name, y=fivekmnox, colour=fivekmnox))+geom_point(aes(x=SAC_name, y=tenkmnox, colour=tenkmnox))+theme(axis.text.x = element_text(angle = 90, hjust = .3))+ylab("NOx"))


# Lets get some data to play with ####
#Kaczensky, P., Knauer, F., Jonozovic, M., Blazic, M. 1999. Slovenian brown bear 1993-1999 telemetry dataset.
#Movebank ID	14261492
#Number of Animals	23
#Number of Tags	22
#Number of Deployments	22
#Time of First Deployed Location	1993-05-04 21:00:00.000
#Time of Last Deployed Location	1999-09-23 20:10:00.000

bears<-read.csv('Brown bear Slovenia 1993-1999.csv')%>%sf::st_as_sf(coords = c("utm.easting", "utm.northing"),crs = 32633) # create an sf object containing bear data
head(bears)

# lets visulaise the dat
# first we need a basemap-get a map
map<- sf::st_as_sf(raster::getData(name = "GADM", country = 'SVN', level = 0))%>%st_transform(crs=32633)%>%st_simplify()# change to same CRS

map2<- sf::st_as_sf(raster::getData(name = "GADM", country = 'HRV', level = 0))%>%st_transform(crs=32633)%>%st_simplify()# change to same CRS

map1<-rbind(map,map2)

map<-st_buffer(map, 40000)
map<-st_crop(map1,map)

rm(map1,map2)

#get some road data
roads<-st_read('roads.shp')%>%filter(type == "motorway"| type=="primary")%>%st_transform(roads, crs=32633)
roads

# plot a map
(bear.map<-ggplot()
  +geom_sf(data=map)
  +geom_sf(data=roads)
  +geom_sf(data=bears, aes(colour=individual.local.identifier))
  +xlab("UTM Easting")
  +ylab("UTM Northing")
  +ggtitle("Bear Re-Loc map") 
  +theme(panel.background = element_rect(fill = "aliceblue")))
  

#get some habitat data for relevant period
#https://land.copernicus.eu/pan-european/corine-land-cover/clc-1990?tab=download
l=list.files("landcover1990", pattern = ".tif", full.names = T)
l
hab<-raster(l[1]) #if there were multiple raster files in this folder with the same extent and resolution (i.e. a time series of climate data) we could directly load them in with the stack command on the list object.

# This is in crs EPSG:3035. So lets define the CRS.
crs(hab)<-'+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs'

# lets have a look at the data
plot(hab) # european level data we need to crop and mask to our area of interest. As in crs EPSG:3035 we will need to reproject as well.
# its probably more time efficient to crop mask and then reproject...

# transform our area of interest map into same CRS
foo<-st_transform(map, crs = 3035)

# crop the raster
hab<-crop(hab, foo, snap="in")

# mask the areas outside of our area of interest
hab<-mask(hab, foo)

# lets have another look
plot(hab)

# reproject to same CRS as bear data. Used nearest neighbour method as categorical data
hab<-projectRaster(hab, res=250, crs='+init=epsg:32633', method="ngb")
hab
plot(hab)
plot(bears$geometry,
     add=T)
foo<-as.factor(hab)
# lets see what habitats they were using
habuse<-raster::extract(foo, bears,df=T, na.rm=T, sp=T)%>%st_as_sf(df)
head(habuse)
habuse$g250_clc90_V18_5<-as.factor(habuse$g250_clc90_V18_5)
plot(habuse$g250_clc90_V18_5) #23=broad-leaved forestry #24 = coniferous forestry #25= mixed forest

# additional activities
# how could we compare this to to what was available in the surrounding 5km ....
# probably needs to be on an individual basis?

habavail<-raster::extract(hab, bears, buffer=5000, df=T, na.rm=T, sp=T)%>%st_as_sf(df)

bears5km<-st_buffer(bears, 5000)

?eisera

# explore elevation data - any seasonal patterns in use
dem<-getData(name='alt', country='SVN', mask=T)
dem