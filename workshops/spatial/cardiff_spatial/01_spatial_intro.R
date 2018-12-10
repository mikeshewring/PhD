# Spatial Workshop - Introduction to SF and spatial data in R
# and maybe some graphs/ maps

# Cardiff 13th Dec 2018
#install ####
install.packages(c("sf","ggplot2","ggspatial","rgdal","rgbif","rnaturalearth", "rgeos", "raster","dplyr","tidyr","ggthemes"))

#libraries ####
library(maps)
library(sf)
library(ggplot2)
library(ggspatial)
library(rgdal)
library(rnaturalearth)
library(rgeos)
library(raster)
library(dplyr)
library(tidyr)
library(rgbif)  # To extract GBIF data
library(CoordinateCleaner)  # To clean coordinates if you want to explore that later
library(gridExtra)  # To make pretty graphs
library(ggrepel)  # To add labels with rounded edges
library(png)  # To add icons
library(mapdata)  # To plot maps
library(ggthemes)# To make maps extra pretty
library(maptools)
library(ggplot2)
library(maps) # To plot maps


# If you like it tidy and the dplyr, sf is very useful. sf is essentally a data.frame with  a list of spatial attribute attached, it works well in the tidy univers. useful blog http://strimas.com/r/tidy-sf/.

# 01. introduction to spatial features and Coordinate reference systems (CRS) ####
# create two objects with their long/lat coordinates
point_cardiff <- data.frame(name = 'Cardiff', longitude = -3.1790, latitude = 51.4815)
point_swansea <- data.frame(name = 'Swansea', longitude = -3.9436, latitude = 51.6214)
city_points <- rbind(point_cardiff, point_swansea)

# Now have a look at them in a 2D space.
plot(city_points[,c("longitude", "latitude")], pch = 19, col = 'magenta')
text(city_points[,c("longitude", "latitude")], labels = city_points$name, pos = c(2, 4))

#This does not look correct.....we need a projection system, essentially a refernce system. For example, you might have seen the expression WGS84, the common longitude-latitude (degree decimal), or maybe EPSG:4326. In other words, spatial objects should come with some information relative to their reference system (CRS), no matter if it's a raster, a point, a line of a complex polygon. To locate an object in space, you need to know the reference system and the units in which the coordinates are expressed

# Adding a projection to our cities, using WGS84 (EPSG:4326)
proj_city_points <- sf::st_as_sf(city_points, coords = c("longitude", "latitude"), crs = 4326)
plot(proj_city_points, pch = 19, col = c("magenta", "blue"), cex = 1.5)
legend("topleft", legend = proj_city_points$name, col = c("magenta", "blue"), pch = 19, cex = 1.5, bty="n")

# get some base layer to see this in perspective
country_sf_gbr <- sf::st_as_sf(raster::getData(name = "GADM", country = 'GBR', level = 1)) # 3 letter code. level 1 is country.

plot(country_sf_gbr$geometry, graticule = TRUE, axes = TRUE, col = "wheat1", xlim = c(-10, 10), ylim = c(50, 61))
plot(proj_city_points, pch = 19, col = c("magenta", "blue"), cex = 1.5, add = TRUE)
legend("topleft", legend = proj_city_points$name, col = c("magenta", "blue"), pch = 19, cex = 1.5, bty="n")
text(x = -15, y = 50.2, "EPSG:4326 - WGS84", pos = 2, cex = 0.7)

#lets filter to wales
wales=country_sf_gbr%>%filter(NAME_1 =="Wales")
wales
plot(wales$geometry, graticule = TRUE, axes = TRUE, col = "wheat1") #xlim = c(-5, 1), ylim = c(51, 53.5))
plot(proj_city_points, pch = 19, col = c("magenta", "blue"), cex = 1.5, add = TRUE)
legend("topleft", legend = proj_city_points$name, col = c("magenta", "blue"), pch = 19, cex = 1.5, bty="n")
text(x = -8, y = 51.4, "EPSG:4326 - WGS84", pos = 4, cex = 0.7)

#The advantage of having a CRS is that you can transform your coordinates, so it is expressed to other reference system (i.e. on other origin and units).

# *Changing Projection* (st_tranform)
# Some projections more appropriate for representing specific geographic context. Why is their so many CRS?
# Reproject your city points in OSGB 1936 / British National Grid (EPSG:27700)
proj_city_points_osgb <- sf::st_transform(proj_city_points, crs = 27700)
wales_osgb <- sf::st_transform(wales, crs = 27700)

# visualize the points on a map
plot(wales_osgb$geometry, graticule = TRUE, axes = TRUE, col = "wheat1")
plot(proj_city_points_osgb, pch = 19, col = c("magenta", "blue"), cex = 1.5, add = TRUE)
legend("topleft", legend = proj_city_points_osgb$name, col = c("magenta", "blue"), pch = 19, cex = 1.5, bty="n")
text(x = -18000, y = 180000, "EPSG:27700 - OSGB 1936 ", pos = 4, cex = 0.7)

# 02. create features from scratch ####
# points ####
p1 <- sf::st_point(c(1, 2)) # using st_point to create a point with the co-ordinates x=1,y=2
p2 <- sf::st_point(c(3, 5))# using st_point to create a point with the co-ordinates x=3,y=5


p3 <- sf::st_multipoint(matrix(2 * c(1, 2, 4, 2, 3, 5, 7, 3), ncol = 2, byrow = TRUE))# using st_multipoint to create a numebr of points with the co-ordinates listed. Multi-points are separate points that share the same attributes but different geographic locations

p4 <- sf::st_as_sf(data.frame(X = c(1, 4, 3, 7), Y = c(2, 2, 5, 3) ), coords = c("X", "Y")) # using st_as_sf to convert a dataframe to a simple feature spatial point feature object. 

p5 <- sf::st_sfc(p1, p2, p3) # using st_sfc to create simple feature geometry list column from point objects created above

plot(p1)
plot(p3, col = "blue", pch = 19)
plot(p5, col = "magenta", pch = 19, add = TRUE)

### Lines ####
l1 <- sf::st_linestring(matrix(c(1, 1, 2, 2, 3, 3, 4, 4, 4, 2), ncol = 2, byrow = TRUE)) # create a line using linestring command and a matrix of co-ordinates

bl1 <- sf::st_buffer(l1, 2) # buffer linestring st_buffer command (also works with points, multi-points and polys)

plot(bl1)
plot(l1, col = "tomato3", lwd = 1.5, add = TRUE)


### Polygons ####
bl1 <- sf::st_buffer(l1, 2)

plot(bl1, col = "lightblue", border = NA)

plot(l1, col = "tomato3", lwd = 1.5, add = TRUE)

### Intersects and intesections

p1 <- sf::st_as_sf(data.frame(X = c(1, 4, 3, 7), Y = c(2, 2, 5, 3) ), coords = c("X", "Y"), crs = 4326)
poly1 <- st_as_sfc(st_bbox(st_buffer(p1[2,], 2)))
poly2 <- st_as_sfc(st_bbox(st_buffer(p1[3,], 1.5)))

plot(st_geometry(poly1), col = "goldenrod", xlim = c(-5, 5), ylim = c(0, 10))
plot(st_geometry(poly2), col = rgb(1,1,0,0.3), add = TRUE)
plot(st_geometry(p1), col = "magenta", pch = 19, cex= 1.5, add = TRUE)

## INTERSECTION - produces a new shape. The area that intersects.
poly3 <- sf::st_intersection(poly1, poly2)
plot(st_geometry(poly3), col = "lightblue", add = TRUE)

## INTERSECT- true false do they intersect- quicker
p1_poly1 <- sf::st_intersects(p1, poly1, sparse = FALSE)
plot(st_geometry(p1[p1_poly1,]), col = "turquoise", pch = 19, cex = 2, add = TRUE)

#### Also see st_difference to determine difference between objects
#### and Union to merge and melt objects

##### Load and manipulate spatial objects

#Spatial data are increasingly available from the Web, from species occurrence to natural and  cultural features data, accessing spatial data is now relatively easy. For base layers, you can find many freely available data sets such as the ones provided by the Natural Earth [http://www.naturalearthdata.com], the IUCN Protected Planet database [www.protectedplanet.net], the GADM project [https://gadm.org], worldclim [http://worldclim.org/version2] the CHELSA climate data sets [http://chelsa-climate.org] or the European Environmental Agency [https://www.eea.europa.eu/data-and-maps/data#c0=5&c11=&c5=all&b_start=0]

# 03. loading data ####
# lets get some seabird data http://lle.gov.wales/catalogue/item/SeabirdsAtSea/?lang=en
#GIS layers showing the abundance and distribution of seabirds in welsh waters. The datasets consist of the raw data showing observations of all seabirds and derived grids showing the density of flying and sitting species on a 3km grid scale and the survey coverage. The purpose of this data capture was to collate data from ESAS and WWT Consulting databases to produce combined database of birds recorded from surveys of Welsh waters.

# load shapefile of polygons
ESAS<-sf::st_read("workshops/spatial/cardiff_spatial/NRW_SEABIRD_WWT_ESAS_MERGED_SITTING_2KM_WGS84/NRW_SEABIRD_WWT_ESAS_MERGED_SITTING_2KM_WGS84Polygon.shp")
ESAS
#lets transform to Uk CRS ref
ESAS <- sf::st_transform(ESAS, crs = 27700)

#inspect data
str(ESAS) # data is in a wide format i.e. some coloumns are not variables (species, count)

# lets convert to a tidy dataframe and filter to Guillemot 
foo=gather(ESAS, key="species", value="count", 4:27)%>%
  # turn into col per variable
    filter(species=="Guillemot")%>%
  #filter to Guillemot
  filter(!count=="0")
  #remove zero values. Probably not a good idea for analysis but useful for visualisation here to save time
foo <- sf::st_transform(foo, crs = 27700)
wales_osgb <- sf::st_transform(wales_osgb, crs = 27700)

# base sf plot of count
plot(wales_osgb$geometry, col="grey", graticule=T, axes = T)
plot(foo[7], add=T)

# ggplot of count on the Wales base map we created earlier
(UA.map<-ggplot(foo) +
  geom_sf(aes(fill = as.numeric(count)))+
    scale_fill_viridis_c(option = "plasma", name = "density")+
    xlab("Longitude")+ylab("Latitude")+
    geom_sf(data=wales_osgb, colour = "gray40")+
    ggtitle("ESAS Uria aalge density map") +
    theme(panel.background = element_rect(fill = "aliceblue")))


# 04. lets download some data fro a different source and check against our ESAS to see if the story is the same####
# Download species occurrence records from the Global Biodiversity Information Facility
# *** rgbif package and the occ_search() function ***
#The package rgbif offers an interface to the Web Service methods provided by GBIF. It includes functions for searching for taxonomic names, retrieving information on data providers, getting species occurrence records and getting counts of occurrence records.

#In the GBIF dataset, every country has a unique code. We can find out the code for the UK with this line of code.

UK_code <- isocodes[grep("United Kingdom", isocodes$name), "code"]

# Now, we can download all the occurrence records for the Common guillemot/ murre (Uria aalge) in the UK using the function occ_search.

occ <- occ_search(scientificName = "Uria aalge", country = UK_code, hasCoordinate = TRUE, limit = 20000, year = '2000,2018', return = "data")
#This will return a dataset of all the occurrences recorded in the UK between 2006 and 2016 that have geographic coordinates.

# Have a look at the dataset
str(occ)

# filter to the variables we need with dplyr
occ<-  dplyr::select(occ,key, name, decimalLongitude,
                     decimalLatitude, year,
                     individualCount, country)


(UA.gbifmap <- ggplot(occ, aes(x = decimalLongitude, y = decimalLatitude)) + 
    # Specify to only present the UK region of the world in the map 
    # Also change the colour, size of map country borders
    borders(database = "world", regions = "UK", colour = "gray40", size = 0.3) +
    # Change the colour and transparency of the plotted occurrence points 
    geom_point(alpha = 0.4, colour = "red")) 

#convert to sf object
gbifocc=sf::st_as_sf(occ,coords = c("decimalLongitude", "decimalLatitude"), crs=4326)

# crop to our area of interest i.e. Wales and surrounding area
gbifocc1=st_crop(gbifocc, (st_buffer(wales, 0.5)))
#lets transform to Uk CRS ref
gbifocc1 <- sf::st_transform(gbifocc1, crs = 27700)

# extract point coords for density mapping in ggplot2
sf_dots <- st_coordinates(gbifocc1)%>%as.data.frame()%>%setNames(c("lon","lat"))
sf_dots

# ggplot of count on the Wales base map we created earlier
(UA.gbifmap<-ggplot()
  +stat_density_2d(data=sf_dots, aes(x=lon, y=lat, fill = ..level..), geom="polygon")
  +scale_fill_viridis_c(option = "plasma", name = "No. of Occurences")
  +geom_sf(data=gbifocc1)
  +xlab("Longitude")
  +ylab("Latitude")
  +geom_sf(data=wales_osgb, colour = "gray40")
  +ggtitle("GBIF Uria aalge occurence map") 
  +theme(panel.background = element_rect(fill = "aliceblue")))

# arrange maps in a grid for presentation
row1 <- grid.arrange(UA.gbifmap, UA.map, ncol = 2, widths = c(1, 1))


# 05. lets download some modelling predictions from tracking data and check against our ESAS and GBIF data####
#2010-2014 Royal Society for the Protection of Birds (RSPB) United Kingdom Shag, Guillemot, Kittiwake and Razorbill distributions
#estimate the distribution at sea of four seabird species, foraging from approximately 5,500 breeding sites in Britain and Ireland. To do so, we GPS-tracked a sample of 230 European Shags Phalacrocorax aristotelis, 464 Black-legged Kittiwakes Rissa tridactyla, 178 Common Murres Uria aalge, and 281 Razorbills Alca torda from 13, 20, 12, and 14 colonies, respectively. Using Poisson point process habitat use models
#data availabke here = http://dassh.ac.uk/downloads/DASSHDT00000346-AS01/Lonlat_Murre.tif"
#
UA.rast<-raster("workshops/spatial/cardiff_spatial/Lonlat_Murre.tif")

#crop to area of interest
UA.rastwales<-crop(UA.rast, wales)

#plot
plot(UA.rastwales, xlab = "lat", ylab="long")


# 06. lets review seabird census data for the region####
JNCC<-st_read("workshops/spatial/cardiff_spatial/JNCC_seabird_census_data/Data/magseabirds.shp")

#inspect data
str(JNCC)

names(JNCC)
# lets convert to a tidy dataframe and filter to Guillemot 
UA.JNCC=gather(JNCC, key="species", value="count", 4:29)%>%
  # turn into col per variable
  filter(species=="GUILLEMOT")

UA.JNCC<-sf::st_transform(UA.JNCC, crs = 27700)

#crop to area of interest
UA.JNCC<-st_crop(UA.JNCC, wales_osgb)

#sf plot
plot(UA.JNCC[5], graticule =T, axes = T)
plot(wales_osgb$geometry, add=T)

# extract point coords for density mapping in ggplot2
sf_dots <- st_coordinates(UA.JNCC)%>%as.data.frame()%>%setNames(c("lon","lat"))
sf_dots

# ggplot of count on the Wales base map we created earlier
(UA.JNCCmap<-ggplot()
  +stat_density_2d(data=sf_dots, aes(x=lon, y=lat, fill = ..level..), geom="polygon")
  +scale_fill_viridis_c(option = "plasma", name = "No. of Occurences")
  +geom_sf(data=UA.JNCC)
  +xlab("Longitude")
  +ylab("Latitude")
  +geom_sf(data=wales_osgb, colour = "gray40")
  +ggtitle("JNCC Uria aalge colony map") 
  +theme(panel.background = element_rect(fill = "aliceblue")))

# 07 colony count data
# http://jncc.defra.gov.uk/smp/Default.aspx

UA.df<-read.csv("workshops/spatial/cardiff_spatial/JNCC_seabird_census_data/JNCC_seabird_data.csv")

summary(as.factor(UA.df$Master.site))

UA.df$year<-as.numeric(UA.df$year)

plot(UA.df$Count~UA.df$year)
plot(UA.df2$scalepop~UA.df2$year)

UA.df2<-UA.df%>%  distinct()%>%  filter(is.finite(Count))%>%  group_by(Master.site, Site)%>%  mutate(maxyear= max(year), minyear=min(year), duration=maxyear-minyear, scalepop=(Count-min(Count))/(max(Count)-min(Count)))%>%filter(is.finite(scalepop), length(unique(year))>5)%>%ungroup()
library(broom)

#skomer trends
dat=filter(UA.df2,UA.df2$Site=="Skomer")
mod<-lm(scalepop~as.factor(year), data=dat)
sk=tidy(mod)
sk$year=seq(1985,2016,1)

plot(sk$estimate~sk$year)
(Skomer.slopes <- ggplot(sk, aes(x = year, y = estimate)) +
  geom_pointrange(aes(ymin = estimate - std.error,
                      ymax = estimate + std.error),
                  alpha = 0.3, size = 0.3) +
    geom_point()+
    geom_smooth(method = "lm", colour = "yellow", fill = "yellow", alpha = 0.4)+
    geom_hline(yintercept = 0, linetype = "dashed") +
    ggtitle("JNCC Skomer SPA Uria aalge colony counts")+
    ylim(-1,1)+
    ylab("Population change\n") +
    xlab("\nYear"))

# castlemartin
dat=filter(UA.df2,UA.df2$Master.site=="Castlemartin Coast SPA (Berryslade to Barafundle Bay)")
mod<-lm(scalepop~as.factor(year), data=dat)
sk=tidy(mod)
sk$year=seq(1986,2018,1)

(Castlemartin.SPA.slopes <- ggplot(sk, aes(x = year, y = estimate)) +
    geom_pointrange(aes(ymin = estimate - std.error,
                        ymax = estimate + std.error),
                    alpha = 0.3, size = 0.3) +
    geom_smooth(method = "lm", colour = "yellow", fill = "yellow", alpha = 0.4)+
    geom_hline(yintercept = 0, linetype = "dashed") +
    ggtitle("JNCC Castlemartin SPA Uria aalge colony counts")+
    ylim(-1,1)+
    ylab("Population change\n") +
    xlab("\nYear"))

# 06 ####
# Create panel of all graphs
# Makes a panel of the map and occurrence plot and specifies the ratio
# i.e., we want the map to be wider than the other plots
# suppressWarnings() suppresses warnings in the ggplot call here
row1 <- suppressWarnings(grid.arrange(UA.map, UA.gbifmap, ncol = 2, widths = c(1, 1)))
# Makes a panel of the four population plots
row2 <- grid.arrange(UA.JNCCmap, Skomer.slopes, Castlemartin.SPA.slopes, ncol = 3, widths = c(1, 1, 1))

# Makes a panel of all the population plots and sets the ratio
# Stich all of your plots together
UA.panel <- grid.arrange(row1, row2, nrow = 2, heights = c(1.2, 0.8))

ggsave(UA.panel, filename = "UA.png", height = 10, width = 15)

