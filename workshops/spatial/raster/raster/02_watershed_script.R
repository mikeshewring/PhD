# sophies DEM stream  script
rast<-raster(file.choose())
rast
plot(rast)
# i think this work flow below will work to pull your ridgelines out.
# you will need a suitable resolution DEM map and then invert it and run this script on it. replace dem with your terrain model.
# need to install GRASS GIS onto your laptop first #https://grass.osgeo.org/

# r can talk to grass GIS through rgrass7::

library(rgrass7)
library(link2GI)
link = findGRASS() 

link2GI::linkGRASS7(rast, ver_select = TRUE) # this a qucik link to get GRASS database running in your spatial projection and region

# see here fyi ?linkGRASS7
# https://gis.stackexchange.com/questions/182120/get-rivers-from-a-dem-raster

# 1. convert raster to spatial grid data frame
g <- as(rast, 'SpatialGridDataFrame')

# 2. load raster into GRASS data base
writeRAST(g, vname = "wales_neg_dem")

# 3.  use Grass r.watershed to delineate streams or ridges...
execGRASS(cmd='r.watershed', flags='overwrite', 
          parameters =  list(elevation='wales_neg_dem', threshold=1000,
                             drainage='wales_drain',accumulation='wales_accu'
                             ,basin='wales__basin', stream='wales__stream',
                             memory=2000))

# 4. thin streams to make them 1-pixel wide
execGRASS('r.thin', parameters = list(input='wales__stream', 
                                      output='wales__stream_th'),
          flags = "overwrite") 
# 5. convert stream raster to vector
execGRASS('r.to.vect', parameters = list(input='wales__stream_th', 
                                         output='v_stream_wales', 
                                         type= "line"), 
          flags = "overwrite")
                                         

# 6. export example as simple feature and plot
ridges = readVECT("v_stream_wales") %>%
  st_as_sf() %>%
  st_geometry()
plot(wales$geometry)
plot(ridges)
head(ridges)


