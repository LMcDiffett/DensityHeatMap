# This script will pull in a CSV file that contains a list of fatal crash locations with GPS coordinates and some basic descriptors
# and generate density heat maps that are overlaid onto city maps

# Much of the basics of this mapping come from an excellent explanation of contour and density mapping by Andrew Collier at 
# http://www.exegetic.biz/blog/2013/12/contour-and-density-layers-with-ggmap/

#requires the following libraries to be installed and loaded

library(ggmap)
library(ggplot2)
library(spatstat)
library(maptools)
library(rgdal)
library(dplyr)
library(RCurl)



# Import the fatal crash data from the .csv file
  crashes <-read.csv( text = getURL("https://raw.githubusercontent.com/LMcDiffett/DensityHeatMap/master/Fatal_Crash_Data_Spokane_WA.csv", ssl.verifypeer = FALSE), header = T, stringsAsFactors = F)
  
  # clean the data in case there are missing or abnormal GPS coordinates
  # we can ensure that no impossible coordinates are present by limiting Lat / Long to -180 <= x <= 180
    crashes <- crashes %>% filter( !is.na(LATITUDE) , !is.na(LONGITUDE), LATITUDE >= -180, LATITUDE <= 180, LONGITUDE >= -180, LONGITUDE <= 180)
  
  # only interested in crashes over the last five years of available data (2010-2015)
    crashes <- crashes %>% filter(YEAR >= 2010)

  # we can also create various subsets of different crash types--let's look at red light and speed related crashes
    rlCrashes <- crashes %>% filter(REDLIGHTCRASH == 1)
    speedCrashes <- crashes %>% filter(SPEEDREL == 1)

# Let's get the basemap defined
  basemap <- get_map(location = "Spokane,WA",maptype = "roadmap", zoom = 12, color = "bw")
  

  
  # Now we simply visuallize it using ggplot2 formatting
    # note that depending on the zoom used on the map, you may get warning messages about certain points not being plotted if the density extends out of the frame

  # all fatal crashes
    ggmap(basemap, extent = "device", maprange = T) +
      geom_density2d(data = crashes, aes(x = LONGITUDE, y = LATITUDE), color = "gray45", n = 500) +
      stat_density2d(data = crashes, aes(x = LONGITUDE, y = LATITUDE,  fill = ..level.., alpha = ..level..), geom = 'polygon', n = 500) +
      scale_fill_gradient(low = "green3", high = "red", guide = F) +
      scale_alpha(range = c(0.05, 0.2), guide = F) +
      theme( axis.title = element_blank(), text = element_text(size = 12), axis.text = element_blank(), axis.ticks = element_blank())+
      labs(title = "Fatal Crash Density",
           caption = "Spokane, WA (2010-2015)")
    
  # speed related fatal crashes
    ggmap(basemap, extent = "device", maprange = T) +
      geom_density2d(data = speedCrashes, aes(x = LONGITUDE, y = LATITUDE), color = "gray45", n = 500) +
      stat_density2d(data = speedCrashes, aes(x = LONGITUDE, y = LATITUDE,  fill = ..level.., alpha = ..level..), geom = 'polygon', n = 500) +
      scale_fill_gradient(low = "green3", high = "red", guide = F) +
      scale_alpha(range = c(0.05, 0.2), guide = F) +
      theme( axis.title = element_blank(), text = element_text(size = 12), axis.text = element_blank(), axis.ticks = element_blank())+
      labs(title = "Speed Related Fatal Crash Density",
           caption = "Spokane, WA (2010-2015)")
  
  # you can adjust the way the density is calculated by manipulating the n variable in geom_density2d() and stat_density2d()
    # a low value has lower resolution
    ggmap(basemap, extent = "device", maprange = T) +
      geom_density2d(data = speedCrashes, aes(x = LONGITUDE, y = LATITUDE), color = "gray45", n = 5) +
      stat_density2d(data = speedCrashes, aes(x = LONGITUDE, y = LATITUDE,  fill = ..level.., alpha = ..level..), geom = 'polygon', n = 5) +
      scale_fill_gradient(low = "green3", high = "red", guide = F) +
      scale_alpha(range = c(0.05, 0.2), guide = F) +
      theme( axis.title = element_blank(), text = element_text(size = 12), axis.text = element_blank(), axis.ticks = element_blank())+
      labs(title = "Fatal Crash Density",
           caption = "Spokane, WA (2010-2015)")
    
    # increasing to 15, we get a bit higher resolution
    ggmap(basemap, extent = "device", maprange = T) +
      geom_density2d(data = speedCrashes, aes(x = LONGITUDE, y = LATITUDE), color = "gray45", n = 15) +
      stat_density2d(data = speedCrashes, aes(x = LONGITUDE, y = LATITUDE,  fill = ..level.., alpha = ..level..), geom = 'polygon', n = 15) +
      scale_fill_gradient(low = "green3", high = "red", guide = F) +
      scale_alpha(range = c(0.05, 0.2), guide = F) +
      theme( axis.title = element_blank(), text = element_text(size = 12), axis.text = element_blank(), axis.ticks = element_blank())+
      labs(title = "Fatal Crash Density",
           caption = "Spokane, WA (2010-2015)")
    