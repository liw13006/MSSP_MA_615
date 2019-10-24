
# programming for
## https://richardlent.github.io/rnotebooks/maps.nb.html

## Making Maps with R
## Richard A. Lent

library(knitr)

##### Map 1: Base R graphics with the maps and mapdata packages

# sitesmap1.R
# Map the sites data using the maps package.
# Use the maps package to plot state boundaries 
# contained in the mapdata package
# and then overlay site locations using the points 
# function. Additional text is plotted
# using the text function to locate text using the 
# latitude-longitude coordinates of the map.
# We then draw a box around the map using the box 
# function and add a scale with the map.scale function.
# All of this works because the base graphics package in R 
# will keep drawing a sequence of graphic statements to 
# the same graphics window until the window is closed 
# or a new graphics window is created.
# The Hmisc package has functions show.col and show.pch to 
# display available color codes and plotting symbols, respectively.
# Use colors() to list names of 657 additional colors.
# See also the R color cheatsheet:
# https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf
library(maps)     # Draw geographical maps.
library(mapdata)  # Map databases.
library(Hmisc)    # Miscellaneous useful functions.
sites <- read.csv("http://college.holycross.edu/faculty/rlent/sites/sites.csv")
# Do a simple linear regression of butterfly forewing length vs thorax width, 
# and save the regression residuals to the sites data frame.
reg <- with(sites, lm(fwlength ~ thorax))
sites$resid <- reg$residuals
# Make the map.
map('county', 'Massachusetts,suffolk', 
    col='gray90', fill=TRUE, mar = c(0, 0, 1, 0),plot = TRUE,boundary = TRUE)
with(sites, points(lon_dd, lat_dd, pch=16, col='black', cex=.7))

# library(tidyverse)

write.csv(sites, "sites.csv")

#############################


# Comment out the previous 'with' statement, and uncomment the following 
# 'with' statement, to size the site points by the magnitude of their regression residual.
# with(sites, points(lon_dd, lat_dd, pch=16, col='black', cex=abs(resid)+1))
with(sites, text(lon_dd-.15, lat_dd, cex=0.7, labels=code))
text(-72, 42.4, labels='MA', cex=1.5)



############################

text(-71.5, 43.4, labels='NH', cex=1.5)
text(-72.9, 44.3, labels='VT', cex=1.5)

############################

box()
map.scale(ratio = FALSE, metric = FALSE)

############################



###########################################################
### Map 2: The leaflet package


# sitesmap2.R
# Map the sites data using the leaflet package.
# This creates a prettier, interactive map.
library(leaflet)
library(maps)
library(htmlwidgets) # To save the map as a web page.

# The data to map.
sites <- read.csv("http://college.holycross.edu/faculty/rlent/sites/sites.csv")
# State boundaries from the maps package. The fill option must be TRUE.
bounds <- map('state', c('Massachusetts', 'Vermont', 'New Hampshire'), fill=TRUE, plot=FALSE)
# A custom icon.
icons <- awesomeIcons(
  icon = 'disc',
  iconColor = 'black',
  library = 'ion', # Options are 'glyphicon', 'fa', 'ion'.
  markerColor = 'blue',
  squareMarker = TRUE
)
# Create the Leaflet map widget and add some map layers.
# We use the pipe operator %>% to streamline the adding of
# layers to the leaflet object. The pipe operator comes from 
# the magrittr package via the dplyr package.
map <- leaflet(data = sites) %>%
  # setView(-72.14600, 43.82977, zoom = 8) %>% 
  addProviderTiles("CartoDB.Positron", group = "Map") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite")%>% 
  addProviderTiles("Esri.WorldShadedRelief", group = "Relief")%>%
  # Marker data are from the sites data frame. We need the ~ symbols
  # to indicate the columns of the data frame.
  addMarkers(~lon_dd, ~lat_dd, label = ~locality, group = "Sites") %>% 
  # addAwesomeMarkers(~lon_dd, ~lat_dd, label = ~locality, group = "Sites", icon=icons) %>%
  addPolygons(data=bounds, group="States", weight=2, fillOpacity = 0) %>%
  addScaleBar(position = "bottomleft") %>%
  addLayersControl(
    baseGroups = c("Map", "Satellite", "Relief"),
    overlayGroups = c("Sites", "States"),
    options = layersControlOptions(collapsed = FALSE)
  )
invisible(print(map))

# Save the interactive map to an HTML page.
saveWidget(map, file="sitesmap2.html", selfcontained=TRUE)
# Uncomment the following line to save the interactive map to a static image file.
# mapshot(map, file="sitesmap2.png")




