# Packages
library(readxl)
library(maps)
library(leaflet)
library(raster)
library(sp)
library(maptools)
library(dplyr)

# Read data
df <- read_excel('CODistances33_23_05_2016.xls')

# Get easier to use column names
df$lon <- df$NEWLONGITU
df$lat <- df$NEWLATITUD

# Get Columbian shapefile
colombia <- getData('GADM', country = 'COL', level = 2)

# Get a spatial version of df
df_spatial <- data.frame(df)
coordinates(df_spatial) <- ~NEWLONGITU + NEWLATITUD
proj4string(df_spatial) <- proj4string(colombia)

# Get which polygons of th shapefile each point is in
keep_polys <- unique(over(df_spatial, polygons(colombia)))

# Get a smaller spatial object of only the relevant polygons
colombia_sub <- colombia[keep_polys,]

# Plot
plot(colombia_sub)
points(df$lon, df$lat, col = 'red')

# Leaflet plot
# m <- leaflet(data = colombia_sub)
m <- leaflet() %>% 
  setView(lng = mean(df$lon),
          lat = mean(df$lat),
          zoom = 8)
# m <- m %>% addProviderTiles('Stamen.Watercolor',
#                             options = providerTileOptions(opacity = 0.35))
# m <- m %>% addProviderTiles('MapQuestOpen.Aerial',
#                             options = providerTileOptions(opacity = 1))
m <- m %>% addProviderTiles('NASAGIBS.ViirsEarthAtNight2012',
                            options = providerTileOptions(opacity = 1))
m <- m %>% addProviderTiles('Stamen.TonerLite',
                            options = providerTileOptions(opacity = 0.25))
m <- m %>% addCircleMarkers(data = df, 
                            lng = ~lon, 
                            lat = ~lat,
                            radius = 5,
                            popup = ~SUBJID,
                            color = 'darkred',
                            opacity = 0.5,
                            fillOpacity = 0.5)
m 


