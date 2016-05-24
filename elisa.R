# Packages
library(readxl)
library(maps)
library(leaflet)
library(raster)
library(sp)
library(maptools)
library(dplyr)
library(ggmap)

# Define hospital location
hospital <- data.frame(
  name = 'Hospital San Jose',
  stringsAsFactors = FALSE
)
# Geocode hospital location
# temp <- geocode(location = hospital$name)
# hospital$lon <- temp$lon; hospital$lat <- temp$lat
hospital$lat <- 8.1772533
hospital$lon <- -76.0568453


# Read data
df <- read_excel('CODistances33_23_05_2016.xls')

# Adjust column names
names(df)[3] <- 'ethnic_group'

# Adjust ethnic group
df$ethnic_group <- ifelse(is.na(df$ethnic_group), 'other', df$ethnic_group)

# Create a color column
df$color <-
  ifelse(df$ethnic_group == 'other', 'blue', 
         ifelse(df$ethnic_group == 'Embera Katio ' , 'darkred', 'grey'))
df$color <- adjustcolor(df$color, alpha.f = 0.6)

# Get easier to use column names
df$lon <- df$NEWLONGITU
df$lat <- df$NEWLATITUD

# # Get distance 
# for (i in 1:nrow(df)){
#   sub_df <- df[i,]
#   temp <- route(to = c(hospital$lon, hospital$lat),
#                   from = c(sub_df$lon, sub_df$lat),
#                   # mode = 'walking',
#                   output = 'all',
#                   messaging = TRUE)
# }

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
points(df$lon, 
       df$lat, 
       col = df$color,
       pch = 16, 
       cex = 1)
# Add lines
for (i in 1:nrow(df)){
    sub_df <- df[i,]
    lines(x = c(sub_df$lon, hospital$lon),
          y = c(sub_df$lat, hospital$lat),
          col = sub_df$color)
}

# Leaflet plot
# m <- leaflet(data = colombia_sub)
m <- leaflet() %>% 
  setView(lng = mean(df$lon),
          lat = mean(df$lat),
          zoom = 9)
# m <- m %>% addProviderTiles('Stamen.Watercolor',
#                             options = providerTileOptions(opacity = 0.35))
# m <- m %>% addProviderTiles('MapQuestOpen.Aerial',
#                             options = providerTileOptions(opacity = 1))
m <- m %>% addProviderTiles('Esri.WorldImagery',
                            options = providerTileOptions(opacity = 1))
# m <- m %>% addProviderTiles('Stamen.TonerLite',
#                             options = providerTileOptions(opacity = 0.5))

# Add lines
df_temp <- df %>%
  dplyr::select(lon, lat, ethnic_group, SUBJID) 
hospital_temp <- hospital %>%
          dplyr::select(lon, lat) %>%
  mutate(ethnic_group = NA,
         SUBJID = NA)
results_list <- list()
for (i in 1:nrow(df)){
  new_data <- rbind(hospital_temp,
                    df_temp[i,])
  new_data$group <- i
  new_data$ethnic_group <- df_temp$ethnic_group[i]
  new_data$SUBJID <- df_temp$SUBJID[i]
  results_list[[i]] <- new_data
}
results <- do.call('rbind', results_list)
# Add indigenous lines
m <- m %>%
  addPolylines(data = results %>% filter(ethnic_group == 'Embera Katio '), 
               lng = ~lon, 
               lat = ~lat, 
               group = ~group,
               popup = ~SUBJID,
               color = 'darkred',
               opacity = 0.5,
               fillOpacity = 0.5)
# Add other lines
m <- m %>%
  addPolylines(data = results %>% filter(ethnic_group != 'Embera Katio '), 
               lng = ~lon, 
               lat = ~lat, 
               group = ~group,
               popup = ~SUBJID,
               color = 'darkblue',
               opacity = 0.5,
               fillOpacity = 0.5)
# Add others
m <- m %>% addCircleMarkers(data = df %>% filter(ethnic_group != 'Embera Katio '), 
                            lng = ~lon, 
                            lat = ~lat,
                            radius = 5,
                            popup = ~SUBJID,
                            color = 'darkblue',
                            opacity = 0.5,
                            fillOpacity = 0.5)
# Add indigenous
m <- m %>% addCircleMarkers(data = df %>% filter(ethnic_group == 'Embera Katio '), 
                            lng = ~lon, 
                            lat = ~lat,
                            radius = 5,
                            popup = ~SUBJID,
                            color = 'darkred',
                            opacity = 0.5,
                            fillOpacity = 0.5)
# Add hospital 
m <- m %>% 
  addCircleMarkers(data = hospital,
                   lng = ~lon,
                   lat = ~lat,
                   radius = 1,
                   popup = ~name,
                   opacity = 0.5,
                   fillOpacity = 0.5,
                   color = 'yellow',
                   weight = 1,
                   fill = FALSE)


m 


