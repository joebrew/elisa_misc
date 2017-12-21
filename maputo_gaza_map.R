library(cism)
library(sp)
library(tidyverse)
library(broom)
library(maptools)
library(RColorBrewer)
library(ggrepel)

data <- moz3
data <- data[data@data$NAME_1 %in% c('Maputo', 'Gaza'),]
data@data$id <- paste0(data@data$NAME_1,
                       '|',
                       data@data$NAME_2)
data <- tidy(data, region = 'id')
data$province <- 
  unlist(lapply(strsplit(data$id, '|', fixed = TRUE), function(x){x[1]}))
data$district <- 
  unlist(lapply(strsplit(data$id, '|', fixed = TRUE), function(x){x[2]}))

label_df <- data %>%
  group_by(district) %>%
  summarise(long = mean(long),
            lat = mean(lat))

ggplot() +
  geom_polygon(data = data,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = province),
               alpha = 0.5) +
  coord_map() +
  geom_polygon(data = data,
               aes(x = long,
                   y = lat,
                   group = group),
               color = 'black',
               fill = NA,
               alpha = 0.6,
               size = 0.5) +
  scale_fill_manual(name = 'Province',
                    values = c('darkgreen', 'darkblue')) +
  geom_label_repel(data = label_df,
             aes(x = long,
                 y = lat,
                 label = district),
             label.padding = unit(0.05, "lines"),
             size = 3,
             alpha = 0.9) +
  labs(x = 'Longitude',
       y = 'Latitude',
       title = 'Gaza and Maputo provinces') +
  theme_cism()
