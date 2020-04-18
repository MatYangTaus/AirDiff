pacman::p_load(tidyverse, sf, tmap, skimr)

pm25.cty = read.csv('Data/Jesse/PM25_cnty_2020.csv')

pm25.mntr = read.csv('Data/Jesse/PM25_monitor_2020.csv')

pm25.mon.loc = pm25.mntr %>% 
       select(Site.ID, POC, Lat, Lon) %>% 
       group_by(Site.ID, POC) %>% 
       mutate(N = n()) %>% 
       distinct(Site.ID, POC, .keep_all = TRUE) %>% 
       arrange(N) %>% 
       st_as_sf(coords = c("Lon", "Lat"), crs = 4326)   %>% 
       st_transform(crs = st_crs(102003)) 

## Map
CountyGIS = st_read('M:\\Atesgis\\CMAQPM25_ER\\Data\\GISData\\gz_2010_us_050_00_5m.shp', stringsAsFactors = FALSE) %>% 
       filter(!(STATE %in% c('02', '15', '72'))) %>% 
       st_transform(crs = st_crs(102003)) %>% 
       arrange(STATE, COUNTY)

map1 = ggplot() +
       geom_sf(data = CountyGIS, fill = 'transparent') +
       geom_sf(data = pm25.mon.loc, aes(color = N), size = 2) +
#       scale_colour_viridis_c(option = "inferno", direction = 1) +
#       scale_fill_nord("aurora") +
       theme_minimal() +
       ggtitle(expression(PM[2.5]*' Monitors from 1/1/2020 to 4/6/2020 (n = 1126)')) +
       theme(plot.title = element_text(hjust = 0.5, size = 16))

map2 = tm_shape(pm25.mon.loc) +
       tm_dots('N', n = 5, palette = c("blue", "red"), 
               size = 1, title = 'Number of observation days', style = 'pretty') + 
       tm_shape(CountyGIS) +
       tm_borders() +
       tm_layout(main.title = 'PM2.5 Monitors from 1/1/2020 to 4/6/2020 (n = 1126) from AirNow',
                 main.title.size = 1.25,
                 main.title.position = "center",
                 legend.position = c("left", "bottom"),
              #   compass.type = "4star",
                 legend.text.size = 0.85,
                 legend.title.size = 1.0,
                 outer.margins=c(0.015, 0.01, 0, -0.05),
                 panel.label.bg.color = 'white',
                 frame = FALSE
       ) 

