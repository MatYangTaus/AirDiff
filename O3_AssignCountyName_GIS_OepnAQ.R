pacman::p_load(tidyverse, sf, tmap, skimr, lubridate, stringr)

load('Data\\Jesse\\df_o3.Rdata') #df.o3
load('Data\\Jesse\\monitor_o3.Rdata') #o3.monitor

df.o3 %>% 
       distinct(location, cityURL, latitude, longitude) %>% 
       dim()

o3.monitor %>% 
       distinct(location, cityURL, latitude, longitude) %>% 
       dim()
## Map
CountyGIS = st_read('M:\\Atesgis\\CMAQPM25_ER\\Data\\GISData\\gz_2010_us_050_00_5m.shp', stringsAsFactors = FALSE) %>% 
       filter(!(STATE %in% c('02', '15', '72'))) %>% 
       st_transform(crs = st_crs(102003)) %>% 
       arrange(STATE, COUNTY)

mon.loc = df.o3 %>% 
       select(location, cityURL, latitude, longitude) %>% 
       distinct(location, cityURL, latitude, longitude, .keep_all = TRUE) %>% 
       mutate(latitude2 = latitude, longitude2 = longitude) %>% 
       st_as_sf(coords = c("longitude2", "latitude2"), crs = 4326)   %>% 
       st_transform(crs = st_crs(102003)) 

mon.loc2 = st_join(mon.loc, CountyGIS, join = st_intersects) %>% 
       filter(!is.na(NAME)) %>% 
       st_drop_geometry() %>% 
       select(-c(GEO_ID, LSAD, CENSUSAREA))

mon.list = df.o3 %>%
       data.frame() %>% 
       mutate(DateTime = with_tz(dateUTC, tz = 'America/Los_Angeles'), Date = date(DateTime) , Hour = hour(DateTime)) %>% 
       select(-c(DateTime, dateUTC, dateLocal, unit, parameter, country)) %>% 
       arrange(location, cityURL, latitude, longitude, Date, Hour) %>% 
       group_by(location, cityURL, latitude, longitude, Date) %>% 
       summarize(O3 = mean(value, na.rm = TRUE)*1000, N_Hour = n()) %>% 
       ungroup() %>% 
       group_by(location, cityURL, latitude, longitude) %>%
       mutate(N = n()) %>% 
       ungroup() %>% 
       distinct(location, cityURL, latitude, longitude, .keep_all = TRUE) %>% 
       arrange(N) %>% 
       inner_join(mon.loc2, by = c('location', 'cityURL', 'latitude', 'longitude')) %>% 
       st_as_sf(coords = c("longitude", "latitude"), crs = 4326)   %>% 
       st_transform(crs = st_crs(102003)) 

map1 = tm_shape(mon.list) +
       tm_dots('N', n = 5, palette = c("blue", "red"), 
               size = 1, title = 'Number of observation days', style = 'pretty') + 
       tm_shape(CountyGIS) +
       tm_borders() +
       tm_layout(main.title = 'O3 Monitors from 1/9/2020 to 4/8/2020 (n = 1133) from ropenAQ',
                 main.title.size = 1.25,
                 main.title.position = "center",
                 legend.position = c("left", "bottom"),
                 #   compass.type = "4star",
                 legend.text.size = 0.75,
                 legend.title.size = 0.9,
                 outer.margins=c(0.015, 0.01, 0, -0.05),
                 panel.label.bg.color = 'white',
                 frame = FALSE
       ) 

pm25.mntr = read.csv('Data/Jesse/PM25_monitor_2020.csv') %>% 
       mutate(GEOID = str_sub(paste0('0', GEOID), -5, -1)) %>% 
       distinct(GEOID, .keep_all = TRUE) %>% 
       select(GEOID, Pop2010:NonEssentialServiceClose)

df.o3.master  = df.o3 %>% 
       data.frame() %>% 
       inner_join(mon.loc2, by = c('location', 'cityURL', 'latitude', 'longitude'))   %>% 
       mutate(GEOID = paste0(STATE, COUNTY)) %>% 
       arrange(GEOID, location, cityURL, latitude, longitude, dateUTC) %>%
       left_join(pm25.mntr, by = 'GEOID') %>% 
       select(-c(Date2, NAME))