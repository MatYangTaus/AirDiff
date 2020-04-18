pacman::p_load(tidyverse, sf, tmap, skimr, lubridate, stringr)

load('Data\\Jesse\\df_so2.Rdata') #df.so2
load('Data\\Jesse\\monitor_so2.Rdata') #so2.monitor

df.so2 %>% 
       distinct(location, cityURL, latitude, longitude) %>% 
       dim()

so2.monitor %>% 
       distinct(location, cityURL, latitude, longitude) %>% 
       dim()
## Map
CountyGIS = st_read('M:\\Atesgis\\CMAQpm25_ER\\Data\\GISData\\gz_2010_us_050_00_5m.shp', stringsAsFactors = FALSE) %>% 
       filter(!(STATE %in% c('02', '15', '72'))) %>% 
       st_transform(crs = st_crs(102003)) %>% 
       arrange(STATE, COUNTY)

mon.loc = df.so2 %>% 
       select(location, cityURL, latitude, longitude) %>% 
       distinct(location, cityURL, latitude, longitude, .keep_all = TRUE) %>% 
       mutate(latitude2 = latitude, longitude2 = longitude) %>% 
       st_as_sf(coords = c("longitude2", "latitude2"), crs = 4326)   %>% 
       st_transform(crs = st_crs(102003)) 

mon.loc2 =st_join(mon.loc, CountyGIS, join = st_intersects) %>% 
       filter(!is.na(NAME)) %>% 
       st_drop_geometry() %>% 
       select(-c(GEO_ID, LSAD, CENSUSAREA))

mon.list = df.so2 %>%
       data.frame() %>% 
       mutate(DateTime = with_tz(dateUTC, tz = 'America/Los_Angeles'), Date = date(DateTime) , Hour = hour(DateTime)) %>% 
       select(-c(DateTime, dateUTC, dateLocal, unit, parameter, country)) %>% 
       arrange(location, cityURL, latitude, longitude, Date, Hour) %>% 
       group_by(location, cityURL, latitude, longitude, Date) %>% 
       summarize(so2 = mean(value, na.rm = TRUE), N_Hour = n()) %>% 
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
       tm_layout(main.title = 'SO2 Monitors from 1/9/2020 to 4/8/2020 (n = 236) from ropenAQ',
                 main.title.size = 1.25,
                 main.title.position = "center",
                 legend.position = c("left", "bottom"),
                 #   so2mpass.type = "4star",
                 legend.text.size = 0.75,
                 legend.title.size = 0.9,
                 outer.margins=c(0.015, 0.01, 0, -0.05),
                 panel.label.bg.color = 'white',
                 frame = FALSE
       ) 


## Merge with other variabels
load('Data/Jesse/Censusvariable.RData') #CensusRestriction.df
load('Data/Jesse/CovidData.RData') #Covid.df

df.so2.master.temp  = df.so2 %>% 
       distinct(location, cityURL, latitude, longitude, dateUTC, value, .keep_all = TRUE) %>% 
       inner_join(mon.loc2, by = c('location', 'cityURL', 'latitude', 'longitude'))   %>% 
       mutate(GEOID = paste0(STATE, COUNTY), Date2 = date(dateLocal), so2 = value) %>% 
       left_join(CensusRestriction.df, by = 'GEOID') %>% 
       left_join(Covid.df, by = c('GEOID', 'Date2')) %>% 
       select(-c('STATE', 'COUNTY', 'country', 'city', 'unit', 'parameter', 'value', 'NAME')) 

df.so2.master.daymean = df.so2.master.temp %>% 
       filter(so2 >= 0) %>% 
       group_by(location, cityURL, latitude, longitude, GEOID, Name, Date2) %>% 
       summarize(N_Hour = n(), so2 = mean(so2, na.rm = TRUE)) %>% 
       filter(N_Hour >= 8) %>% 
       ungroup()

# criteria is 3 observations per month (assuming every 10 days at lesat)
df.so2.goodmonitor = df.so2.master.daymean %>% 
       mutate(Month = month(Date2)) %>% 
       group_by(location, cityURL, latitude, longitude, GEOID, Name, Month) %>%  
       summarize(N_Days = n()) %>% 
       ungroup() %>% 
       filter(Month != 4) %>% 
       filter(N_Days >= 3) %>% 
       group_by(location, cityURL, latitude, longitude, GEOID, Name) %>%  
       summarize(N.Month = n()) %>% 
       ungroup() %>% 
       filter(N.Month == 3) %>% 
       arrange(GEOID) %>% 
       mutate(SO2MonitorID = row_number())

df.so2.monitor = df.so2.master.daymean %>% 
       inner_join(df.so2.goodmonitor, by = c('location', 'cityURL', 'latitude', 'longitude', 'GEOID', 'Name')) %>% 
       left_join(CensusRestriction.df, by = c('GEOID' , 'Name')) %>% 
       left_join(Covid.df, by = c('GEOID', 'Date2')) %>% 
       arrange(SO2MonitorID, Date2) %>% 
       mutate(Name = as.character(Name), Stay.at.Home = as.Date(Stay.at.Home ), EduFacilClose = as.Date(EduFacilClose), NonEssentialServiceClose = as.Date(NonEssentialServiceClose), State = str_sub(Name, -2, -1)) %>% 
       select(SO2MonitorID, GEOID, Date2, State, Name, latitude, longitude, so2, N_Hour, Pop2010:Deaths) %>% 
       data.frame()

table(df.so2.monitor$State)
n_distinct(df.so2.monitor$SO2MonitorID)
n_distinct(df.so2.monitor$GEOID)

#save(df.so2.monitor, file = 'Data/Jesse/so2MonitorDF.RData')
## Create so2unty data

df.so2.county = df.so2.monitor %>% 
       group_by(GEOID, Date2) %>% 
       summarize(so2 = mean(so2, na.rm = TRUE)) %>% 
       ungroup() %>% 
       left_join(CensusRestriction.df, by = 'GEOID') %>% 
       left_join(Covid.df, by = c('GEOID', 'Date2')) %>% 
       mutate(State = str_sub(Name, -2, -1)) %>% 
       arrange(GEOID, Date2) %>% 
       select(GEOID, Date2, State, Name, so2,  everything()) %>% 
       data.frame()

#save(df.so2.so2unty, file = 'Data/Jesse/so2so2untyDF.RData')

## Create Past AQS Data
load('K:\\AirData\\OriginalData\\so2_Data_20160120.RData')
df.so2.county.pastyears = SO2_AQS %>% 
       mutate(GEOID = substr(FIPSPOC, 1,5)) %>% 
       group_by(GEOID, Date) %>% 
       summarize(so2 = mean(SO2_Value, na.rm = TRUE)) %>% 
       ungroup() %>%
       filter(GEOID %in% unique(df.so2.so2unty$GEOID), as.numeric(month(Date)) %in% c(1:4), as.numeric(year(Date)) %in% c(2017:2019)) %>% 
       data.frame()

#save(df.so2.so2unty.pastyears, file = 'Data/Jesse/so2so2untyPastYearDF.RData')

##
test = df.so2.county %>% 
       mutate(Index = ifelse(as.Date(Date2) <  as.Date(EduFacilClose), 'Before', 'After')) %>% 
       filter(!is.na(Index)) %>% 
       #group_by(GEOID, Index) %>% 
       #group_by(Urbanicity2, Index) %>% 
       group_by(State, Index) %>% 
       summarize(so2 = mean(so2, na.rm = TRUE)) %>% 
       pivot_wider(names_from = Index, values_from = so2) %>% 
       filter(!is.na(Before), !is.na(After)) %>% 
       mutate(Reduction = (1-After/Before)*100)
summary(test$Reduction)
filter(test, Reduction <0)
