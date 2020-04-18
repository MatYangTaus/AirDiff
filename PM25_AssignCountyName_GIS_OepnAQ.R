pacman::p_load(tidyverse, sf, tmap, skimr, lubridate, stringr)

load('Data\\Jesse\\df_pm25.Rdata') #df.pm25
load('Data\\Jesse\\monitor_pm25.Rdata') #pm25.monitor

df.pm25 %>% 
       distinct(location, cityURL, latitude, longitude) %>% 
       dim()

pm25.monitor %>% 
       distinct(location, cityURL, latitude, longitude) %>% 
       dim()
## Map
CountyGIS = st_read('M:\\Atesgis\\CMAQPM25_ER\\Data\\GISData\\gz_2010_us_050_00_5m.shp', stringsAsFactors = FALSE) %>% 
       filter(!(STATE %in% c('02', '15', '72'))) %>% 
       st_transform(crs = st_crs(102003)) %>% 
       arrange(STATE, COUNTY)

mon.loc = df.pm25 %>% 
       select(location, cityURL, latitude, longitude) %>% 
       distinct(location, cityURL, latitude, longitude, .keep_all = TRUE) %>% 
       mutate(latitude2 = latitude, longitude2 = longitude) %>% 
       st_as_sf(coords = c("longitude2", "latitude2"), crs = 4326)   %>% 
       st_transform(crs = st_crs(102003)) 

mon.loc2 =st_join(mon.loc, CountyGIS, join = st_intersects) %>% 
       filter(!is.na(NAME)) %>% 
       st_drop_geometry() %>% 
       select(-c(GEO_ID, LSAD, CENSUSAREA))

mon.list = df.pm25 %>%
        data.frame() %>% 
       mutate(DateTime = with_tz(dateUTC, tz = 'America/Los_Angeles'), Date = date(DateTime) , Hour = hour(DateTime)) %>% 
       select(-c(DateTime, dateUTC, dateLocal, unit, parameter, country)) %>% 
       arrange(location, cityURL, latitude, longitude, Date, Hour) %>% 
       group_by(location, cityURL, latitude, longitude, Date) %>% 
       summarize(PM25 = mean(value, na.rm = TRUE), N_Hour = n()) %>% 
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
       tm_layout(main.title = 'PM2.5 Monitors from 1/9/2020 to 4/8/2020 (n = 1188) from ropenAQ',
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


## Merge with other variabels
load('Data/Jesse/Censusvariable.RData') #CensusRestriction.df
load('Data/Jesse/CovidData.RData') #Covid.df

df.pm25.master.temp  = df.pm25 %>% 
        distinct(location, cityURL, latitude, longitude, dateUTC, value, .keep_all = TRUE) %>% 
        inner_join(mon.loc2, by = c('location', 'cityURL', 'latitude', 'longitude'))   %>% 
        mutate(GEOID = paste0(STATE, COUNTY), Date2 = date(dateLocal), PM25 = value) %>% 
        left_join(CensusRestriction.df, by = 'GEOID') %>% 
        left_join(Covid.df, by = c('GEOID', 'Date2')) %>% 
        select(-c('STATE', 'COUNTY', 'country', 'city', 'unit', 'parameter', 'value', 'NAME')) 

df.pm25.master.daymean = df.pm25.master.temp %>% 
        filter(PM25 >= 0) %>% 
        group_by(location, cityURL, latitude, longitude, GEOID, Name, Date2) %>% 
        summarize(N_Hour = n(), PM25 = mean(PM25, na.rm = TRUE)) %>% 
        filter(N_Hour >= 8) %>% 
        ungroup()

# criteria is 3 observations per month (assuming every 10 days at lesat)
df.pm25.goodmonitor = df.pm25.master.daymean %>% 
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
        mutate(PM25MonitorID = row_number())

df.pm25.monitor = df.pm25.master.daymean %>% 
        inner_join(df.pm25.goodmonitor, by = c('location', 'cityURL', 'latitude', 'longitude', 'GEOID', 'Name')) %>% 
        left_join(CensusRestriction.df, by = c('GEOID' , 'Name')) %>% 
        left_join(Covid.df, by = c('GEOID', 'Date2')) %>% 
        arrange(PM25MonitorID, Date2) %>% 
        mutate(Name = as.character(Name), Stay.at.Home = as.Date(Stay.at.Home ), EduFacilClose = as.Date(EduFacilClose), NonEssentialServiceClose = as.Date(NonEssentialServiceClose), State = str_sub(Name, -2, -1)) %>% 
        select(PM25MonitorID, GEOID, Date2, State, Name, latitude, longitude, PM25, N_Hour, Pop2010:Deaths) %>% 
        data.frame()

table(df.pm25.monitor$State)
n_distinct(df.pm25.monitor$PM25MonitorID)
n_distinct(df.pm25.monitor$GEOID)

save(df.pm25.monitor, file = 'Data/Jesse/PM25MonitorDF.RData')
## Create county data

df.pm25.county = df.pm25.monitor %>% 
        group_by(GEOID, Date2) %>% 
        summarize(PM25 = mean(PM25, na.rm = TRUE)) %>% 
        ungroup() %>% 
        left_join(CensusRestriction.df, by = 'GEOID') %>% 
        left_join(Covid.df, by = c('GEOID', 'Date2')) %>% 
        mutate(State = str_sub(Name, -2, -1)) %>% 
        arrange(GEOID, Date2) %>% 
        select(GEOID, Date2, State, Name, PM25,  everything()) %>% 
        data.frame()

save(df.pm25.county, file = 'Data/Jesse/PM25CountyDF.RData')

## Create Past AQS Data
load('K:\\AirData\\OriginalData\\PM25_Data_20160120.RData')
df.pm25.county.pastyears = PM25_AQS %>% 
        mutate(GEOID = substr(FIPSPOC, 1,5)) %>% 
        group_by(GEOID, Date) %>% 
        summarize(PM25 = mean(PM25_Value, na.rm = TRUE)) %>% 
        ungroup() %>%
        filter(GEOID %in% unique(df.pm25.county$GEOID), as.numeric(month(Date)) %in% c(1:4), as.numeric(year(Date)) %in% c(2017:2019)) %>% 
        data.frame()

save(df.pm25.county.pastyears, file = 'Data/Jesse/PM25CountyPastYearDF.RData')

##
test = df.pm25.county %>% 
        mutate(Index = ifelse(as.Date(Date2) <  as.Date(EduFacilClose), 'Before', 'After')) %>% 
        filter(!is.na(Index)) %>% 
        group_by(GEOID, Index) %>% 
        #group_by(Urbanicity2, Index) %>% 
       # group_by(State, Index) %>% 
        summarize(PM25 = mean(PM25, na.rm = TRUE)) %>% 
        pivot_wider(names_from = Index, values_from = PM25) %>% 
        filter(!is.na(Before), !is.na(After)) %>% 
        mutate(Reduction = (1-After/Before)*100)
summary(test$Reduction)
filter(test, Reduction <0)
