pacman::p_load(tidyverse, sf, tmap, skimr, lubridate, stringr)

load('Data\\Jesse\\df_co.Rdata') #df.co
load('Data\\Jesse\\monitor_co.Rdata') #co.monitor

df.co %>% 
       distinct(location, cityURL, latitude, longitude) %>% 
       dim()

co.monitor %>% 
       distinct(location, cityURL, latitude, longitude) %>% 
       dim()
## Map
CountyGIS = st_read('M:\\Atesgis\\CMAQpm25_ER\\Data\\GISData\\gz_2010_us_050_00_5m.shp', stringsAsFactors = FALSE) %>% 
       filter(!(STATE %in% c('02', '15', '72'))) %>% 
       st_transform(crs = st_crs(102003)) %>% 
       arrange(STATE, COUNTY)

mon.loc = df.co %>% 
       select(location, cityURL, latitude, longitude) %>% 
       distinct(location, cityURL, latitude, longitude, .keep_all = TRUE) %>% 
       mutate(latitude2 = latitude, longitude2 = longitude) %>% 
       st_as_sf(coords = c("longitude2", "latitude2"), crs = 4326)   %>% 
       st_transform(crs = st_crs(102003)) 

mon.loc2 =st_join(mon.loc, CountyGIS, join = st_intersects) %>% 
       filter(!is.na(NAME)) %>% 
       st_drop_geometry() %>% 
       select(-c(GEO_ID, LSAD, CENSUSAREA))

mon.list = df.co %>%
       data.frame() %>% 
       mutate(DateTime = with_tz(dateUTC, tz = 'America/Los_Angeles'), Date = date(DateTime) , Hour = hour(DateTime)) %>% 
       select(-c(DateTime, dateUTC, dateLocal, unit, parameter, country)) %>% 
       arrange(location, cityURL, latitude, longitude, Date, Hour) %>% 
       group_by(location, cityURL, latitude, longitude, Date) %>% 
       summarize(co = mean(value, na.rm = TRUE), N_Hour = n()) %>% 
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
       tm_layout(main.title = 'CO Monitors from 1/9/2020 to 4/8/2020 (n = 179) from ropenAQ',
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

df.co.master.temp  = df.co %>% 
       distinct(location, cityURL, latitude, longitude, dateUTC, value, .keep_all = TRUE) %>% 
       inner_join(mon.loc2, by = c('location', 'cityURL', 'latitude', 'longitude'))   %>% 
       mutate(GEOID = paste0(STATE, COUNTY), Date2 = date(dateLocal), co = value) %>% 
       left_join(CensusRestriction.df, by = 'GEOID') %>% 
       left_join(Covid.df, by = c('GEOID', 'Date2')) %>% 
       select(-c('STATE', 'COUNTY', 'country', 'city', 'unit', 'parameter', 'value', 'NAME')) 

df.co.master.daymean = df.co.master.temp %>% 
       filter(co >= 0) %>% 
       group_by(location, cityURL, latitude, longitude, GEOID, Name, Date2) %>% 
       summarize(N_Hour = n(), co = mean(co, na.rm = TRUE)) %>% 
       filter(N_Hour >= 8) %>% 
       ungroup()

# criteria is 3 observations per month (assuming every 10 days at lesat)
df.co.goodmonitor = df.co.master.daymean %>% 
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
       mutate(coMonitorID = row_number())

df.co.monitor = df.co.master.daymean %>% 
       inner_join(df.co.goodmonitor, by = c('location', 'cityURL', 'latitude', 'longitude', 'GEOID', 'Name')) %>% 
       left_join(CensusRestriction.df, by = c('GEOID' , 'Name')) %>% 
       left_join(Covid.df, by = c('GEOID', 'Date2')) %>% 
       arrange(coMonitorID, Date2) %>% 
       mutate(Name = as.character(Name), Stay.at.Home = as.Date(Stay.at.Home ), EduFacilClose = as.Date(EduFacilClose), NonEssentialServiceClose = as.Date(NonEssentialServiceClose), State = str_sub(Name, -2, -1)) %>% 
       select(coMonitorID, GEOID, Date2, State, Name, latitude, longitude, co, N_Hour, Pop2010:Deaths) %>% 
       data.frame()

table(df.co.monitor$State)
n_distinct(df.co.monitor$coMonitorID)
n_distinct(df.co.monitor$GEOID)

#save(df.co.monitor, file = 'Data/Jesse/coMonitorDF.RData')
## Create county data

df.co.county = df.co.monitor %>% 
       group_by(GEOID, Date2) %>% 
       summarize(co = mean(co, na.rm = TRUE)) %>% 
       ungroup() %>% 
       left_join(CensusRestriction.df, by = 'GEOID') %>% 
       left_join(Covid.df, by = c('GEOID', 'Date2')) %>% 
       mutate(State = str_sub(Name, -2, -1)) %>% 
       arrange(GEOID, Date2) %>% 
       select(GEOID, Date2, State, Name, co,  everything()) %>% 
       data.frame()

#save(df.co.county, file = 'Data/Jesse/coCountyDF.RData')

## Create Past AQS Data
load('K:\\AirData\\OriginalData\\co_Data_20160120.RData')
df.co.county.pastyears = CO_AQS %>% 
       mutate(GEOID = substr(FIPSPOC, 1,5)) %>% 
       group_by(GEOID, Date) %>% 
       summarize(co = mean(CO_Value, na.rm = TRUE)) %>% 
       ungroup() %>%
       filter(GEOID %in% unique(df.co.county$GEOID), as.numeric(month(Date)) %in% c(1:4), as.numeric(year(Date)) %in% c(2017:2019)) %>% 
       data.frame()

#save(df.co.county.pastyears, file = 'Data/Jesse/coCountyPastYearDF.RData')

##
test = df.co.county %>% 
       mutate(Index = ifelse(as.Date(Date2) <  as.Date(EduFacilClose), 'Before', 'After')) %>% 
       filter(!is.na(Index)) %>% 
       #group_by(GEOID, Index) %>% 
       #group_by(Urbanicity2, Index) %>% 
        group_by(State, Index) %>% 
       summarize(co = mean(co, na.rm = TRUE)) %>% 
       pivot_wider(names_from = Index, values_from = co) %>% 
       filter(!is.na(Before), !is.na(After)) %>% 
       mutate(Reduction = (1-After/Before)*100)
summary(test$Reduction)
filter(test, Reduction <0)
