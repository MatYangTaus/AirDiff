pacman::p_load(tidyverse)

Census.df = read.csv('Data/Jesse/County_2010Census-Revised.csv') %>% 
       select(GEOID, Name, Pop2010 = DP0180001, NCHS_2013, Urbanicity, Urbanicity2) %>% 
       mutate(GEOID = str_sub(paste0('0', GEOID), -5, -1)) %>% 
       arrange(GEOID)

Restriction.df = read.csv('Data/Jesse/PM25_cnty_2020.csv') %>% 
        mutate(STATEFIPS = str_sub(paste0('0', GEOID),-5, -4)) %>% 
       distinct(STATEFIPS, .keep_all = TRUE) %>% 
       select(STATEFIPS, Stay.at.Home, EduFacilClose, NonEssentialServiceClose) %>% 
       arrange(STATEFIPS)

CensusRestriction.df = Census.df %>% 
        mutate(STATEFIPS = substr(GEOID, 1, 2)) %>% 
        left_join(Restriction.df, by = 'STATEFIPS') %>% 
        select(-STATEFIPS)

save(CensusRestriction.df , file = 'Data/Jesse/Censusvariable.RData')

Covid.df = read.csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv') %>% 
        #read.csv('Data/Jesse/PM25_cnty_2020.csv') %>% 
        mutate(GEOID = str_sub(paste0('0', fips), -5, -1), Date2 = as.Date(date)) %>% 
        select(GEOID, Date2, Cases = cases, Deaths = deaths) %>% 
        #mutate(GEOID = str_sub(paste0('0', GEOID), -5, -1), Date2 = as.Date(Date2)) %>% 
        arrange(GEOID, Date2)

save(Covid.df , file = 'Data/Jesse/CovidData.RData')
