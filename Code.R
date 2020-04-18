pacman::p_load(tidyverse, lubridate, skimr)

year2019 = read.csv('Data/NO2_PICKDATA_2019-1-1.csv') %>% 
       filter(substr(date, 6, 7) == '03')  %>% 
       mutate(NO2 = value * 1000) %>% 
       select(- c(units, prelim, quality)) %>% 
       mutate(site = as.character(site), date = date(date), DOW = wday(date, label = TRUE))  

year2019.2 = year2019 %>% 
       group_by(site, date) %>% 
#       arrange(-NO2) %>% 
#       slice(1) %>% 
        summarize(NO2 = mean(NO2)) %>% 
        ungroup() %>% 
        mutate(DOW = wday(date, label = TRUE))  

year2020 = read.csv('Data/NO2_PICKDATA_2020-3-26.csv') %>% 
       filter(substr(date, 6, 7) == '03')  %>% 
       mutate(NO2 = value * 1000) %>% 
       select(- c(units, prelim, quality)) %>% 
       mutate(site = as.character(site), date = date(date), DOW = wday(date, label = TRUE))

year2020.2 = year2020 %>% 
       group_by(site, date) %>% 
       #arrange(-NO2) %>% 
       #slice(1) %>% 
       summarize(NO2 = mean(NO2)) %>% 
        ungroup() %>% 
        mutate(DOW = wday(date, label = TRUE))  

WeekendName = c('Sat', 'Sun')
df = rbind(year2019.2, year2020.2) %>% 
       mutate(Year = year(date), Weekend = ifelse(DOW %in% WeekendName, 'Weekend', 'Weekday'), day = day(date))
#      substr(date, 1, 4), Day = as.numeric(substr(date, 9, 10)))

df %>%  
        filter(day(date) >= 15) %>% 
       group_by(site, Year, Weekend) %>% 
       summarize(NO2 = mean(NO2)) 

df %>% 
        filter(day(date) >= 15) %>% 
       ggplot(aes(x = day, y = NO2, color = as.factor(Year))) +
       geom_line() +
       geom_point() +
       facet_wrap(~site) +
       theme_minimal()

