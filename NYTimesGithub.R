pacman::p_load(tidyverse, lubridate, skimr, fs)

df.nyt = read.csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv') %>% 
       #filter(county == 'San Francisco')
       filter(county == 'Alameda')

df.nyt2 = read.csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv') %>% 
   #    filter(state == 'New York')
       filter(state == 'California')
