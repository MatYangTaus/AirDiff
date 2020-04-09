pacman::p_load(tidyverse, skimr, sf, ggthemes)

#df = read.csv('us-states.csv', colClasses = c("fips" = "character", "date" = "Date")) 
df = read.csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv', colClasses = c("fips" = "character", "date" = "Date"))

latest.df = df %>% 
    group_by(state) %>% 
    arrange(desc(date)) %>% 
    slice(1) %>% 
    ungroup() %>% 
    filter(cases > 1200) %>% 
    arrange(desc(cases))

slice(latest.df, 1:15)

df %>% 
#  filter(fips == '06') %>% 
  filter(state %in% latest.df$state, date > as.Date("2020-02-25")) %>% 
  group_by(state) %>%  
  mutate(New.Case = cases - lag(cases), New.Death = deaths - lag(deaths)) %>%
  ungroup() %>% 
  filter(!is.na(New.Case)) %>% 
  ggplot(aes(x = date, y = New.Case)) +
#  ggplot(aes(x = date, y = New.Death)) +
    geom_bar(stat = 'identity', fill = 'steelblue') +
    facet_wrap(~state, scales = 'free') +
    theme_minimal()

df %>%
  filter(state %in% latest.df$state, date > as.Date("2020-02-25")) %>% 
  #group_by(state) %>%
  #arrange(date) %>%
  #mutate(cum.cases = cumsum(cases)) %>%
  #ungroup() %>%
  #group_by(type, Province.State, date) %>%
  #summarize(cum.cases2 = sum(cum.cases)) %>%
  #ungroup() %>%
  ggplot(aes(x = date, y = (cases))) + #, color = type)) +
    geom_point() +
    geom_line() +
    facet_wrap(~state, scales = 'free') +
    theme_fivethirtyeight()

df %>%
  filter(state %in% latest.df$state, date > as.Date("2020-02-25")) %>% 
  ggplot(aes(x = date, y = log(cases), color = state)) + #, color = type)) +
  geom_point() +
  geom_line() +
#  facet_wrap(~state, scales = 'free') +
  theme_fivethirtyeight()

# Case by County

#df2 = read.csv('us-counties.csv', colClasses = c("fips" = "character", "date" = "Date"))
df2 = read.csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv', colClasses = c("fips" = "character", "date" = "Date"))

Bay.Area = c('041', '097', '055', '095', '013', '001', '085', '087', '081', '075', '037')
#Bay.Area = c('013', '001',  '075')

df2 %>% 
  filter(state == 'California', date > as.Date("2020-02-25")) %>% 
  filter(substr(fips, 3, 5) %in% Bay.Area) %>% 
#  filter(county == 'San Francisco') %>% 
  group_by(county) %>%  
  mutate(New.Case = cases - lag(cases)) %>% 
  filter(!is.na(New.Case)) %>% 
  ungroup() %>% 
  ggplot(aes(x = date, y = New.Case)) +
      #geom_line() +
      #geom_point() +
      geom_bar(stat = 'identity', fill = 'steelblue') +
      facet_wrap(~county, scales = 'free') +
      #theme_minimal() +
      geom_vline(xintercept = as.Date('2020-03-16')) + 
      geom_text(mapping = aes(label = 'Shelter in place declared', x = as.Date('2020-03-16'), y = -0.75), angle = 0, hjust = 0) +
      theme_fivethirtyeight()

df2 %>% 
#  filter(state == 'California',date == as.Date("2020-03-30")) %>% 
  filter(county == 'Santa Clara') %>% 
  arrange(-cases)