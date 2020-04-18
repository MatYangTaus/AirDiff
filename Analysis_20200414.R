pacman::p_load(tidyverse, sf, tmap)
options(dplyr.width = Inf)

load('Data/Jesse/NO2CountyPastYearDF.RData')
load('Data/Jesse/NO2CountyDF.RData')
n_distinct(df.no2.county$GEOID)
n_distinct(df.no2.county.pastyears$GEOID)

BasicInfo = df.no2.county %>% 
       distinct(GEOID, .keep_all = TRUE) %>% 
       select(GEOID, Pop2010:NonEssentialServiceClose) %>% 
       mutate(Closed.Timing = ifelse(as.Date(NonEssentialServiceClose) >= as.Date('2020-03-25') | is.na(NonEssentialServiceClose),'Later', 'Early'))

ref.period = c(seq(as.Date('2017-01-09'),as.Date('2017-03-12'), 'day'),
              seq(as.Date('2018-01-09'),as.Date('2018-03-12'), 'day'),
              seq(as.Date('2019-01-09'),as.Date('2019-03-12'), 'day'))

ref.period2 = c(seq(as.Date('2017-03-13'),as.Date('2017-04-08'), 'day'),
               seq(as.Date('2018-03-13'),as.Date('2018-04-08'), 'day'),
               seq(as.Date('2019-03-13'),as.Date('2019-04-08'), 'day'))

Close.date = df.no2.county.pastyears %>% 
 #      filter(as.numeric(substr(Date, 6, 7)) %in%  c(3:4)) %>% 
       filter(Date %in% ref.period2) %>% 
       rename(Date2 = Date) %>% 
       mutate(Period = 'Past')

df = df.no2.county %>% 
       mutate(Period = 'Current') %>% 
       filter(as.Date(Date2) >= as.Date('2020-03-13')) %>% 
       select(GEOID, Date2, NO2, Period) %>% 
       rbind(Close.date) %>% 
       mutate(Year = substr(Date2, 1, 4)) %>% 
       inner_join(BasicInfo, by = 'GEOID') 

df %>% 
       group_by(Period) %>% 
       summarize(NO2 = mean(NO2, na.rm = TRUE))

df %>% 
       ggplot(aes(x = Period, y = NO2)) +
              geom_boxplot()
              
df %>% 
#       filter(GEOID == '06083') %>% 
       ggplot(aes(x = Year, y = NO2)) +
              geom_boxplot() +
              theme_minimal()

df %>% 
       group_by(GEOID, Period) %>% 
       summarize(NO2 = mean(NO2, na.rm = TRUE)) %>% 
       pivot_wider(names_from = Period, values_from = NO2) %>% 
       filter(!is.na(Current), !is.na(Past)) %>% 
       inner_join(BasicInfo, by = 'GEOID') %>% 
       ggplot() +
              geom_point(aes(x = Past, y = Current, color = Urbanicity2), size =2) +
             # facet_wrap(~Stay.at.Home) +
              geom_abline(slope = 1) +
              xlim(0, 40) +
              ylim(0, 40) +
              ggtitle(expression(NO[2]*' comparison (March 13th ~ April 8th) ')) +
              theme_bw() +
              theme(plot.title = element_text(hjust = 0.5))
       
df %>% 
       filter(!is.na(NO2)) %>% 
       ggplot(aes(x = Urbanicity2, y = NO2, color = Period)) +
              geom_boxplot() +
              theme_minimal()

df %>% 
       filter(!is.na(NO2)) %>% 
       group_by(Urbanicity2, Period) %>% 
       summarize(NO2Ave = mean(NO2)) %>% 
       pivot_wider(names_from = Period, values_from = NO2Ave) %>% 
       

plot.data = df %>% 
       group_by(GEOID, Period) %>% 
       summarize(NO2 = mean(NO2, na.rm = TRUE)) %>% 
       pivot_wider(names_from = Period, values_from = NO2) %>% 
       filter(!is.na(Current), !is.na(Past)) %>% 
       inner_join(BasicInfo, by = 'GEOID') %>% 
        ungroup()

dim(plot.data)
summary(plot.data$Current)
summary(plot.data$Past)

t.test(plot.data$Current, plot.data$Past, paired = TRUE)

plot.data.early = plot.data %>% 
       filter(Closed.Timing == 'Early')
t.test(plot.data.early$Current, plot.data.early$Past, paired = TRUE)

plot.data.later = plot.data %>% 
       filter(Closed.Timing == 'Later')
t.test(plot.data.later$Current, plot.data.later$Past, paired = TRUE)

ggplot(data = plot.data) +
       geom_point(aes(x = Past, y = Current, color = Urbanicity), size =2) +
       geom_abline(slope = 1) +
       facet_wrap(~Closed.Timing) +
       xlim(0, 40) +
       ylim(0, 40) +
       ggtitle(expression(NO[2]*' comparison (March 13th ~ April 8th) stratified by Non-essential Closure Status')) +
       ylab(expression('Mean '*PM[2.5]*' during Covid 2020')) +
       xlab(expression('Historical Mean '*PM[2.5])) +
       theme_bw() +
       theme(plot.title = element_text(hjust = 0.5))

plot.data %>% 
       mutate(Diff.Abs = Current - Past, Diff.Pct = (Current - Past)*100/Past) %>% 
       ggplot() +
              geom_point(aes(x = Diff.Abs, y = Diff.Pct, color = Urbanicity2), size = 2) +
#              facet_wrap(~Closed.Timing) +
              facet_wrap(~Stay.at.Home) +
              geom_vline(xintercept = 0) +
              geom_hline(yintercept = 0) +
              theme_bw()
       
plot.data %>% 
       mutate(Diff.Abs = Current - Past, Diff.Pct = (Current - Past)*100/Past) %>% 
       filter(Diff.Abs > 1.0)

filter(df.no2.county, GEOID == '06083') %>% 
       ggplot() +
              geom_point(aes(x = Date2, y = NO2), size = 2) +
              geom_line(aes(x = Date2, y = NO2)) +
              theme_minimal()

#Map
CountyGIS = st_read('M:\\Atesgis\\CMAQPM25_ER\\Data\\GISData\\gz_2010_us_050_00_5m.shp', stringsAsFactors = FALSE) %>% 
        filter(!(STATE %in% c('02', '15', '72'))) %>% 
        st_transform(crs = st_crs(102003)) %>% 
        arrange(STATE, COUNTY) %>% 
        mutate(GEOID = paste0(STATE, COUNTY))

plot.data2 = CountyGIS %>% 
        inner_join(plot.data, by = 'GEOID') %>% 
        mutate(Diff.Abs = Current - Past, Diff.Pct = (Current - Past)*100/Past)
       
map1 = tm_shape(plot.data2) +
        tm_polygons('Diff.Pct', n = 5, palette = c("blue", "red"), style = 'pretty') +
                    #, palette = c("blue", "red"), pal = 'YlOrRd',
                #size = 1, title = 'Number of observation days') + 
        tm_shape(CountyGIS) +
        tm_borders() +
        tm_layout(main.title = 'NO2 % Difference between Historical and 2020',
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
map1

### PM2.5      
load('Data/Jesse/PM25CountyPastYearDF.RData')
load('Data/Jesse/PM25CountyDF.RData')              
n_distinct(df.pm25.county$GEOID)
n_distinct(df.pm25.county.pastyears$GEOID)