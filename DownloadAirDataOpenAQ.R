#install.packages("ropenaq")
#library(ropenaq)
pacman::p_load(tidyverse, ropenaq)


## NO2 
no2.monitor <- aq_locations(country="US", parameter="no2")
# Pay attntion in running. Covered period will be different (donwloade day 4/9/2020)
result1.no2 <- aq_measurements(country="US", date_from = '2020-01-08',date_to = '2020-01-20', parameter="no2")
result2.no2 <- aq_measurements(country="US", date_from = '2020-01-21',date_to = '2020-01-31', parameter="no2")
result3.no2 <- aq_measurements(country="US", date_from = '2020-02-01',date_to = '2020-02-10', parameter="no2")
result4.no2 <- aq_measurements(country="US", date_from = '2020-02-11',date_to = '2020-02-20', parameter="no2")
result5.no2 <- aq_measurements(country="US", date_from = '2020-02-21',date_to = '2020-02-29', parameter="no2")
result6.no2 <- aq_measurements(country="US", date_from = '2020-03-01',date_to = '2020-03-10', parameter="no2")
result7.no2 <- aq_measurements(country="US", date_from = '2020-03-11',date_to = '2020-03-20', parameter="no2")
result8.no2 <- aq_measurements(country="US", date_from = '2020-03-21',date_to = '2020-03-31', parameter="no2")
result9.no2 <- aq_measurements(country="US", date_from = '2020-04-01',date_to = '2020-04-09', parameter="no2")

df.no2 = rbind(result1.no2, result2.no2, result3.no2, result4.no2, result5.no2, result6.no2, result7.no2, result8.no2, result9.no2) %>% 
       arrange(location, dateUTC)

#save(no2.monitor, file = 'Data\\Jesse\\monitor_no2.RData')
#save(df.no2, file = 'Data\\Jesse\\df_no2.RData')

## PM2.5
pm25.monitor <- aq_locations(country="US", parameter="pm25")
# Pay attntion in running. Covered period will be different (donwloade day 4/9/2020)
result1.pm25 <- aq_measurements(country="US", date_from = '2020-01-08',date_to = '2020-01-20', parameter="pm25")
result2.pm25 <- aq_measurements(country="US", date_from = '2020-01-21',date_to = '2020-01-31', parameter="pm25")
result3.pm25 <- aq_measurements(country="US", date_from = '2020-02-01',date_to = '2020-02-10', parameter="pm25")
result4.pm25 <- aq_measurements(country="US", date_from = '2020-02-11',date_to = '2020-02-20', parameter="pm25")
result5.pm25 <- aq_measurements(country="US", date_from = '2020-02-21',date_to = '2020-02-29', parameter="pm25")
result6.pm25 <- aq_measurements(country="US", date_from = '2020-03-01',date_to = '2020-03-10', parameter="pm25")
result7.pm25 <- aq_measurements(country="US", date_from = '2020-03-11',date_to = '2020-03-20', parameter="pm25")
result8.pm25 <- aq_measurements(country="US", date_from = '2020-03-21',date_to = '2020-03-31', parameter="pm25")
result9.pm25 <- aq_measurements(country="US", date_from = '2020-04-01',date_to = '2020-04-09', parameter="pm25")

df.pm25 = rbind(result1.pm25, result2.pm25, result3.pm25, result4.pm25, result5.pm25, result6.pm25, result7.pm25, result8.pm25, result9.pm25) %>% 
       arrange(location, dateUTC)

table(df.pm25$location) %>% 
       data.frame() %>% 
       arrange(Freq)

#save(pm25.monitor, file = 'Data\\Jesse\\monitor_pm25.RData')
#save(df.pm25, file = 'Data\\Jesse\\df_pm25.RData')

## O3
o3.monitor <- aq_locations(country="US", parameter="o3")
# Pay attntion in running. Covered period will be different (donwloade day 4/9/2020)
result1.o3 <- aq_measurements(country="US", date_from = '2020-01-08',date_to = '2020-01-20', parameter="o3")
result2.o3 <- aq_measurements(country="US", date_from = '2020-01-21',date_to = '2020-01-31', parameter="o3")
result3.o3 <- aq_measurements(country="US", date_from = '2020-02-01',date_to = '2020-02-10', parameter="o3")
result4.o3 <- aq_measurements(country="US", date_from = '2020-02-11',date_to = '2020-02-20', parameter="o3")
result5.o3 <- aq_measurements(country="US", date_from = '2020-02-21',date_to = '2020-02-29', parameter="o3")
result6.o3 <- aq_measurements(country="US", date_from = '2020-03-01',date_to = '2020-03-10', parameter="o3")
result7.o3 <- aq_measurements(country="US", date_from = '2020-03-11',date_to = '2020-03-20', parameter="o3")
result8.o3 <- aq_measurements(country="US", date_from = '2020-03-21',date_to = '2020-03-31', parameter="o3")
result9.o3 <- aq_measurements(country="US", date_from = '2020-04-01',date_to = '2020-04-09', parameter="o3")

df.o3 = rbind(result1.o3, result2.o3, result3.o3, result4.o3, result5.o3, result6.o3, result7.o3, result8.o3, result9.o3) %>% 
       arrange(location, dateUTC)

table(df.o3$location) %>% 
       data.frame() %>% 
       arrange(Freq)

#save(o3.monitor, file = 'Data\\Jesse\\monitor_o3.RData')
#save(df.o3, file = 'Data\\Jesse\\df_o3.RData')

## CO
co.monitor <- aq_locations(country="US", parameter="co")
# Pay attntion in running. Covered period will be different (donwloade day 4/9/2020)
result1.co <- aq_measurements(country="US", date_from = '2020-01-08',date_to = '2020-01-20', parameter="co")
result2.co  <- aq_measurements(country="US", date_from = '2020-01-21',date_to = '2020-01-31', parameter="co")
result3.co  <- aq_measurements(country="US", date_from = '2020-02-01',date_to = '2020-02-10', parameter="co")
result4.co  <- aq_measurements(country="US", date_from = '2020-02-11',date_to = '2020-02-20', parameter="co")
result5.co  <- aq_measurements(country="US", date_from = '2020-02-21',date_to = '2020-02-29', parameter="co")
result6.co  <- aq_measurements(country="US", date_from = '2020-03-01',date_to = '2020-03-10', parameter="co")
result7.co  <- aq_measurements(country="US", date_from = '2020-03-11',date_to = '2020-03-20', parameter="co")
result8.co  <- aq_measurements(country="US", date_from = '2020-03-21',date_to = '2020-03-31', parameter="co")
result9.co  <- aq_measurements(country="US", date_from = '2020-04-01',date_to = '2020-04-09', parameter="co")

df.co = rbind(result1.co, result2.co, result3.co, result4.co, result5.co, result6.co, result7.co, result8.co, result9.co) %>% 
       arrange(location, dateUTC)

table(df.co$location) %>% 
       data.frame() %>% 
       arrange(Freq)

#save(co.monitor, file = 'Data\\Jesse\\monitor_co.RData')
#save(df.co, file = 'Data\\Jesse\\df_co.RData')

## SO2
so2.monitor <- aq_locations(country="US", parameter="so2")
# Pay attntion in running. Covered period will be different (donwloade day 4/9/2020)
result1.so2 <- aq_measurements(country="US", date_from = '2020-01-08',date_to = '2020-01-20', parameter="so2")
result2.so2 <- aq_measurements(country="US", date_from = '2020-01-21',date_to = '2020-01-31', parameter="so2")
result3.so2 <- aq_measurements(country="US", date_from = '2020-02-01',date_to = '2020-02-10', parameter="so2")
result4.so2 <- aq_measurements(country="US", date_from = '2020-02-11',date_to = '2020-02-20', parameter="so2")
result5.so2 <- aq_measurements(country="US", date_from = '2020-02-21',date_to = '2020-02-29', parameter="so2")
result6.so2 <- aq_measurements(country="US", date_from = '2020-03-01',date_to = '2020-03-10', parameter="so2")
result7.so2 <- aq_measurements(country="US", date_from = '2020-03-11',date_to = '2020-03-20', parameter="so2")
result8.so2 <- aq_measurements(country="US", date_from = '2020-03-21',date_to = '2020-03-31', parameter="so2")
result9.so2 <- aq_measurements(country="US", date_from = '2020-04-01',date_to = '2020-04-09', parameter="so2")

df.so2 = rbind(result1.so2, result2.so2, result3.so2, result4.so2, result5.so2, result6.so2, result7.so2, result8.so2, result9.so2) %>% 
       arrange(location, dateUTC)

table(df.so2$location) %>% 
       data.frame() %>% 
       arrange(Freq)

#save(so2.monitor, file = 'Data\\Jesse\\monitor_so2.RData')
#save(df.so2, file = 'Data\\Jesse\\df_so2.RData')

## SO2
pm10.monitor <- aq_locations(country="US", parameter="pm10")
# Pay attntion in running. Covered period will be different (donwloade day 4/9/2020)
result1.pm10 <- aq_measurements(country="US", date_from = '2020-01-08',date_to = '2020-01-20', parameter="pm10")
result2.pm10 <- aq_measurements(country="US", date_from = '2020-01-21',date_to = '2020-01-31', parameter="pm10")
result3.pm10 <- aq_measurements(country="US", date_from = '2020-02-01',date_to = '2020-02-10', parameter="pm10")
result4.pm10 <- aq_measurements(country="US", date_from = '2020-02-11',date_to = '2020-02-20', parameter="pm10")
result5.pm10 <- aq_measurements(country="US", date_from = '2020-02-21',date_to = '2020-02-29', parameter="pm10")
result6.pm10 <- aq_measurements(country="US", date_from = '2020-03-01',date_to = '2020-03-10', parameter="pm10")
result7.pm10 <- aq_measurements(country="US", date_from = '2020-03-11',date_to = '2020-03-20', parameter="pm10")
result8.pm10 <- aq_measurements(country="US", date_from = '2020-03-21',date_to = '2020-03-31', parameter="pm10")
result9.pm10 <- aq_measurements(country="US", date_from = '2020-04-01',date_to = '2020-04-09', parameter="pm10")

df.pm10 = rbind(result1.pm10, result2.pm10, result3.pm10, result4.pm10, result5.pm10, result6.pm10, result7.pm10, result8.pm10, result9.pm10) %>% 
       arrange(location, dateUTC)

table(df.pm10$location) %>% 
       data.frame() %>% 
       arrange(Freq)

#save(pm10.monitor, file = 'Data\\Jesse\\monitor_pm10.RData')
#save(df.pm10, file = 'Data\\Jesse\\df_pm10.RData')
