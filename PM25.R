pacman::p_load(tidyverse, lubridate, skimr, fs)

file.list = fs::dir_ls('Data/PM25/')

df.pm = file.list  %>% 
       map_dfr(read_csv)

WeekendName = c('Sat', 'Sun')

df.pm2 = df.pm %>% 
       filter(!is.na(longitude), substr(summary_date, 6, 7) == '03') %>% 
       select(-c('units', 'basin', 'state', 'monitor')) %>% 
       mutate(date = date(summary_date), DOW = lubridate::wday(date, label = TRUE), Year = year(date), Weekend = ifelse(DOW %in% WeekendName, 'Weekend', 'Weekday'), day = day(date)) %>% 
        {.}

#write.csv(df.pm2, 'Data/Jesse/PM25.csv') 

table(df.pm2$date, df.pm2$site)

df.pm2 %>%  
       filter(day(date) >= 15) %>% 
       filter(Weekend == 'Weekday') %>% 
       group_by(name, Year) %>% 
       summarize(PM25 = mean(pm25_davg, na.rm = TRUE))  %>% 
       arrange(name, Year)

df.pm2 %>%  
       filter(day(date) >= 15) %>% 
       group_by(name, Year, Weekend) %>% 
       summarize(PM25 = mean(pm25_davg, na.rm = TRUE), SD = sd(pm25_davg))  %>% 
       arrange(name, Weekend, Year) %>% 
       #write.csv('Result/PM25.csv') %>% 
        {.}

df.pm2 %>% 
       filter(day(date) >= 15, Weekend == 'Weekday', !is.na(pm25_davg)) %>% 
       ggplot(aes(x = day, y = pm25_davg, color = as.factor(Year))) +
               geom_line() +
               geom_point() +
               facet_wrap(~name) +
               theme_minimal()

df.pm2 %>%  
        filter(day(date) >= 15, !is.na(pm25_davg)) %>% 
        ggplot(aes(x = as.factor(Weekend), y = pm25_davg, fill = as.factor(Year))) +
                geom_boxplot(position=position_dodge(1)) +
                facet_wrap(~name) +
                ggtitle(expression(PM[2.5]*' Distribution by Weekday/Weekend during March 16-31 in 2019 & 2020')) +
                #theme_minimal()  
                labs(fill = 'Year') + 
                ylab(expression(PM[2.5]*' ('*mu*'g/'*m^3*')')) +
                theme_bw() +
                scale_fill_manual(values=c("#56B4E9", "#E69F00")) +
                theme(plot.title = element_text(hjust = 0.5, size = 20),
                      axis.text = element_text(size = 14),
                      axis.title.x = element_blank(),
                      axis.title.y = element_text(size = 16),
                      strip.text.x = element_text(size = 18),
                      legend.title=element_text(size = 18),
                      legend.text=element_text(size = 18),
                      legend.position = 'bottom')


df.pm2 %>%  
        filter(Year == 2020, !is.na(pm25_davg)) %>% 
        mutate(Period = ifelse(day %in% c(1:15), 'First (1-15)', 'Second (16-30)')) %>% 
        ggplot(aes(x = as.factor(Weekend), y = pm25_davg, fill = as.factor(Period))) +
        geom_boxplot(position=position_dodge(1)) +
        facet_wrap(~name) +
        ggtitle(expression(PM[2.5]*' Distribution by Weekday/Weekend during March 1-15 vs 16-31 in 2020')) +
        #theme_minimal()  
        labs(fill = 'Period') + 
        ylab(expression(PM[2.5]*' ('*mu*'g/'*m^3*')')) +
        theme_bw() +
        scale_fill_manual(values=c("#56B4E9", "#E69F00")) +
        theme(plot.title = element_text(hjust = 0.5, size = 20),
              axis.text = element_text(size = 14),
              axis.title.x = element_blank(),
              axis.title.y = element_text(size = 16),
              strip.text.x = element_text(size = 18),
              legend.title=element_text(size = 18),
              legend.text=element_text(size = 18),
              legend.position = 'bottom')
