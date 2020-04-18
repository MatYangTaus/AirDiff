pacman::p_load(tidyverse, lubridate, skimr, fs, ggthemes, nord)

file.list = fs::dir_ls('Data/NO2/')

df.no2 = file.list  %>% 
       map_dfr(read_csv)

WeekendName = c('Sat', 'Sun')

df.no2_2 = df.no2 %>% 
       filter(!is.na(longitude), substr(summary_date, 6, 7) == '03') %>% 
       select(-c('units', 'basin', 'state')) %>% 
       mutate(date = date(summary_date), DOW = lubridate::wday(date, label = TRUE), Year = year(date), Weekend = ifelse(DOW %in% WeekendName, 'Weekend', 'Weekday'), day = day(date), no2_davg = no2_davg*1000)

#write.csv(df.no2_2, 'Data/Jesse/NO2.csv') 

df.no2_2 %>%  
       filter(day(date) >= 15) %>% 
       filter(Weekend == 'Weekday') %>% 
       group_by(name, Year) %>% 
       summarize(NO2 = mean(no2_davg, na.rm = TRUE))  %>% 
       arrange(name, Year)

df.no2_2 %>%  
       filter(day(date) >= 15) %>% 
       group_by(name, Year, Weekend) %>% 
       summarize(NO2 = mean(no2_davg), SD = sd(no2_davg))  %>% 
       arrange(name, Weekend, Year) %>% 
        #write.csv('Result/NO2.csv') %>% 
        {.}

df.no2_2 %>% 
       filter(day(date) >= 15, Weekend == 'Weekday', !is.na(no2_davg)) %>% 
       ggplot(aes(x = day, y = no2_davg, color = as.factor(Year))) +
       geom_line() +
       geom_point() +
       facet_wrap(~name) +
#       theme_minimal() +
        theme_fivethirtyeight()

df.no2_2 %>%  
        filter(day(date) >= 15, !is.na(no2_davg)) %>%
        filter(no2_davg >= 4.1) %>% 
        ggplot(aes(x = as.factor(Weekend), y = no2_davg, fill = as.factor(Year))) +
                geom_boxplot(position=position_dodge(1)) +
                facet_wrap(~name) +
                ggtitle(expression(NO[2]*' Distribution by Weekday/Weekend during March 16-31 in 2019 & 2020')) +
                #theme_minimal()  
                labs(fill = 'Year') + 
                ylab(expression(NO[2]*' (ppb)')) +
                #theme_fivethirtyeight() +
                theme_bw() +
              #  scale_fill_nord("halifax_harbor") +
                scale_fill_manual(values=c("#56B4E9", "#E69F00")) +
                theme(plot.title = element_text(hjust = 0.5, size = 20),
                 #     strip.background =element_rect(fill="white"),
                      axis.text = element_text(size = 14),
                      axis.title.x = element_blank(),
                      axis.title.y = element_text(size = 16),
                      strip.text.x = element_text(size = 18),
                      legend.title=element_text(size = 18),
                      legend.text=element_text(size = 18),
                      legend.position = 'bottom')

df.no2_2 %>%  
        filter(Year == 2020, !is.na(no2_davg)) %>% 
        mutate(Period = ifelse(day %in% c(1:15), 'First (1-15)', 'Second (16-30)')) %>% 
        #filter(no2_davg >= 4.1) %>% 
        ggplot(aes(x = as.factor(Weekend), y = no2_davg, fill = as.factor(Period))) +
        geom_boxplot(position=position_dodge(1)) +
        facet_wrap(~name) +
        ggtitle(expression(NO[2]*' Distribution by Weekday/Weekend during March 1-15 vs 16-31 in 2020')) +
        #theme_minimal()  
        labs(fill = 'Period') + 
        ylab(expression(NO[2]*' (ppb)')) +
        #theme_fivethirtyeight() +
        theme_bw() +
        #  scale_fill_nord("halifax_harbor") +
        scale_fill_manual(values=c("#56B4E9", "#E69F00")) +
        theme(plot.title = element_text(hjust = 0.5, size = 20),
              #     strip.background =element_rect(fill="white"),
              axis.text = element_text(size = 14),
              axis.title.x = element_blank(),
              axis.title.y = element_text(size = 16),
              strip.text.x = element_text(size = 18),
              legend.title=element_text(size = 18),
              legend.text=element_text(size = 18),
              legend.position = 'bottom')
