pacman::p_load(tidyverse, stringr)
######################
############Download AQS Site
######################
NO2_AQS=data.frame()

test2=c(2017:2019)
ptm <- proc.time()

for (i in 1:length(test2)){  	
       url=paste("https://aqs.epa.gov/aqsweb/airdata/hourly_42602_",test2[i],".zip",sep='')
       download.file(url,'temp2.zip')
       temp=read.csv(unz('temp2.zip',paste("hourly_42602_",test2[i],".csv",sep='')),header=TRUE)
       names(temp)=c('StateCode','CountyCode','SiteID','Parameter','POC','Latitude','Longitude','Datum','Name', 'DateLocal', 'TimeLocal', 'DateGMT', 'TimeGMT', 'value', 'Unit', 'MDL', 'Uncertainty', 'Qualifier', 'MethodType', 'MethodCode', 'MethodName', 'StateName', 'CountyName', 'DateChange')
       
       temp2 = temp %>% 
              mutate(StateCode = str_sub(paste0('00', StateCode), -2, -1),
                     CountyCode = str_sub(paste0('000', CountyCode), -3, -1),
                     SiteID = str_sub(paste0('0000', SiteID), -4, -1),
                     POC = str_sub(paste0('00', POC), -2, -1),
                     DateLocal = as.Date(DateLocal),
                     TimeLocal = as.numeric(substr(TimeLocal, 1, 2)),
                     GEOID.POC = paste0(StateCode, CountyCode, SiteID, POC),
                     GEOID = paste0(StateCode, CountyCode, SiteID)
                     ) %>% 
              filter(as.numeric(substr(DateLocal, 6, 7)) %in% c(1:4)) %>% 
              select(-c(DateGMT, TimeGMT, Unit, Uncertainty, Qualifier, MethodType, MethodCode, MethodName, DateChange))
       NO2_AQS = rbind(NO2_AQS, temp2)
       rm(url,temp,temp2)
}
proc.time() - ptm #This takes about 16 min

summary(NO2_AQS$value)

NO2.monitor = NO2_AQS %>% 
       distinct(GEOID.POC, .keep_all = TRUE)

NO2.previousyear = NO2_AQS %>% 
       filter(value >= 0) %>% 
       group_by(GEOID.POC, DateLocal) %>% 
       summarize(NO2 = max(value, na.rm = TRUE), N_Hour = n()) %>% 
       ungroup() %>% 
       mutate(GEOID.site = substr(GEOID.POC, 1, 9)) %>% 
       group_by(GEOID.site, DateLocal) %>% 
       summarize(NO2Mean = mean(NO2, na.rm = TRUE)) %>% 
       ungroup() %>% 
       mutate(GEOID = substr(GEOID.site, 1,5)) %>% 
       group_by(GEOID, DateLocal) %>% 
       summarize(NO2 = mean(NO2Mean, na.rm = TRUE)) %>% 
       ungroup()

save(NO2.previousyear, file = 'Data/Jesse/NO2AQSPastYearDF.RData' )
