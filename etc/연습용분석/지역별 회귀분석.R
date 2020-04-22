library(dplyr)

View(analysis_total_Fixed)

df_concat <- analysis_total_Fixed

table(is.na(df_concat$ `최고기온(°C)`))
(df_concat %>% filter(is.na(`최고기온(°C)`)))
temp <-df_concat %>%  filter(시도코드 == '11' & substr(일시,1,7)=='2017-10'&(!is.na(`최고기온(°C)`))) 
temp <- mean(temp$`최고기온(°C)`)
df_concat$`최고기온(°C)` <-ifelse(((df_concat$시도코드=='11')&(df_concat$일시=='2017-10-12')),temp,df_concat$`최고기온(°C)`)
df_concat$`최고기온(°C)`[is.na(df_concat$`최고기온(°C)`)] <- temp

table(is.na(df_concat$ `평균기온(°C)`))
(df_concat %>% filter(is.na(`평균기온(°C)`)))
temp <-df_concat %>%  filter(시도코드=='27' & substr(일시,1,7)=='2017-07'&(!is.na(`평균기온(°C)`))) 
temp <- mean(temp$`평균기온(°C)`)
df_concat$`평균기온(°C)`[is.na(df_concat$`평균기온(°C)`)] <- temp

analysis_total_Fixed <- df_concat
table(is.na(analysis_total_Fixed))

save(analysis_total_Fixed,file = 'refinedata/analysis/analysis_total_Fixed.rda')

lm_total <- analysis_total_Fixed %>% 
  mutate(`100명당 발생건수` = 발생건수/인구수*100) %>% 
  group_by(시도) %>% 
  summarise(
    `평균기온(°C)` = mean(`평균기온(°C)`),
    `최저기온(°C)` = mean(`최저기온(°C)`),	
    `최고기온(°C)` = mean(`최고기온(°C)`),
    `평균 풍속(m/s)` = mean(`평균 풍속(m/s)`),
    `평균 현지기압(hPa)` = mean(`평균 현지기압(hPa)`),
    `일 최심신적설(cm)` = mean(`일 최심신적설(cm)`),
    `일강수량(mm)` = mean(`일강수량(mm)`),
    `강수 계속시간(hr)` = mean(`강수 계속시간(hr)`),
    SO2 = mean(SO2),	
    CO = mean(CO),	
    O3 = mean(O3),	
    NO2 = mean(NO2),	
    PM10 = mean(PM10),	
    PM25 = mean(PM25),	
    `100명당 발생건수` = sum(`100명당 발생건수`)
    )

lm_Fixed <- lm_total[-1]
(rownames(lm_Fixed) <- lm_total$시도)

View(lm_Fixed)  

lm.fit <- lm(data = lm_vif_Fixed, formula = `100명당 발생건수` ~ .)

summary(lm.fit)
library(car)

vif(lm.fit)
lm_vif_Fixed <- lm_Fixed[c(-1,-2,-4,-6,-7,-8,-11,-12,-14)]
cor(lm_vif_Fixed)
