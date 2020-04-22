# 월별 기초통계량

summary(analysis_total_Fixed)

max_rain <- analysis_total_Fixed %>% filter(
  substr(일시,1,4)== '2018') %>% filter(
  `일강수량(mm)`>= 90)
  )
View(max_rain)

analysis_total_Fixed %>% 
  group_by(시도) %>% 
  summarise(PM10 = mean(PM10),
            PM25 = mean(PM25))

analysis_total_Fixed <- analysis_total_Fixed %>% 
  mutate(월 = substr(일시, 6, 7))


analysis_total_Fixed <- as.data.frame(analysis_total_Fixed)

str(analysis_total_Fixed)

month_aggregate <- analysis_total_Fixed %>% 
  group_by(시도,월) %>% 
  summarise(
    발생건수 = sum(발생건수),
    `평균기온(°C)` = mean(`평균기온(°C)`),
    일교차 = mean(`최고기온(°C)` - `최저기온(°C)`),
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
    발병률 = mean(발병률))

View(month_aggregate)

ggplot(analysis_total_Fixed, aes(x=월, y=PM10)) +
  geom_boxplot()

ggplot(analysis_total_Fixed, aes(x=월, y=PM25)) +
  geom_boxplot()

ggplot(analysis_total_Fixed, aes(x=월, y=`평균기온(°C)`)) +
  geom_boxplot()

ggplot(analysis_total_Fixed, aes(x=월, y=`평균 풍속(m/s)`)) +
  geom_boxplot()

ggplot()