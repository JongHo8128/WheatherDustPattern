library(kormaps2014)
library(ggiraphExtra)
library(ggplot2)
library(data.table)
library(dplyr)

load("refinedata/analysis/analysis_total_Fixed.rda")

analysis_total <- analysis_total_Fixed


analysis_sido <- analysis_total %>% group_by(시도코드,시도) %>% summarise(`평균기온(°C)` = mean(`평균기온(°C)`,na.rm=TRUE),
                                                                    `평균 풍속(m/s)` = mean(`평균 풍속(m/s)`,na.rm=TRUE),
                                                                    `평균 현지기압(hPa)` = mean(`평균 현지기압(hPa)`,na.rm=TRUE),
                                                                    `일 최심신적설(cm)` = mean(`일 최심신적설(cm)`,na.rm=TRUE),
                                                                    `일강수량(mm)` = mean(`일강수량(mm)`,na.rm=TRUE),
                                                                    `강수 계속시간(hr)` = mean(`강수 계속시간(hr)`,na.rm=TRUE),
                                                                    SO2 = mean(SO2,na.rm=TRUE),
                                                                    CO = mean(CO,na.rm=TRUE),
                                                                    O3 = mean(O3,na.rm=TRUE),
                                                                    NO2 = mean(NO2,na.rm=TRUE),
                                                                    PM10 = mean(PM10,na.rm=TRUE),
                                                                    PM25 = mean(PM25,na.rm=TRUE),
                                                                    발생건수 = sum(발생건수),
                                                                    발생율 = sum(발생건수)/sum(인구수))
tempmap <- kormap1 

tempmap


analysis_sido$code <- as.character(analysis_sido$시도코드)
analysis_sido$발생건수 <- as.character(analysis_sido$발생건수)



code <- c('42','41','43','44','30','47','48','45','46','11','28','27','31','29','26','49','36') 
name1 <- c('강원도','경기도','충청북도','충청남도','대전광역시','경상북도','경상남도','전라북도','전라남도','서울특별시','인천광역시','대구광역시','울산광역시','광주광역시','부산광역시','제주특별자치도','세종특별자치시')
df_sido <- data.frame("code"=code,"name1"=name1)

temp_map_join <- inner_join(tempmap,df_sido,by=c('name1'))

temp_map_join <- temp_map_join %>% select(-code.x)
temp_map_join <- rename(temp_map_join,code=code.y)
temp_map_join$code <- as.character(temp_map_join$code)


temp_map_join$region <- temp_map_join$code
temp_map_join$SIDO_CD <- temp_map_join$code
temp_map_join
View(kormap1)

df_sido$name <- as.character(df_sido$name)

analysis_sido$발생건수 <- as.numeric(analysis_sido$발생건수)
analysis_sido$시도 <- analysis_sido$시도
analysis_sido <- analysis_sido %>% arrange(desc(발생건수))
analysis_sido$발생율 <- analysis_sido$발생율 * 10000











changeCode(analysis_sido)
changeCode(kormap1)

korpop1 <- rename(korpop1, pop=총인구_명, name=행정구역별_읍면동)
View(analysis_sido)
analysis_sido

ggChoropleth(data=analysis_sido, 
             
             aes(fill=PM10, 
                 
                 map_id=code,
                 
                 tooltip=시도
                 
                 
                 ),
             #palette = '',
             map=temp_map_join, 
             
             interactive=TRUE)





