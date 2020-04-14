# 일별 시계열 그래프 비교

library(ggplot2)
library(dplyr)
library(tidyr)

load('../../refinedata/analysis/analysis_total_Fixed.rda')

analysis_total_Fixed <-  as.data.frame(analysis_total_Fixed)

analysis_total <- analysis_total_Fixed %>% 
  group_by(일시) %>% 
  summarise(`평균기온(°C)` = mean(`평균기온(°C)`),
            일교차 = mean(`최고기온(°C)` - `최저기온(°C)`),
            `평균 풍속(m/s)` = mean(`평균 풍속(m/s)`),
            `평균 현지기압(hPa)` = mean(`평균 현지기압(hPa)`),
            `일강수량(mm)` = mean(`일강수량(mm)`),
            SO2 = mean(SO2),
            CO = mean(CO),
            O3 = mean(O3),
            NO2 = mean(NO2),
            PM10 = mean(PM10),
            PM25 = mean(PM25),
            발병률 = sum(발병률)*1000)

# 평균기온, PM10, 발병률

df <- analysis_total %>%
  dplyr::select(일시,  `평균기온(°C)`, PM10, 발병률) %>% 
  gather(key = "variable", value = "value", -일시)

ggplot(df, aes(x = 일시, y = value)) + 
  geom_line(aes(color = variable), size = 1) + 
  # scale_color_manual(values = c("#00AFBB", "#E7B800", )) +
  theme_minimal()

# PM10, PM25

df <- analysis_total %>%
  dplyr::select(일시,  PM10, PM25) %>% 
  gather(key = "variable", value = "value", -일시)

ggplot(df, aes(x = 일시, y = value)) + 
  geom_line(aes(color = variable), size = 1) + 
  # scale_color_manual(values = c("#00AFBB", "#E7B800", )) +
  theme_minimal()




df <- analysis_total %>%
  dplyr::select(일시,  O3, 발병률) %>% 
  gather(key = "variable", value = "value", -일시)

ggplot(df, aes(x = 일시, y = value)) + 
  geom_line(aes(color = variable), size = 1) + 
  # scale_color_manual(values = c("#00AFBB", "#E7B800", )) +
  theme_minimal()


df <- analysis_total %>%
  dplyr::select(일시,  CO, 발병률) %>% 
  gather(key = "variable", value = "value", -일시)

ggplot(df, aes(x = 일시, y = value)) + 
  geom_line(aes(color = variable), size = 1) + 
  # scale_color_manual(values = c("#00AFBB", "#E7B800", )) +
  theme_minimal()


df <- analysis_total %>%
  dplyr::select(일시,  PM25, 발병률) %>% 
  gather(key = "variable", value = "value", -일시)

ggplot(df, aes(x = 일시, y = value)) + 
  geom_line(aes(color = variable), size = 1) + 
  # scale_color_manual(values = c("#00AFBB", "#E7B800", )) +
  theme_minimal()

df <- analysis_total %>%
  dplyr::select(일시,  일교차, 발병률) %>% 
  gather(key = "variable", value = "value", -일시)

ggplot(df, aes(x = 일시, y = value)) + 
  geom_line(aes(color = variable), size = 1) + 
  # scale_color_manual(values = c("#00AFBB", "#E7B800", )) +
  theme_minimal()



# 월별 시계열 그래프 비교

analysis_total_Fixed <-  as.data.frame(analysis_total_Fixed)

analysis_total <- analysis_total_Fixed %>% 
  group_by(substr(일시,1,7)) %>% 
  summarise(`평균기온(°C)` = mean(`평균기온(°C)`),
            일교차 = mean(`최고기온(°C)` - `최저기온(°C)`),
            `평균 풍속(m/s)` = mean(`평균 풍속(m/s)`),
            `평균 현지기압(hPa)` = mean(`평균 현지기압(hPa)`),
            `일강수량(mm)` = mean(`일강수량(mm)`),
            SO2 = mean(SO2),
            CO = mean(CO),
            O3 = mean(O3),
            NO2 = mean(NO2),
            PM10 = mean(PM10),
            PM25 = mean(PM25),
            발병률 = sum(발병률))

analysis_total <- rename(analysis_total,일시=`substr(일시, 1, 7)`)
analysis_total$일시 <- paste0(analysis_total$일시,'-01')
analysis_total$일시 <- as.Date(analysis_total$일시,tryFormats = '%Y-%m-%d')

# 평균기온

df <- analysis_total %>%
  dplyr::select(일시, PM10 ,PM25) %>% 
  gather(key = "variable", value = "value", -일시)

ggplot(df, aes(x = 일시, y = value)) + 
  geom_line(aes(color = variable), size = 1) + 
  # scale_color_manual(values = c("#00AFBB", "#E7B800", )) +
  theme_minimal()


# 평균기온, PM10, 발병률

df <- analysis_total %>%
  dplyr::select(일시,  `평균기온(°C)`, PM10, 발병률) %>% 
  gather(key = "variable", value = "value", -일시)

ggplot(df, aes(x = 일시, y = value)) + 
  geom_line(aes(color = variable), size = 1) + 
  # scale_color_manual(values = c("#00AFBB", "#E7B800", )) +
  theme_minimal()

#

df <- analysis_total %>%
  dplyr::select(일시,  `평균 풍속(m/s)`, 발병률) %>% 
  gather(key = "variable", value = "value", -일시)

ggplot(df, aes(x = 일시, y = value)) + 
  geom_line(aes(color = variable), size = 1) + 
  # scale_color_manual(values = c("#00AFBB", "#E7B800", )) +
  theme_minimal()

#

df <- analysis_total %>%
  dplyr::select(일시,  `평균 현지기압(hPa)`, 발생건수) %>% 
  gather(key = "variable", value = "value", -일시)

ggplot(df, aes(x = 일시, y = value)) + 
  geom_line(aes(color = variable), size = 1) + 
  # scale_color_manual(values = c("#00AFBB", "#E7B800", )) +
  theme_minimal()

#

df <- analysis_total %>%
  dplyr::select(일시,  `일강수량(mm)`, 발생건수) %>% 
  gather(key = "variable", value = "value", -일시)

ggplot(df, aes(x = 일시, y = value)) + 
  geom_line(aes(color = variable), size = 1) + 
  # scale_color_manual(values = c("#00AFBB", "#E7B800", )) +
  theme_minimal()

#

df <- analysis_total %>%
  dplyr::select(일시,  NO2, 발병률) %>% 
  gather(key = "variable", value = "value", -일시)

ggplot(df, aes(x = 일시, y = value)) + 
  geom_line(aes(color = variable), size = 1) + 
  # scale_color_manual(values = c("#00AFBB", "#E7B800", )) +
  theme_minimal()

#

df <- analysis_total %>%
  dplyr::select(일시,  SO2, 발병률) %>% 
  gather(key = "variable", value = "value", -일시)

ggplot(df, aes(x = 일시, y = value)) + 
  geom_line(aes(color = variable), size = 1) + 
  # scale_color_manual(values = c("#00AFBB", "#E7B800", )) +
  theme_minimal()

#

df <- analysis_total %>%
  dplyr::select(일시,  O3, 발병률) %>% 
  gather(key = "variable", value = "value", -일시)

ggplot(df, aes(x = 일시, y = value)) + 
  geom_line(aes(color = variable), size = 1) + 
  # scale_color_manual(values = c("#00AFBB", "#E7B800", )) +
  theme_minimal()

#

df <- analysis_total %>%
  dplyr::select(일시,  CO, 발병률) %>% 
  gather(key = "variable", value = "value", -일시)

ggplot(df, aes(x = 일시, y = value)) + 
  geom_line(aes(color = variable), size = 1) + 
  # scale_color_manual(values = c("#00AFBB", "#E7B800", )) +
  theme_minimal()

#

df <- analysis_total %>%
  dplyr::select(일시,  PM25, 발병률) %>% 
  gather(key = "variable", value = "value", -일시)

ggplot(df, aes(x = 일시, y = value)) + 
  geom_line(aes(color = variable), size = 1) + 
  # scale_color_manual(values = c("#00AFBB", "#E7B800", )) +
  theme_minimal()

# 

df <- analysis_total %>%
  dplyr::select(일시, O3, NO2, SO2) %>% 
  gather(key = "variable", value = "value", -일시)

ggplot(df, aes(x = 일시, y = value)) + 
  geom_line(aes(color = variable), size = 1) + 
  # scale_color_manual(values = c("#00AFBB", "#E7B800", )) +
  theme_minimal()

#

# 

df <- analysis_total %>%
  dplyr::select(일시, 일교차, 발병률) %>% 
  gather(key = "variable", value = "value", -일시)

ggplot(df, aes(x = 일시, y = value)) + 
  geom_line(aes(color = variable), size = 1) + 
  # scale_color_manual(values = c("#00AFBB", "#E7B800", )) +
  theme_minimal()

#


