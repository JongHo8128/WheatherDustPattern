### 그룹 1 회귀분석

library(dplyr)

## 그룹 1 분류

analysis_1 <- analysis_total_Fixed %>% filter(시도 %in% c('전남' ,'제주'))

analysis_1 <- as.data.frame(analysis_1)


## 2016년 1분기

# 필요변수 생성 및 추출

analysis_1_2016_quarter1 <- analysis_1 %>% 
  mutate(발생률 = 발생건수/인구수 * 10000) %>% 
  filter(substr(일시,1,4) == '2016' & substr(일시,6,7) %in% c('01','02'))

View(analysis_1_2016_quarter1)

analysis_1_2016_quarter1 <- 
  analysis_1_2016_quarter1 %>% 
    dplyr::select(-시도코드,-발생건수,-PM10등급,-PM25등급,-인구수,-년도,-`최다풍향(16방위)`,-시도,-일시)

# 회귀분석

fit1 <- lm(data = analysis_1_2016_quarter1, formula = 발생률 ~ .)
fit2 <- lm(data = analysis_1_2016_quarter1, formula = 발생률 ~ 1)

library(MASS)
stepAIC(fit1, direction = 'backward')
stepAIC(fit2, direction = 'forward', scope = list(upper = fit1,lower = fit2))
stepAIC(fit2, direction = 'both', scope = list(upper = fit1,lower = fit2))

lm.fit <- lm(formula = 발생률 ~ NO2 + `평균 풍속(m/s)` + O3 + PM10, 
             data = analysis_1_2016_quarter1)

  # backward 최적화모델이 가장 AIC값이 낮긴하지만
  # 평균기온과 최저기온의 VIF값이 높아 다중공선성
  # 존재할 가능성이 있음
  # 따라서, both 방식으로 나온 최적화모델 채택

cor(analysis_1_2016_quarter1)

summary(lm.fit)

par(mfrow = c(2, 2))
plot(lm.fit)

library(car)
vif(lm.fit)


## 2016년 2분기

# 필요변수 생성 및 추출

analysis_1_2016_quarter2 <- analysis_1 %>% 
  mutate(발생률 = 발생건수/인구수 * 10000) %>% 
  filter(substr(일시,1,4) == '2016' & substr(일시,6,7) %in% c('03','04','05'))

View(analysis_1_2016_quarter2)

analysis_1_2016_quarter2 <- 
  analysis_1_2016_quarter2 %>% 
  dplyr::select(-시도코드,-발생건수,-PM10등급,-PM25등급,-인구수,-년도,-`최다풍향(16방위)`,-시도,-일시)

# 회귀분석

fit1 <- lm(data = analysis_1_2016_quarter2, formula = 발생률 ~ .)
fit2 <- lm(data = analysis_1_2016_quarter2, formula = 발생률 ~ 1)

stepAIC(fit1, direction = 'backward')
stepAIC(fit2, direction = 'forward', scope = list(upper = fit1,lower = fit2))
stepAIC(fit2, direction = 'both', scope = list(upper = fit1,lower = fit2))

lm.fit <- lm(formula = 발생률 ~ `최저기온(°C)` + `평균 풍속(m/s)` + 
               SO2 + O3 + NO2 + PM10, data = analysis_1_2016_quarter2)

  # backward 사용

cor(analysis_1_2016_quarter2)

summary(lm.fit)

par(mfrow = c(2, 2))
plot(lm.fit)

vif(lm.fit)


## 2016년 3분기

# 필요변수 생성 및 추출

analysis_1_2016_quarter3 <- analysis_1 %>% 
  mutate(발생률 = 발생건수/인구수 * 10000) %>% 
  filter(substr(일시,1,4) == '2016' & substr(일시,6,7) %in% c('06','07','08'))

View(analysis_1_2016_quarter3)

analysis_1_2016_quarter3 <- 
  analysis_1_2016_quarter3 %>% 
  dplyr::select(-시도코드,-발생건수,-PM10등급,-PM25등급,-인구수,-년도,-`최다풍향(16방위)`,-시도,-일시)

# 회귀분석

fit1 <- lm(data = analysis_1_2016_quarter3, formula = 발생률 ~ .)
fit2 <- lm(data = analysis_1_2016_quarter3, formula = 발생률 ~ 1)

stepAIC(fit1, direction = 'backward')
stepAIC(fit2, direction = 'forward', scope = list(upper = fit1,lower = fit2))
stepAIC(fit2, direction = 'both', scope = list(upper = fit1,lower = fit2))

lm.fit <- lm(formula = 발생률 ~ `평균기온(°C)` + PM10 + PM25 + 
               NO2 + SO2 + `평균 풍속(m/s)` + `강수 계속시간(hr)`, 
             data = analysis_1_2016_quarter3)

  # both 사용

cor(analysis_1_2016_quarter3)

summary(lm.fit)

par(mfrow = c(2, 2))
plot(lm.fit)

vif(lm.fit)


## 2016년 4분기

# 필요변수 생성 및 추출

analysis_1_2016_quarter4 <- analysis_1 %>% 
  mutate(발생률 = 발생건수/인구수 * 10000) %>% 
  filter(substr(일시,1,4) == '2016' & substr(일시,6,7) %in% c('09','10','11'))

View(analysis_1_2016_quarter4)

analysis_1_2016_quarter4 <- 
  analysis_1_2016_quarter4 %>% 
  dplyr::select(-시도코드,-발생건수,-PM10등급,-PM25등급,-인구수,-년도,-`최다풍향(16방위)`,-시도,-일시)

# 회귀분석

cor(analysis_1_2016_quarter4)

fit1 <- lm(data = analysis_1_2016_quarter4, formula = 발생률 ~ .)
fit2 <- lm(data = analysis_1_2016_quarter4, formula = 발생률 ~ 1)

stepAIC(fit1, direction = 'backward')
stepAIC(fit2, direction = 'forward', scope = list(upper = fit1,lower = fit2))
stepAIC(fit2, direction = 'both', scope = list(upper = fit1,lower = fit2))

lm.fit <- lm(formula = 발생률 ~ `평균기온(°C)` + NO2 + SO2 + `평균 풍속(m/s)`, 
             data = analysis_1_2016_quarter4)

  # both 사용

summary(lm.fit)

par(mfrow = c(2, 2))
plot(lm.fit)

vif(lm.fit)


## 2017년 1분기

analysis_1_2017_quarter1 <- analysis_1 %>% 
  mutate(발생률 = 발생건수/인구수 * 10000) %>% 
  filter((substr(일시,1,4) == '2016' & substr(일시,6,7) %in% c('12')) |
           (substr(일시,1,4) == '2017' & substr(일시,6,7) %in% c('01','02'))
         )

View(analysis_1_2017_quarter1)

analysis_1_2017_quarter1 <- 
  analysis_1_2017_quarter1 %>% 
  dplyr::select(-시도코드,-발생건수,-PM10등급,-PM25등급,-인구수,-년도,-`최다풍향(16방위)`,-시도,-일시)

# 회귀분석

fit1 <- lm(data = analysis_1_2017_quarter1, formula = 발생률 ~ .)
fit2 <- lm(data = analysis_1_2017_quarter1, formula = 발생률 ~ 1)

stepAIC(fit1, direction = 'backward')
stepAIC(fit2, direction = 'forward', scope = list(upper = fit1,lower = fit2))
stepAIC(fit2, direction = 'both', scope = list(upper = fit1,lower = fit2))

cor(analysis_1_2017_quarter1)

lm.fit <- lm(formula = 발생률 ~ `최저기온(°C)` + O3 + `평균기온(°C)` + 
               `평균 현지기압(hPa)` + `평균 풍속(m/s)` + NO2, 
             data = analysis_1_2017_quarter1)

  # both 사용

summary(lm.fit)

par(mfrow = c(2, 2))
plot(lm.fit)

vif(lm.fit)


## 2017년 2분기

analysis_1_2017_quarter2 <- analysis_1 %>% 
  mutate(발생률 = 발생건수/인구수 * 10000) %>% 
  filter(
    (substr(일시,1,4) == '2017' & substr(일시,6,7) %in% c('03','04','05'))
  )

View(analysis_1_2017_quarter2)

analysis_1_2017_quarter2 <- 
  analysis_1_2017_quarter2 %>% 
  dplyr::select(-시도코드,-발생건수,-PM10등급,-PM25등급,-인구수,-년도,-`최다풍향(16방위)`,-시도,-일시)

# 회귀분석

fit1 <- lm(data = analysis_1_2017_quarter2, formula = 발생률 ~ .)
fit2 <- lm(data = analysis_1_2017_quarter2, formula = 발생률 ~ 1)

stepAIC(fit1, direction = 'backward')
stepAIC(fit2, direction = 'forward', scope = list(upper = fit1,lower = fit2))
stepAIC(fit2, direction = 'both', scope = list(upper = fit1,lower = fit2))

cor(analysis_1_2017_quarter2)

lm.fit <- lm(formula = 발생률 ~ `평균 풍속(m/s)` + `일강수량(mm)` + 
               `강수 계속시간(hr)` + SO2 + NO2, data = analysis_1_2017_quarter2)

  # backward 사용

summary(lm.fit)

par(mfrow = c(2, 2))
plot(lm.fit)

vif(lm.fit)



## 2017년 3분기

analysis_1_2017_quarter3 <- analysis_1 %>% 
  mutate(발생률 = 발생건수/인구수 * 10000) %>% 
  filter(
    (substr(일시,1,4) == '2017' & substr(일시,6,7) %in% c('06','07','08'))
  )

View(analysis_1_2017_quarter3)

analysis_1_2017_quarter3 <- 
  analysis_1_2017_quarter3 %>% 
  dplyr::select(-시도코드,-발생건수,-PM10등급,-PM25등급,-인구수,-년도,-`최다풍향(16방위)`,-시도,-일시)

# 회귀분석

cor(analysis_1_2017_quarter3)

fit1 <- lm(data = analysis_1_2017_quarter3, formula = 발생률 ~ .)
fit2 <- lm(data = analysis_1_2017_quarter3, formula = 발생률 ~ 1)

stepAIC(fit1, direction = 'backward')
stepAIC(fit2, direction = 'forward', scope = list(upper = fit1,lower = fit2))
stepAIC(fit2, direction = 'both', scope = list(upper = fit1,lower = fit2))

lm.fit <- lm(formula = 발생률 ~ `평균기온(°C)` + SO2 + NO2 + `평균 풍속(m/s)` + 
               `강수 계속시간(hr)`, data = analysis_1_2017_quarter3)
  
  # both 사용
summary(lm.fit)

par(mfrow = c(2, 2))
plot(lm.fit)

vif(lm.fit)


## 2017년 4분기

analysis_1_2017_quarter4 <- analysis_1 %>% 
  mutate(발생률 = 발생건수/인구수 * 10000) %>% 
  filter(
    (substr(일시,1,4) == '2017' & substr(일시,6,7) %in% c('09','10','11'))
  )

View(analysis_1_2017_quarter4)

analysis_1_2017_quarter4 <- 
  analysis_1_2017_quarter4 %>% 
  dplyr::select(-시도코드,-발생건수,-PM10등급,-PM25등급,-인구수,-년도,-`최다풍향(16방위)`,-시도,-일시)

# 회귀분석

cor(analysis_1_2017_quarter4)

fit1 <- lm(data = analysis_1_2017_quarter4, formula = 발생률 ~ .)
fit2 <- lm(data = analysis_1_2017_quarter4, formula = 발생률 ~ 1)

stepAIC(fit1, direction = 'backward')
stepAIC(fit2, direction = 'forward', scope = list(upper = fit1,lower = fit2))
stepAIC(fit2, direction = 'both', scope = list(upper = fit1,lower = fit2))

lm.fit <- lm(formula = 발생률 ~ `평균기온(°C)` + NO2 + SO2 + `평균 현지기압(hPa)` + 
               O3 + `최저기온(°C)` + `강수 계속시간(hr)` + PM25, 
             data = analysis_1_2017_quarter4)

  # both 사용

summary(lm.fit)

par(mfrow = c(2, 2))
plot(lm.fit)

vif(lm.fit)


## 2018년 1분기

analysis_1_2018_quarter1 <- analysis_1 %>% 
  mutate(발생률 = 발생건수/인구수 * 10000) %>% 
  filter((substr(일시,1,4) == '2017' & substr(일시,6,7) %in% c('12')) |
           (substr(일시,1,4) == '2018' & substr(일시,6,7) %in% c('01','02'))
  )

View(analysis_1_2018_quarter1)

analysis_1_2018_quarter1 <- 
  analysis_1_2018_quarter1 %>% 
  dplyr::select(-시도코드,-발생건수,-PM10등급,-PM25등급,-인구수,-년도,-`최다풍향(16방위)`,-시도,-일시)

# 회귀분석

fit1 <- lm(data = analysis_1_2018_quarter1, formula = 발생률 ~ .)
fit2 <- lm(data = analysis_1_2018_quarter1, formula = 발생률 ~ 1)

stepAIC(fit1, direction = 'backward')
stepAIC(fit2, direction = 'forward', scope = list(upper = fit1,lower = fit2))
stepAIC(fit2, direction = 'both', scope = list(upper = fit1,lower = fit2))

cor(analysis_1_2018_quarter1)

lm.fit <- lm(formula = 발생률 ~ O3 + SO2 + PM25, data = analysis_1_2018_quarter1)

  # backward, forward, both 세가지 방식 모두
  # 최적화 모델이 같음
  # ** NO2 유일하게 안나옴 **

summary(lm.fit)

par(mfrow = c(2, 2))
plot(lm.fit)

vif(lm.fit)


## 2018년 2분기

analysis_1_2018_quarter2 <- analysis_1 %>% 
  mutate(발생률 = 발생건수/인구수 * 10000) %>% 
  filter(
    (substr(일시,1,4) == '2018' & substr(일시,6,7) %in% c('03','04','05'))
  )

View(analysis_1_2018_quarter2)

analysis_1_2018_quarter2 <- 
  analysis_1_2018_quarter2 %>% 
  dplyr::select(-시도코드,-발생건수,-PM10등급,-PM25등급,-인구수,-년도,-`최다풍향(16방위)`,-시도,-일시)

# 회귀분석

fit1 <- lm(data = analysis_1_2018_quarter2, formula = 발생률 ~ .)
fit2 <- lm(data = analysis_1_2018_quarter2, formula = 발생률 ~ 1)

stepAIC(fit1, direction = 'backward')
stepAIC(fit2, direction = 'forward', scope = list(upper = fit1,lower = fit2))
stepAIC(fit2, direction = 'both', scope = list(upper = fit1,lower = fit2))

cor(analysis_1_2018_quarter2)

lm.fit <- lm(formula = 발생률 ~ SO2 + NO2 + PM25 + `일강수량(mm)`, 
             data = analysis_1_2018_quarter2)

  # backward에서 나온 최적화모델이 AIC값이 가장 낮긴 하지만
  # 최저,최고,평균기온이 모두 포함 되어있어 다중공선성이 생김
  # 따라서, both 방식을 사용하여 모델 채택

summary(lm.fit)

par(mfrow = c(2, 2))
plot(lm.fit)

vif(lm.fit)



## 2018년 3분기

analysis_1_2018_quarter3 <- analysis_1 %>% 
  mutate(발생률 = 발생건수/인구수 * 10000) %>% 
  filter(
    (substr(일시,1,4) == '2018' & substr(일시,6,7) %in% c('06','07','08'))
  )

View(analysis_1_2018_quarter3)

analysis_1_2018_quarter3 <- 
  analysis_1_2018_quarter3 %>% 
  dplyr::select(-시도코드,-발생건수,-PM10등급,-PM25등급,-인구수,-년도,-`최다풍향(16방위)`,-시도,-일시)

# 회귀분석

cor(analysis_1_2018_quarter3)

fit1 <- lm(data = analysis_1_2018_quarter3, formula = 발생률 ~ .)
fit2 <- lm(data = analysis_1_2018_quarter3, formula = 발생률 ~ 1)

stepAIC(fit1, direction = 'backward')
stepAIC(fit2, direction = 'forward', scope = list(upper = fit1,lower = fit2))
stepAIC(fit2, direction = 'both', scope = list(upper = fit1,lower = fit2))

lm.fit <- lm(formula = 발생률 ~ `최고기온(°C)` + `평균 풍속(m/s)` + 
               `일강수량(mm)` + SO2 + CO + NO2, data = analysis_1_2018_quarter3)

  # backward 사용

summary(lm.fit)

par(mfrow = c(2, 2))
plot(lm.fit)

vif(lm.fit)


## 2018년 4분기

analysis_1_2018_quarter4 <- analysis_1 %>% 
  mutate(발생률 = 발생건수/인구수 * 10000) %>% 
  filter(
    (substr(일시,1,4) == '2018' & substr(일시,6,7) %in% c('09','10','11'))
  )

View(analysis_1_2018_quarter4)

analysis_1_2018_quarter4 <- 
  analysis_1_2018_quarter4 %>% 
  dplyr::select(-시도코드,-발생건수,-PM10등급,-PM25등급,-인구수,-년도,-`최다풍향(16방위)`,-시도,-일시)

# 회귀분석

cor(analysis_1_2018_quarter4)

fit1 <- lm(data = analysis_1_2018_quarter4, formula = 발생률 ~ .)
fit2 <- lm(data = analysis_1_2018_quarter4, formula = 발생률 ~ 1)

stepAIC(fit1, direction = 'backward')
stepAIC(fit2, direction = 'forward', scope = list(upper = fit1,lower = fit2))
stepAIC(fit2, direction = 'both', scope = list(upper = fit1,lower = fit2))

lm.fit <- lm(formula = 발생률 ~ `평균 풍속(m/s)` + `일강수량(mm)` + 
               SO2 + NO2 + PM25, data = analysis_1_2018_quarter4)

summary(lm.fit)

  # both 사용

par(mfrow = c(2, 2))
plot(lm.fit)

vif(lm.fit)