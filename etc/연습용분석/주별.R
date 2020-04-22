library(ggplot2)
library(dplyr)
library(tidyr)
library(gvlma)
library(FinCal)

load('refinedata/analysis/analysis_total_Fixed.rda')






# 시도 고려 X

total <- analysis_total %>%
  group_by(주) %>% summarise(
    `평균기온(°C)` = geometric.mean(`평균기온(°C)`),
    `평균 풍속(m/s)` = geometric.mean(`평균 풍속(m/s)`),
    `평균 현지기압(hPa)` = geometric.mean(`평균 현지기압(hPa)`),
    `일강수량(mm)` = geometric.mean(`일강수량(mm)`),
    SO2 = geometric.mean(SO2),
    CO = geometric.mean(CO),
    O3 = geometric.mean(O3),
    NO2 = geometric.mean(NO2),
    PM10 = geometric.mean(PM10),
    PM25 = geometric.mean(PM25),
    발병률  = sum(발병률)
  )

fit <- lm(data = total[c(-1,-2,-11)], formula = 발병률 ~ .)
summary(fit)
library(car)
vif(fit)

fit <- lm(data = total[-1], formula = 발병률 ~ SO2 + CO + O3 + NO2 + PM10)
summary(fit)

vif(fit)



# 주별 시계열 그래프 및 시계열 자료형을 통한 회귀분석

total <- as.data.frame(total)
ts_total <- ts(total[-1])
View(ts)

# 발병률
str(ts)

ts <- ts(total$발병률, start = c(2016, 1), freq = 52)

fit <- stl(ts, s.window = 'periodic')
plot(fit)

# 미세먼지

ts <- ts(total$PM10, start = c(2016, 1), freq = 52)

fit <- stl(ts, s.window = 'periodic')
plot(fit)

# PM25

ts <- ts(total$PM25, start = c(2016, 1), freq = 52)

fit <- stl(ts, s.window = 'periodic')
plot(fit)

# O3

str(total)

args(lag)
ts_temperature <- ts(total$`평균기온(°C)`, start = c(2016, 1), freq = 52)
ts_air <- ts(total$`평균 현지기압(hPa)`, start = c(2016, 1), freq = 52)
ts_wind <- ts(total$`평균 풍속(m/s)`, start = c(2016, 1), freq = 52)
ts_rain <- ts(total$`일강수량(mm)`, start = c(2016, 1), freq = 52)
ts_CO <- ts(total$CO, start = c(2016, 1), freq = 52)
ts_발병률 <- ts(total$발병률, start = c(2016, 1), freq = 52)
ts_NO2 <- ts(total$NO2, start = c(2016, 1), freq = 52)
ts_SO2 <- ts(total$SO2, start = c(2016, 1), freq = 52)
ts_O3 <- ts(total$O3, start = c(2016, 1), freq = 52)
ts_PM10 <- ts(total$PM10, start = c(2016, 1), freq = 52)
ts_PM25 <- ts(total$PM25, start = c(2016, 1), freq = 52)


ts_lag <- stats::lag(ts, k = 12)
ts_lag

#
args(lag)
View(ts)
fit <- lm(ts_발병률 ~ ts_temperature + ts_air + ts_rain + ts_wind + ts_CO + ts_SO2 + ts_NO2 + ts_O3 + ts_PM10 + ts_PM25)

par(mfrow = c(2,2))
plot(fit)
summary(fit)
library(gvlma)
gvlma(fit)
vif(fit)

fit <- lm(formula = ts_발병률 ~ ts_air + ts_wind + ts_CO + ts_SO2 + 
             ts_NO2 + ts_O3)
par(mfrow = c(2,2))
plot(fit)
summary(fit)
library(gvlma)
gvlma(fit)
vif(fit)



fit <- stl(ts_lag, s.window = 'periodic')
plot(fit)


####

df <- total %>%
  dplyr::select(주, 발병률 ,O3) %>% 
  gather(key = "variable", value = "value", -주)

ggplot(df, aes(x = 주, y = value)) + 
  geom_line(aes(color = variable), size = 1) + 
  # scale_color_manual(values = c("#00AFBB", "#E7B800", )) +
  theme_minimal()

fit <- lm(발병률 ~ ., data = total[c(-1,-6,-8)])
summary(fit)
car::vif(fit)


library(forecast)
fit <- auto.arima(ts[,'발병률'],
                  xreg=ts[,'O3'])

cbind("Regression Errors" = residuals(fit, type="regression"),
      "ARIMA errors" = residuals(fit, type="innovation")) %>%
  autoplot(facets=TRUE)

checkresiduals(fit)

summary(fit)

str(total)

cor(analysis_total[c(-1,-2)])

nrow(analysis_total)

autoplot(ts[,c('O3','발병률')])

fit <- lm(발병률 ~ NO2, data = analysis_total[c(-1,-2)])
par(mfrow = c(2,2))
plot(fit)
summary(fit)


