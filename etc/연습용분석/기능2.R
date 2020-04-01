load("refinedata/analysis/analysis_total.rda")
library(forecast)
library(prophet)
str(analysis_sido)


dayfreq <- 365
daystart <- c(2016,1)
dayend <- c(2018,12)
question <- 10

analysis_day <- analysis_total %>% group_by(substr(일시,1,10)) %>% summarise(`평균기온(°C)` = mean(`평균기온(°C)`,na.rm=TRUE),
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
View(analysis_day)


# -------------------------------
# (4) 시계열 예측수행
# -------------------------------



library(plotly)
library(ggplot2)

p <- plot(forecast(fit, h = 100))
f <- forecast(fit, h = 100)
f
f$mean

dev.off()
help('plot')







originaldata <-analysis_day$PM10
newdata <- c()

analysis_day <- analysis_day[-1]
 


for(i in 1:question){
  ts_day <- ts(originaldata,start=daystart,frequency = dayfreq) 
  fit <- auto.arima(ts_day)
  f <- forecast(fit, h = 1)   # 3-step ahead forecasting
  originaldata <- append(originaldata , f$mean[1])
  newdata <- append(newdata, f$mean[1])
}

ts_new <- ts(newdata,start=c(2019,0),frequency = dayfreq) 
plot(ts_new )

str(analysis_day)

analysis_day <- rename(analysis_day,일시=`substr(일시, 1, 10)`)
analysis_day
analysis_day<-analysis_day[c("일시","PM10")]
str(analysis_day)
args(as.Date())
analysis_day$일시 <- as.Date(analysis_day$일시,tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))

analysis_day <- rename(analysis_day,ds=일시,y=PM10)

#m <- prophet(ds = analysis_day$일시,y = analysis_day$PM10)
m <- prophet(analysis_day)

future <- make_future_dataframe(m, periods = 365)

tail(future)

forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
View(forecast)
plot(m, forecast[1300:1400,])
