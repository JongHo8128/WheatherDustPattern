# 분산분석을 통한 풍향별 미세먼지 차이 비교

analysis_total_Fixed <- as.data.frame(analysis_total_Fixed)

View(analysis_total_Fixed)
str(analysis_total_Fixed)

analysis_total_Fixed$`최다풍향(16방위)` <- as.factor(analysis_total_Fixed$`최다풍향(16방위)`)

analysis_total <- analysis_total_Fixed %>% 
  dplyr::select(`최다풍향(16방위)`, PM10, PM25)

analysis_total <- rename(analysis_total,wind = `최다풍향(16방위)`)

str(analysis_total)

fit.aov <- aov(formula = PM10 ~ wind,data = analysis_total)

summary(fit.aov)

library(multcomp)
fit.aov
tuk <- glht(fit.aov, linfct=mcp(wind = 'Tukey'))
summary(tuk)
plot( cld(tuk, level = .05), col='lightgray' )

# 서풍일때 미세먼지 농도 가장 높음