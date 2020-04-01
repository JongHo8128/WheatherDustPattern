load("refinedata/analysis/analysis_total.rda")
library(dplyr)
args(hclust)
analysis_sido$시도 <- as.factor(analysis_sido$시도)
analysis_sido <- analysis_total %>% group_by(시도) %>% summarise(`평균기온(°C)` = mean(`평균기온(°C)`,na.rm=TRUE),
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
                                                           발병률 = sum(발생건수)/sum(인구수)*100)
analysis_sido
analysis_sido_num
analysis_sido_num <- analysis_sido %>% select(-시도)
View(analysis_sido_num)
analysis_sido_num_scale <- scale(analysis_sido_num )
class(analysis_sido_num_scale)
View(analysis_sido_num_scale)

library(pls)
analysis_sido_num_scale2 <- stdize(as.matrix(analysis_sido_num) )
View(analysis_sido_num_scale2)

rownames(analysis_sido_num_scale) <- analysis_sido$시도
View(analysis_sido_num_scale)
analysis_sido_num_scale
d<- dist(analysis_sido_num_scale)
hc<-hclust(d,method='ward')




save(analysis_sido_num,file="lawdata.rda")



plot(hc,hang=-1)

dim(analysis_total_2016_01)

table(is.na(analysis_total_2016_01))

hclust(analysis_total_2016_01)
analysis_sido_num_scale

kmc <- kmeans(analysis_sido_num_scale,16)
kmc
table(kmc$cluster)

plot(as.data.frame(analysis_sido_num_scale),col=kmc$cluster)

analysis_sido_num_scale

hc <- hclust(dist(analysis_sido_num_scale),method='ward')
hc
plot(hc,hang=-1)

hc4<-cutree(hc,k=4)
class(hc4)

library(dplyr)
hc4
ta(analysis_sido_num_scale)
analysis_sido_num_scale <- as.data.frame(analysis_sido_num_scale)
analysis_sido_num_scale <- analysis_sido_num_scale %>% mutate(cluster=hc4)

analysis_sido_num_scale

kmc <- kmeans(analysis_sido_num_scale,centers = 4)
analysis_sido_num_scale$kcluster <- kmc$cluster
analysis_sido_num_scale

plot(analysis_sido_num_scale , col = analysis_sido_num_scale$kcluster , pch=16)

library(cluster)

# (4) 차원축소 후, 군집결과 시각화
clusplot(
  as.data.frame(analysis_sido_num_scale), 
  kmc$cluster, 
  color = T, 
  shade = T,
  labels = 2,
  lines = 0
)
class(analysis_sido_num_scale)
View(analysis_sido_num_scale)
# (5) 특정 군집의 관측치들만 추출
df_cl2 <- subset(analysis_sido_num_scale, kmc$cluster == 2)
View(df_cl2)
library(NbClust)
NbClust(
        data =as.data.frame(analysis_sido_num_scale),
        distance = 'euclidean',
        min.nc=2,
        max.nc=15,
        method = 'average',
        index=c( "kl", "ch", "hartigan", "ccc", "scott", "marriot", "trcovw")
)
help(NbClust)
data(nutrient,package="flexclust")


View(analysis_sido_num_scale)
View(cor(analysis_sido_num_scale))
save(analysis_sido_scale,file="test.rda")
