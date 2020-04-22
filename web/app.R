library(shiny)
library(dplyr)
library(ggplot2)
library(data.table)
library(readr)
library(tidyverse)
library(prophet)
library(forecast)
library(plotly)
library(ggmap)


#Load data
analysis_total <- read_csv("dust_data/analysis_total_Fixed.csv",locale=locale('ko',encoding='euc-kr'))
analysis_total$일시 <- as.POSIXct(analysis_total$일시)

register_google(key = 'AIzaSyBo9Ro047DFTo4ua8PoU8e2A6VfydYPvhY')
map <- get_map(location='south korea', zoom=7, maptype='roadmap')

location <- read.csv("dust_data/location.csv",header=T, as.is=T)

location$sido <- ifelse(location$sido=="서울","seoul",location$sido)
location$sido <- ifelse(location$sido=="경기","kyungki",location$sido)
location$sido <- ifelse(location$sido=="인천","incheon",location$sido)
location$sido <- ifelse(location$sido=="대전","daejeon",location$sido)
location$sido <- ifelse(location$sido=="대구","daegu",location$sido)
location$sido <- ifelse(location$sido=="강원","gangwon",location$sido)
location$sido <- ifelse(location$sido=="전남","jeonnam",location$sido)
location$sido <- ifelse(location$sido=="전북","jeonbuk",location$sido)
location$sido <- ifelse(location$sido=="충남","chungnam",location$sido)
location$sido <- ifelse(location$sido=="충북","chungbuk",location$sido)
location$sido <- ifelse(location$sido=="경남","kyungnam",location$sido)
location$sido <- ifelse(location$sido=="경북","kyungbuk",location$sido)
location$sido <- ifelse(location$sido=="울산","ulsan",location$sido)
location$sido <- ifelse(location$sido=="부산","busan",location$sido)
location$sido <- ifelse(location$sido=="광주","gwangju",location$sido)
location$sido <- ifelse(location$sido=="세종","sejong",location$sido)
location$sido <- ifelse(location$sido=="제주","jeju",location$sido)

location$sido <-as.factor(location$sido)
# Define UI
ui <- fluidPage(
                
                titlePanel("호흡기 질환과 기상요인,미세먼지 패턴분석"),
                
                sidebarLayout(
                  
                  sidebarPanel(
                    
                    # Descriptor text
                    hr(),
                    HTML("1. 변수별 시도별 변수별 추이"),
                    hr(),
                    # Select variable type of trend to plot
                    selectInput(inputId = "type",
                                label = strong("변수 선택"),
                                choices = c("SO2" = "SO2",
                                            "CO" = "CO",
                                            "NO2" = "NO2",
                                            "O3" = "O3",
                                            "평균기온(°C)"="평균기온(°C)",
                                            "평균 풍속(m/s)"="평균 풍속(m/s)",
                                            "평균 현지기압(hPa)"="평균 현지기압(hPa)",
                                            "일 최심신적설(cm)"="일 최심신적설(cm)",
                                            "일강수량(mm)"="일강수량(mm)",
                                            "강수 계속시간(hr)"="강수 계속시간(hr)",
                                            "발생건수"="발생건수",
                                            "발병률"="발병률",
                                            "PM10"="PM10",
                                            "PM25"="PM25"),
                                selected = "SO2"),
                    
                    # Select sido sido of trend to plot
                    selectInput(inputId = "sido",
                                label = strong("시도 선택"),
                                choices = c("서울" = "서울",
                                            "경기" = "경기",
                                            "인천" = "인천",
                                            "대전" = "대전",
                                            "대구" = "대구",
                                            "강원" = "강원",
                                            "전남" = "전남",
                                            "전북" = "전북",
                                            "충남" = "충남",
                                            "충북" = "충북",
                                            "경남" = "경남",
                                            "경북" = "경북",
                                            "울산" = "울산",
                                            "부산" = "부산",
                                            "광주" = "광주",
                                            "세종" = "세종",
                                            "제주" = "제주"),
                                selected = "서울"),
                    
                    
                    
                    
                    # Select date range to be plotted
                    dateRangeInput("date", strong("날짜 선택"),
                                   start = "2016-01-01", end = "2018-12-31",
                                   min = "2016-01-01", max = "2018-12-31"),
                    
                    
                    
                    # Select whether to overlay smooth trend line
                    checkboxInput(inputId = "smoother",
                                  label = strong("Overlay smooth trend line"),
                                  value = FALSE),
                    
                    # Display only if the smoother is checked
                    conditionalPanel(condition = "input.smoother == true",
                                     sliderInput(inputId = "f",
                                                 label = "Smoother span:",
                                                 min = 0.01, max = 1, value = 0.67, step = 0.01,
                                                 animate = animationOptions(interval = 100)),
                                     HTML("Higher values give more smoothness.")
                    ),
                    hr(),
                    HTML("2. 시도별 변수들 예측"),
                    hr(),
                    selectInput(inputId = "type2",
                                label = strong("변수 선택"),
                                choices = c("SO2" = "SO2",
                                            "CO" = "CO",
                                            "NO2" = "NO2",
                                            "O3" = "O3",
                                            "평균기온(°C)"="평균기온(°C)",
                                            "평균 풍속(m/s)"="평균 풍속(m/s)",
                                            "평균 현지기압(hPa)"="평균 현지기압(hPa)",
                                            "일 최심신적설(cm)"="일 최심신적설(cm)",
                                            "일강수량(mm)"="일강수량(mm)",
                                            "강수 계속시간(hr)"="강수 계속시간(hr)",
                                            "발생건수"="발생건수",
                                            "발병률"="발병률",
                                            "PM10"="PM10",
                                            "PM25"="PM25"),
                                selected = "SO2"),
                    selectInput(inputId = "sido2",
                                label = strong("시도 선택"),
                                choices = c("서울" = "서울",
                                            "경기" = "경기",
                                            "인천" = "인천",
                                            "대전" = "대전",
                                            "대구" = "대구",
                                            "강원" = "강원",
                                            "전남" = "전남",
                                            "전북" = "전북",
                                            "충남" = "충남",
                                            "충북" = "충북",
                                            "경남" = "경남",
                                            "경북" = "경북",
                                            "울산" = "울산",
                                            "부산" = "부산",
                                            "광주" = "광주",
                                            "세종" = "세종",
                                            "제주" = "제주"),
                                selected = "서울"),
                    hr(),
                    HTML("3. 시도별 기간별 변수들 평균"),
                    hr(),
                    selectInput(inputId = "type3",
                                label = strong("변수 선택"),
                                choices = c("SO2" = "SO2",
                                            "CO" = "CO",
                                            "NO2" = "NO2",
                                            "O3" = "O3",
                                            "평균기온(°C)"="평균기온(°C)",
                                            "평균 풍속(m/s)"="평균 풍속(m/s)",
                                            "평균 현지기압(hPa)"="평균 현지기압(hPa)",
                                            "일 최심신적설(cm)"="일 최심신적설(cm)",
                                            "일강수량(mm)"="일강수량(mm)",
                                            "강수 계속시간(hr)"="강수 계속시간(hr)",
                                            "발생건수"="발생건수",
                                            "발병률"="발병률",
                                            "PM10"="PM10",
                                            "PM25"="PM25"),
                                selected = "SO2"),
                    
                    # Select date range to be plotted
                    dateRangeInput("date3", strong("날짜 선택"),
                                   start = "2016-01-01", end = "2018-12-31",
                                   min = "2016-01-01", max = "2018-12-31")
                    
                  ),
                  
                  
                  # Output: Description, lineplot, and reference
                  mainPanel(
                    plotOutput(outputId = "lineplot"),
                    plotOutput(outputId = "nextplot"),
                    plotOutput(outputId = "mapgraphicplot", width = "100%", height = "800px",),
                    plotOutput(outputId = "mapchartplot")
                  )
                )
)

# Define server function
server <- function(input, output) {
  
  
  
  # Subset data
  selected_trends <- reactive({
    
    req(input$date)
    select_type <- input$type
    
    
    
    data <- analysis_total %>% filter(시도 == input$sido)
    data <- data %>% 
      select(일시,select_type) %>% 
      filter(일시 > input$date[1] & 일시 < input$date[2]) 
    data["price"] <- data[select_type]
    
    validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
    data
    
  })
  
  selected_next <- reactive({
    
    req(input$date)
    select_type <- input$type2
    
    
    
    data2 <- analysis_total %>% filter(시도 == input$sido2)
    data2 <- data2 %>% select(일시,select_type)
    data2["price"] <- data2[select_type]
    
  
    data2
    
  })
  
  selected_map <- reactive({
    req(input$date3)
    data3 <-  analysis_total %>% filter(일시 > input$date3[1] & 일시 < input$date3[2]) 
    data3 <- data3 %>% group_by(시도코드,시도) %>% summarise(`평균기온(°C)` = mean(`평균기온(°C)`,na.rm=TRUE),
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
                                                       발병률 = sum(발병률))
    
    data3$code <- as.character(data3$시도코드)
    data3$시도 <- as.character(data3$시도)
    data3["price"] <-data3[input$type3]
    
    data3$시도 <- ifelse(data3$시도=="서울","seoul",data3$시도)
    data3$시도 <- ifelse(data3$시도=="경기","kyungki",data3$시도)
    data3$시도 <- ifelse(data3$시도=="인천","incheon",data3$시도)
    data3$시도 <- ifelse(data3$시도=="대전","daejeon",data3$시도)
    data3$시도 <- ifelse(data3$시도=="대구","daegu",data3$시도)
    data3$시도 <- ifelse(data3$시도=="강원","gangwon",data3$시도)
    data3$시도 <- ifelse(data3$시도=="전남","jeonnam",data3$시도)
    data3$시도 <- ifelse(data3$시도=="전북","jeonbuk",data3$시도)
    data3$시도 <- ifelse(data3$시도=="충남","chungnam",data3$시도)
    data3$시도 <- ifelse(data3$시도=="충북","chungbuk",data3$시도)
    data3$시도 <- ifelse(data3$시도=="경남","kyungnam",data3$시도)
    data3$시도 <- ifelse(data3$시도=="경북","kyungbuk",data3$시도)
    data3$시도 <- ifelse(data3$시도=="울산","ulsan",data3$시도)
    data3$시도 <- ifelse(data3$시도=="부산","busan",data3$시도)
    data3$시도 <- ifelse(data3$시도=="광주","gwangju",data3$시도)
    data3$시도 <- ifelse(data3$시도=="세종","sejong",data3$시도)
    data3$시도 <- ifelse(data3$시도=="제주","jeju",data3$시도)
  
    data3$시도 <- as.factor(data3$시도)
    data3$sido<-data3$시도
    
    data3
    
    
    
  })
  
  output$mapgraphicplot <- renderPlot({

 

  analysis_total_map <- inner_join(selected_map(),location,by=c("sido"))


  
   ggmap(map) +   geom_jitter( data=analysis_total_map, aes(x=long, y=lat, size =price,color=sido)) + scale_size(name="variable") +   geom_text(data=analysis_total_map, aes(x=long-0.1, y=lat-0.1, label=sido),size=6,col="red")
   #+   geom_text(data=analysis_total_map, aes(x=long-0.1, y=lat-0.2, label=price),size=6,col="blue")




  })
  
  output$mapchartplot <- renderPlot({


    
    ggplot(selected_map(), aes(x=sido, y=price, fill=sido))+ ggtitle("Average of variables by trial period")+theme_bw(base_family = 'Gothic')+ geom_bar(stat="identity")


  })
  
  
  output$lineplot <- renderPlot({

    select_type <- input$type
    color = "#434343"

    plot(x = selected_trends()$일시, y = selected_trends()$price, type = "l",
         xlab = "Date", ylab = "Trend index",
         col = color, fg = color,
         col.lab = color, col.axis = color,main="variation trend")
    # Display only if smoother is checked
    if(input$smoother){
      smooth_curve <- lowess(x = as.numeric(selected_trends()$일시), y = selected_trends()$price, f = input$f)
      lines(smooth_curve, col = "#E6553A", lwd = 3)
    }
  })
  
  output$nextplot <- renderPlot({
    # stlf()
    start<-selected_next()$일시

    data <- ts(selected_next()$price,frequency = 365,start=c(2016,1))
    fcast <- stlf(data, method='naive')
    plot(fcast,main="variation expectation")


    

  })
  
  
  
  
}

# Create Shiny object
shinyApp(ui = ui, server = server)