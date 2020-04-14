library(shiny)
library(shinythemes)
library(tidyverse)#x
library(dplyr)#x
library(ggiraphExtra)
library(ggplot2)
library(data.table)
library(maps)
library(kormaps2014)
library(readr)


#Load data
analysis_total_Fixed <- read_csv("dust_data/analysis_total_Fixed.csv",locale=locale('ko',encoding='euc-kr'))
analysis_total <- analysis_total_Fixed
analysis_total$일시 <- as.POSIXct(analysis_total$일시)

tempmap <- kormap1 

code <- c('42','41','43','44','30','47','48','45','46','11','28','27','31','29','26','49','36') 
name1 <- c('강원도','경기도','충청북도','충청남도','대전광역시','경상북도','경상남도','전라북도','전라남도','서울특별시','인천광역시','대구광역시','울산광역시','광주광역시','부산광역시','제주특별자치도','세종특별자치시')
df_sido <- data.frame("code"=code,"name1"=name1)

temp_map_join <- inner_join(tempmap,df_sido,by=c('name1'))

temp_map_join <- temp_map_join %>% select(-code.x)
temp_map_join <- rename(temp_map_join,code=code.y)
temp_map_join$code <- as.character(temp_map_join$code)


temp_map_join$region <- temp_map_join$code
temp_map_join$SIDO_CD <- temp_map_join$code



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  
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
  
  selected_map <- reactive({
    req(input$date)
    data2 <-  analysis_total %>% filter(일시 > input$date[1] & 일시 < input$date[2]) 
    data2 <- data2 %>% group_by(시도코드,시도) %>% summarise(`평균기온(°C)` = mean(`평균기온(°C)`,na.rm=TRUE),
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
    
    data2$code <- as.character(data2$시도코드)
    data2["price"] <-data2[input$type]
    data2
    
    
    
  })
  
  # Create scatterplot object the plotOutput function is expecting
  output$lineplot <- renderPlot({
    
    select_type <- input$type
    color = "#434343"
    par(mar = c(4, 4, 1, 1))
    plot(x = selected_trends()$일시, y = selected_trends()$price, type = "l",
         xlab = "Date", ylab = "Trend index",
         col = color, fg = color,
         col.lab = color, col.axis = color)
    # Display only if smoother is checked
    if(input$smoother){
      smooth_curve <- lowess(x = as.numeric(selected_trends()$일시), y = selected_trends()$price, f = input$f)
      lines(smooth_curve, col = "#E6553A", lwd = 3)
    }
  })
  
  output$mapplot <- renderPlot(width = 750, height = 750,{
    
    par(mar = c(2, 2, 1, 1))
    
    ggChoropleth(
      data = selected_map(),
      
      digit = 3,
      
      aes(fill=price, 
          
          map_id=code,
          
          tooltip=시도
          
          
      ),
      #palette = '',
      map=temp_map_join, 
      
      interactive=F)
    
    
  })
  
  
  
})