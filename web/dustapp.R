library(shiny)
library(shinythemes) 
library(dplyr)
library(ggiraphExtra) 
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

temp_map_join <- as.data.frame(temp_map_join)
temp_map_join <- temp_map_join[c("code","long","lat")]
temp_map_join <- rename(temp_map_join,시도코드=code)
temp_map_join$시도코드 <- as.numeric(temp_map_join$시도코드)

analysis_total_Fixed <- read_csv("refinedata/analysis/analysis_total_Fixed.csv",locale=locale('ko',encoding='euc-kr'))
analysis_total_Fixed <- analysis_total_Fixed [-1]
analysis_total_Fixed <- analysis_total_Fixed %>% select(-발생건수,-`최저기온(°C)`,-`최고기온(°C)`,-`최다풍향(16방위)`,-PM10등급,-PM25등급,-년도,-인구수)



analysis_map <- inner_join(analysis_total_Fixed,temp_map_join,by=c("시도코드")) 
write.csv(analysis_map,file = "analysis_map.csv")



#trend_data <- read_csv("web/data/trend_data.csv")
#str(trend_data)

# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("호흡기 질환과 기상요인,미세먼지 패턴분석"),
                sidebarLayout(
                  sidebarPanel(

                    # Select variable type of trend to plot
                    selectInput(inputId = "type",
                                label = strong("변수별 일별 추이"),
                                choices = c("SO2" = "SO2",
                                            "CO" = "CO",
                                            "NO2" = "NO2",
                                            "O3" = "O3"),
                                selected = "SO2"),
                    
                    # Select sido sido of trend to plot
                    selectInput(inputId = "sido",
                                label = strong("시도 추이"),
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

                    # Descriptor text
                    HTML("2016년 1월 1일 부터 2018년 12월 31일 까지의 데이터입니다"),

                    # Line for visual separation
                    hr(),

                    # Select date range to be plotted
                    dateRangeInput("date", strong("Date range"),
                                   start = "2016-01-01", end = "2018-12-31",
                                   min = "2016-01-01", max = "2018-12-31"),

                    # Line for visual separation
                    hr(),

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
                    )
                  ),

                  # Output: Description, lineplot, and reference
                  mainPanel(
                    plotOutput(outputId = "lineplot"),
                    plotOutput(outputId = "mapplot"))
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
  
  
  
 
  
}

# Create Shiny object
shinyApp(ui = ui, server = server)
