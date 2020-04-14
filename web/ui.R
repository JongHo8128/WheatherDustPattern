library(shiny)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("호흡기 질환과 기상요인,미세먼지 패턴분석"),
  
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
      plotOutput(outputId = "mapplot")
    )
 
))
