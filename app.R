library(shiny)
library(data.table)
library(tidyverse)
library(readr)

#setwd("C:/Users/ConditionKim/Desktop/성균관대학교/2021-1/대용량자료관리및시각화/프로젝트/shiny")


train <- fread("data/train.csv",
               header = T,
               data.table = T,
               encoding = "UTF-8")
load("data/map.RData")

vars <- train %>% select_if(is.numeric) %>% colnames() %>% setdiff("unit_price")
gu <- train$자치구명 %>% unique() %>% sort()
gu <- c("전체", gu)

ui <- fluidPage(
  titlePanel("서울시 부동산 요인 분석"),
  
  sidebarPanel(
    selectInput('xcol', '설명변수', vars),
    selectInput('gu', '구', gu, selected = gu[[1]]),
    checkboxInput("log_t",
                  label = "로그변환", value = FALSE),
    HTML('<center><img src = "map.gif", height = 500, width = 500></center>'),

    br(), br(), br(),  br(), br(),
    br(), br(), br(),  br(), br(),
    br(), br(), br(),  br(), br(),
    
    br(), br(), br(),  br(), br(),
    
    checkboxGroupInput("checkgroup", 
                       h3("구 선택"), 
                       choices = list("강남구" = 2,
                                      "강동구" = 3,
                                      "강북구" = 4,
                                      "강서구" = 5,
                                      "관악구" = 6,
                                      "광진구" = 7,
                                      "구로구" = 8,
                                      "금천구" = 9,
                                      "노원구" = 10,
                                      "도봉구" = 11,
                                      "동대문구" = 12,
                                      "동작구" = 13,
                                      "마포구" = 14,
                                      "서대문구" = 15,
                                      "서초구" = 16,
                                      "성동구" = 17,
                                      "성북구" = 18,
                                      "송파구" = 19,
                                      "양천구" = 20,
                                      "영등포구" = 21,
                                      "용산구" = 22,
                                      "은평구" = 23,
                                      "종로구" = 24,
                                      "중구" = 25,
                                      "중랑구" = 26),
                       selected = 2:26),
  ),

  mainPanel(
    plotOutput('plot1', height = 600, width = 700),
    br(), br(), br(), br(),
    br(), br(), br(), br(),
    plotOutput("plot2", height = 600, width = 700)
  )
)

server <- function(input, output){
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    if(input$gu == "전체"){
      if(input$log_t){
        train %>% mutate(unit_price = log(unit_price)) 
      }else{
        train 
      }
    }else{
      if(input$log_t){
        train %>% 
          filter(자치구명 == input$gu) %>% 
          mutate(unit_price = log(unit_price)) 
        }
      else{
        train %>% 
          filter(자치구명 == input$gu)
        
      }
    }
  })
  selectseouldt <- reactive(
    seoul_dt %>% 
      mutate(시가총액 = ifelse(! NM %in% gu[as.numeric(input$checkgroup)], 0, 시가총액)) 
  )
  

  
  output$plot1 <- renderPlot({
    selectedData() %>% 
      ggplot(aes(x = .data[[input$xcol]],
                 y = unit_price)) +
      geom_smooth(color = "darkblue",
                  method = "lm", 
                  formula ="y~x",  
                  se = T) + 
      geom_point(alpha = 0.4, size = 3) +
      labs(title = input$xcol, y = "거래단가") +
      theme(panel.background = element_rect(fill = "white", color = "black"),
            text = element_text(face = "bold"))
  })
  
  output$plot2 <- renderPlot({
    selectseouldt() %>% draw_map()
    })
  

}

# Run app
shinyApp(ui, server)



