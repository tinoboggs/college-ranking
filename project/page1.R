library(tidyverse)
library(shiny)

data <- read_csv("data/colleges.csv")

data %>%
  filter(YEAR == 2020) %>%
  ggplot() +
  geom_point(aes(LONGITUDE, LATITUDE))

ui <- fluidPage(
  titlePanel("get ready to make the most important decision of your life (no pressure)"),
  column(4,
         textInput("act_score", "ACT Score")
  ),
  column(4,
         fluidRow(
           plotOutput("map"),
           plotOutput("hist")
         )
  ),
  column(4, dataTableOutput("table")),
)

server <- function(input, output) {
  output$map <- renderPlot({
    data %>%
      filter(
        YEAR == 2020,
        ACTCM25 < input$act_score
      ) %>%
      ggplot() +
      geom_point(aes(LONGITUDE, LATITUDE))
  })
  output$hist <- renderPlot({
    data %>%
      filter(
        YEAR == 2020,
        ACTCM25 < input$act_score
      ) %>%
      ggplot() +
      geom_histogram(aes(COSTT4_A))
  })
  output$table <- renderDataTable({
    data %>%
      filter(
        YEAR == 2020,
        ACTCM25 < input$act_score
      ) %>%
      select(NAME, RANK, CITY, STABBR)
  })
}

shinyApp(ui, server)
