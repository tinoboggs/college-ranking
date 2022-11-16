library(tidyverse)
library(shiny)

data <- read_csv("data/colleges.csv")
raw <- read_csv("college-ranking/project/data/colleges.csv")
data <- raw %>%
  mutate(ACTCM25 = as.integer(ACTCM25))

filter_data <- function(act_score) {
  out <- data %>%
    filter(
      YEAR == 2020,
      act_score > ACTCM25
      )
  return(out)
}

ui <- fluidPage(
  titlePanel("get ready to make the most important decision of your life (no pressure)"),
  column(4,
         numericInput("act_score", "ACT Score")
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
  app_data <- reactive({
    filter_data(input$act_score)
  })
  output$map <- renderPlot({
    app_data() %>%
      ggplot() +
      geom_point(aes(LONGITUDE, LATITUDE))
  })
  output$hist <- renderPlot({
    app_data() %>%
      ggplot() +
      geom_histogram(aes(COSTT4_A))
  })
  output$table <- renderDataTable({
    app_data() %>%
      select(NAME, RANK, CITY, STABBR, ACTCM25)
  })
}

shinyApp(ui, server)
