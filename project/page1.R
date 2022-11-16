library(tidyverse)
library(shiny)

data <- read_csv("data/colleges.csv")
raw <- read_csv("college-ranking/project/data/colleges.csv")

data <- raw %>%
  mutate_at(vars(contains("ACTCM")), funs(as.integer))

states <- data$STABBR %>%
  unique() %>%
  sort() %>%
  as.list()

filter_data <- function(act_score, selectivity, min_admit, home_state, budget) {
  out <- data %>%
    mutate(
      cur_tuition = ifelse(STABBR == home_state, TUITIONFEE_IN, TUITIONFEE_OUT),
      admit_difficulty = case_when(
        ADM_RATE < .1 ~ "Reach", # Any school with admit rate <10% is reach, regardless of ACT score
        act_score > ACTCM75 ~ "Safety",
        act_score >= ACTCM25 ~ "Target",
        act_score < ACTCM25 ~ "Reach"
      )
      ) %>%
    filter(
      YEAR == 2020,
      admit_difficulty %in% selectivity,
      min_admit <= ADM_RATE,
      cur_tuition >= budget[1],
      cur_tuition <= budget[2]
      )
  return(out)
}

ui <- fluidPage(
  titlePanel("get ready to make the most important decision of your life (no pressure)"),
  column(4,
         numericInput("act_score", "ACT Score", value = 36),
         checkboxGroupInput("selectivity", "Selectivity", c("Reach", "Target", "Safety"), selected = c("Reach", "Target", "Safety")),
         sliderInput("min_admit", "Minimum Admit Rate", 0, 1, 0),
         selectInput("home_state", "Home State", states, selected = "WI"),
         sliderInput("budget", "Tuition Range (per year)", 0, 70000, c(0, 70000), pre = "$")
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
    filter_data(input$act_score, input$selectivity, input$min_admit, input$home_state, input$budget)
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
      select(NAME, ACTCM25, ACTCM75, ADM_RATE, admit_difficulty)#RANK, CITY, STABBR, ACTCM25)
  })
}

shinyApp(ui, server)
