library(tidyverse)
library(leaflet)
library(shiny)
theme_set(theme_minimal())

data <- read_csv("data/colleges.csv")
#raw <- read_csv("college-ranking/project/data/colleges.csv")

data <- data %>%
  mutate_at(vars(contains("ACTCM")), funs(as.integer)) %>%
  mutate(avg_yearly_cost = COSTT4_A / 4)

filter_data <- function(act_score, selectivity, min_admit, home_state, budget, size, type) {
  out <- data %>%
    mutate(
      cur_tuition = ifelse(STABBR == home_state, TUITIONFEE_IN, TUITIONFEE_OUT),
      admit_difficulty = case_when(
        ADM_RATE < .1 ~ "Reach", # Any school with admit rate <10% is reach, regardless of ACT score
        act_score > ACTCM75 ~ "Safety",
        act_score >= ACTCM25 ~ "Target",
        act_score < ACTCM25 ~ "Reach"
      ),
      inst_type = case_when(
        TYPE == "PRIV" ~ "Private",
        TYPE == "PUB" ~ "Public"
      )
    ) %>%
    filter(
      YEAR == 2020,
      admit_difficulty %in% selectivity,
      min_admit <= ADM_RATE,
      cur_tuition >= budget[1],
      cur_tuition <= budget[2],
      UGDS >= size[1],
      UGDS <= size[2],
      inst_type %in% type
    )
  return(out)
}

ui <- fluidPage(
  titlePanel("get ready to make the most important decision of your life (no pressure)"),
  column(4,
         numericInput("act_score", "ACT Score", value = 36),
         checkboxGroupInput("selectivity", "Selectivity", c("Reach", "Target", "Safety"), selected = c("Reach", "Target", "Safety")),
         sliderInput("min_admit", "Minimum Admit Rate", 0, 1, 0),
         selectInput("home_state", "Home State", state.abb, selected = "WI"),
         plotOutput("cost", brush = brushOpts("cost_brush", direction = "x"), height = 100),
         sliderInput("budget", "Tuition Range (per year)", 0, 70000, c(0, 70000), pre = "$"),
         sliderInput("size", "Student Body Size", 0, 80000, c(0, 80000)),
         checkboxGroupInput("type", "Institution Type", c("Public", "Private"), selected = c("Public", "Private"))
  ),
  column(4,
         fluidRow(
           leafletOutput("map"),
           plotOutput("hist")
         )
  ),
  column(4, dataTableOutput("table")),
)

server <- function(input, output) {
  app_data <- reactive({
    filter_data(input$act_score, input$selectivity, input$min_admit, input$home_state, input$budget, input$size, input$type)
  })
  output$cost <- renderPlot({
    data %>%
      ggplot() +
      geom_histogram(aes(avg_yearly_cost)) +
      labs(
        title = "Yearly Average Cost of Attendance"
      ) +
      theme(
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
  })
  output$map <- renderLeaflet({
    app_data() %>%
      leaflet() %>% 
      addTiles() %>% 
      addCircles(lng = ~LONGITUDE, lat = ~LATITUDE) %>% 
      setView(lng = -93.85, lat = 37.45, zoom = 3)
  })
  output$hist <- renderPlot({
    app_data() %>%
      ggplot() +
      geom_histogram(aes(COSTT4_A))
  })
  output$table <- renderDataTable({
    app_data() %>%
      select(NAME, ACTCM25, ACTCM75, ADM_RATE, admit_difficulty) # RANK, CITY, STABBR, ACTCM25)
  })
}

shinyApp(ui, server)
