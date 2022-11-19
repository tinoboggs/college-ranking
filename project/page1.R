library(tidyverse)
library(leaflet)
library(shiny)
theme_set(theme_minimal())

data <- read_csv("data/colleges.csv") %>% 
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

histplot <- function(data, var) {
  ggplot(data) +
    geom_histogram(aes({{var}})) +
    theme(
      axis.title = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
}

ui <- navbarPage("College Ranking",
  tabPanel("Page 1",
   fluidPage(
     titlePanel("get ready to make the most important decision of your life (no pressure)"),
     fluidRow(
       column(2, selectInput("home_state", "Home State", state.abb, selected = "WI")),
       column(2, numericInput("act_score", "ACT Score", value = 36, min = 0, max = 36)),
       column(4, sliderInput("min_admit", "Minimum Admit Rate", 0, 1, 0)),
       column(1, checkboxGroupInput("selectivity", "Selectivity", c("Reach", "Target", "Safety"), selected = c("Reach", "Target", "Safety"))),
       column(1, checkboxGroupInput("type", "Institution Type", c("Public", "Private"), selected = c("Public", "Private")))
       ),
     fluidRow(
       column(3,
        plotOutput("cost", brush = brushOpts("cost_brush", direction = "x"), height = 200),
        sliderInput("budget", "Tuition Range (per year)", 0, 70000, c(0, 70000), pre = "$"),
        sliderInput("size", "Student Body Size", 0, 80000, c(0, 80000))
       ),
       column(5, leafletOutput("map")),
       column(4, plotOutput("hist"))
     ),
     fluidRow(dataTableOutput("table")),
   )
  ),
  tabPanel("Page 2")
)

server <- function(input, output) {
  app_data <- reactive({
    filter_data(input$act_score, input$selectivity, input$min_admit, input$home_state, input$budget, input$size, input$type)
  })
  
  output$cost <- renderPlot({
    histplot(data, avg_yearly_cost) +
      labs(title = "Yearly Average Cost of Attendance")
  })

  output$map <- renderLeaflet({
    app_data() %>%
      leaflet(options = leafletOptions(minZoom = 3)) %>% 
      addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(noWrap = TRUE)) %>% 
      addCircles(lng = ~LONGITUDE, lat = ~LATITUDE, label = ~NAME) %>% 
      setView(lng = -93.85, lat = 37.45, zoom = 3) %>% 
      setMaxBounds(lng1 = -40, lat1 = 10, lng2 = -160, lat2 = 60)
  })
  
  output$hist <- renderPlot({
    histplot(app_data(), COSTT4_A) + theme(axis.title.x = element_text()) + labs(x="Average Cost of Attendance")
  })
  
  output$table <- renderDataTable({
    app_data() %>%
      select(NAME, ACTCM25, ACTCM75, ADM_RATE, admit_difficulty) # RANK, CITY, STABBR, ACTCM25)
  })
}

shinyApp(ui, server)
