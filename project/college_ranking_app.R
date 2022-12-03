library(tidyverse)
library(leaflet)
library(shiny)
theme_set(theme_minimal())

data <- read_csv("data/colleges.csv")

data <- read_csv("data/colleges.csv") %>% 
  mutate_at(vars(contains("ACTCM")), funs(as.integer)) %>%
  mutate(
    CITY = paste0(CITY, ", ", STABBR),
    avg_yearly_cost = round(COSTT4_A / 4, digits = -1),
    UGDS = round(UGDS, digits = -1),
    TUITIONFEE_IN = round(TUITIONFEE_IN, digits = -1),
    TUITIONFEE_OUT = round(TUITIONFEE_OUT, digits = -1),
    cur_tuition = round(ifelse(STABBR == "WI", TUITIONFEE_IN, TUITIONFEE_OUT), digits = -1),
    selected = 1,
  ) %>%
  drop_na()

data_2020 <- filter(data, YEAR == 2020)

update_data <- function(home_state, act_score, min_admit, selectivity, type, cost_range, tuition_range, size_range) {
  out <- data %>%
    filter(YEAR == 2020) %>%
    mutate(
      cur_tuition = round(ifelse(STABBR == home_state, TUITIONFEE_IN, TUITIONFEE_OUT), digits = -1),
      admit_difficulty = case_when(
        ADM_RATE < .1 ~ "Reach", # Any school with admit rate <10% is reach, regardless of ACT score
        act_score > ACTCM75 ~ "Safety",
        act_score >= ACTCM25 ~ "Target",
        act_score < ACTCM25 ~ "Reach"
      ),
      inst_type = case_when(
        TYPE == "PRIV" ~ "Private",
        TYPE == "PUB" ~ "Public"
      ),
      selected = case_when(
        !(admit_difficulty %in% selectivity) ~ 0,
        min_admit > ADM_RATE ~ 0,
        !(inst_type %in% type) ~ 0,
        avg_yearly_cost <= cost_range[1] ~ 0,
        avg_yearly_cost >= cost_range[2] ~ 0,
        cur_tuition <= tuition_range[1] ~ 0,
        cur_tuition >= tuition_range[2] ~ 0,
        UGDS <= size_range[1] ~ 0,
        UGDS >= size_range[2] ~ 0,
        TRUE ~ 1
      )
    )
  return(out)
}

histplot <- function(df, var, selected) {
  ggplot(df) +
    geom_histogram(aes(x = {{var}}, fill = factor(selected, levels = c("FALSE", "TRUE")))) +
    scale_fill_manual(values=c("orange", "purple")) +
    theme(
      legend.position = "none",
      axis.title = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
}

ui <- navbarPage("College Ranking",
                 tabPanel("Find Universities",
                          fluidPage(
                            titlePanel("Find Universities"),
                            fluidRow(
                              column(2, selectInput("home_state", "Home State", state.abb, selected = "WI")),
                              column(2, numericInput("act_score", "ACT Score", value = 36, min = 0, max = 36)),
                              column(4, sliderInput("min_admit", "Minimum Admit Rate", 0, 1, 0)),
                              column(2, checkboxGroupInput("selectivity", "Selectivity", c("Reach", "Target", "Safety"), selected = c("Reach", "Target", "Safety"))),
                              column(2, checkboxGroupInput("type", "Institution Type", c("Public", "Private"), selected = c("Public", "Private")))
                            ),
                            fluidRow(
                              column(4,
                                     plotOutput("cost", height = 100),
                                     sliderInput("cost_range", NULL, min(data_2020$avg_yearly_cost), max(data_2020$avg_yearly_cost), c(min(data_2020$avg_yearly_cost), max(data_2020$avg_yearly_cost)), round = 100),
                                     plotOutput("tuition", height = 100),
                                     sliderInput("tuition_range", NULL, min(data_2020$TUITIONFEE_IN), max(data_2020$TUITIONFEE_OUT), c(min(data_2020$TUITIONFEE_IN), max(data_2020$TUITIONFEE_OUT))),
                                     plotOutput("size", height = 100),
                                     sliderInput("size_range", NULL, min(data_2020$UGDS), max(data_2020$UGDS), c(min(data_2020$UGDS), max(data_2020$UGDS)))
                              ),
                              column(8, leafletOutput("map")),
                            ),
                            fluidRow(dataTableOutput("table"))
                          )
                 ),
                 tabPanel("Compare Universities",
                          fluidPage(
                            titlePanel("Compare Universities")
                          )
                 )
)

server <- function(input, output) {
  data <- reactive({
    update_data(input$home_state, input$act_score, input$min_admit, input$selectivity, input$type, input$cost_range, input$tuition_range, input$size_range)
  })
  
  selected <- reactive({
    data()$selected
  })
  
  output$cost <- renderPlot({
    histplot(data(), avg_yearly_cost, selected()) +
      labs(title = "Yearly Average Cost of Attendance")
  })
  
  output$tuition <- renderPlot({
    histplot(data(), cur_tuition, selected()) +
      labs(title = "Tuition Range (per year)")
  })
  
  output$size <- renderPlot({
    histplot(data(), UGDS, selected()) +
      labs(title = "Student Body Size")
  })
  
  output$map <- renderLeaflet({
    data() %>%
      leaflet(options = leafletOptions(minZoom = 3)) %>% 
      addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(noWrap = TRUE)) %>% 
      addCircles(lng = ~LONGITUDE, lat = ~LATITUDE, label = ~NAME, color = ~colorFactor(palette=c("orange", "purple"), domain=selected)(selected)) %>% 
      setView(lng = -93.85, lat = 37.45, zoom = 3) %>% 
      setMaxBounds(lng1 = -40, lat1 = 10, lng2 = -160, lat2 = 60)
  })
  
  output$table <- renderDataTable({
    data() %>%
      filter(selected == TRUE) %>% 
      select(NAME, CITY, RANK, admit_difficulty, cur_tuition, avg_yearly_cost) # RANK, CITY, STABBR, ACTCM25)
  })
}

shinyApp(ui, server)
