library(tidyverse)
library(leaflet)
library(shiny)
library(fmsb)
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
    selected = 1
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
    geom_histogram(aes(x = {{var}}, fill = factor(selected, levels = c(0, 1)))) +
    scale_fill_manual(values=c("orange", "purple")) +
    theme(
      legend.position = "none",
      axis.title = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
}

### PAGE 2 FUNCTIONS ###

# Star plot function
starplot <- function(data,
                     vlabels = colnames(data), vlcex = 0.7,
                     caxislabels = NULL, title = NULL, ...){
  
  color <- c(rgb(1, 0, 0, 0.25),
             rgb(0, 1, 0, 0.25),
             rgb(0, 0, 1, 0.25))
  
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}

# Setting up data for star plot
star_data_convert <- function(star_school, star_stats){
  # Convert selected stats into percentiles using all 2020 schools
  star_data <- data %>%
    filter(YEAR == 2020) %>% 
    select(NAME, star_stats) %>% 
    mutate(across(star_stats, function(x) ecdf(x)(x)))
  
  # Add min/max levels of 0 and 1 (needed for the starplot bounds)
  star_data <- rbind(c("MAX",rep(1,ncol(star_data)-1)),
                     c("MIN",rep(0,ncol(star_data)-1)),
                     star_data) %>% 
    mutate(across(star_stats, as.numeric))
  
  # Filter only selected school and create plot
  out <- star_data %>% 
    filter(NAME %in% c("MAX","MIN", star_school)) %>% 
    select(-NAME) %>% 
    as.data.frame()
  
  return(out)
}

# Line plot function
lineplot = function(df, line_variable){
  ggplot(mapping = aes(color = NAME)) +
    geom_line(data=df, aes(YEAR, .data[[line_variable]])) +
    scale_x_continuous(breaks = c(2011,2012,2013,2014,2015,2016,2017,2018,2019,2020))+
    labs(x = "Year", y = line_variable,
         title = paste0(line_variable, " evolution (2011-2020)"), color = "")+
    theme(text = element_text(size = 18))
}

# Setting up data for line plot (use same schools as in star plot)
line_data_convert <- function(star_school, line_variable){
  # Get selected variable
  line_data <- data %>%
    select(NAME, YEAR, line_variable)
  
  # Filter only selected school
  out <- line_data %>% 
    filter(NAME %in% c(star_school)) %>% 
    as.data.frame()
  
  return(out)
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
                            titlePanel("Micro view"),
                            fluidRow(
                              column(2, selectInput("star_school1", "Select First School", unique(data$NAME), selected = NA)),
                              column(2, selectInput("star_school2", "Select Second School", c(NA,unique(data$NAME)), selected = NA)),
                              column(2, selectInput("star_school3", "Select Third School", c(NA,unique(data$NAME)), selected = NA))
                            ),
                            fluidRow(
                              column(4, checkboxGroupInput("star_stats", "Select Stats (at least 3)", 
                                                           c("RANK", "ADM_RATE", "UGDS", "COSTT4_A", "TUITIONFEE_IN","TUITIONFEE_OUT",
                                                             "C150_4", "ACTCM25", "ACTCM75", "ACTCMMID", "NPT", "avg_yearly_cost","SAFETY_INDEX"),
                                                           selected = c("RANK","ADM_RATE","UGDS","COSTT4_A"))),
                              column(5, plotOutput("starplot"))
                            ),
                            fluidRow(
                              column(2, selectInput("line_variable", "Select variable", 
                                                    c("RANK", "ADM_RATE", "UGDS", "COSTT4_A", "TUITIONFEE_IN", "TUITIONFEE_OUT",
                                                      "C150_4", "ACTCM25", "ACTCM75", "ACTCMMID", "NPT", "avg_yearly_cost","SAFETY_INDEX"),
                                                    selected = c("ADM_RATE"))),
                              
                              column(9, plotOutput("lineplot")),
                            ),
                            fluidRow(
                              dataTableOutput("startable")
                            ),
                            fluidRow(
                              dataTableOutput("linetable")
                            )
                          )
                 )
)

server <- function(input, output, session) {
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
  
  ### PAGE 2 ###
  
  # Make the star plot school selection input options change based on page 1 filters
  observe({
    p1schools <- data()$NAME
    
    if (is.null(p1schools)){
      p1schools <- character(0)
    }
    
    updateSelectInput(session, "star_school1",
                      choices = p1schools,
                      selected = head(p1schools, 1)
    )
    
    updateSelectInput(session, "star_school2",
                      choices = c(NA,p1schools),
                      selected = NA
    )
    
    updateSelectInput(session, "star_school3",
                      choices = c(NA,p1schools),
                      selected = NA
    )
    
  })
  
  # Convert data for star plot
  star_data <- reactive({
    star_data_convert(c(input$star_school1,input$star_school2,input$star_school3), input$star_stats)
  })
  
  output$starplot <- renderPlot({
    starplot(star_data())
  })
  
  output$startable <- renderDataTable({
    star_data()
  })
  
}

shinyApp(ui, server)
