library(tidyverse)
library(leaflet)
library(shiny)
library(fmsb)
theme_set(theme_minimal())

data <- read_csv("data/colleges_crime.csv") %>% 
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

### PAGE 2 FUNCTIONS ###

# Star plot function
starplot <- function(data){
  
  color <- c("#F8766D", "#00BFC4", "#7CAE00")
  
  par(mar=c(3,8,2,8))
  
  radarchart(
    data %>% select(-NAME), axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.15), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = 0.9, vlabels = colnames(data %>% select(-NAME)),
    calcex=0.7, caxislabels = c("0%","25%","50%","75%","100%"),
    title = "Head to Head Comparisons\n(percentiles)"
  )

  legend(
    x = "bottom", xpd=TRUE, inset=c(0, -.05),
    legend = data[-c(1:2),1], horiz = TRUE,
    bty = "n", pch = 20 , col = c("#F8766D", "#00BFC4", "#7CAE00"),
    text.col = "black", cex = 1, pt.cex = 2
  )

}

# Setting up data for star plot
star_data_convert <- function(star_school, star_stats){
  stats_all <- c("RANK", "ADM_RATE", "UGDS", "COSTT4_A", "TUITIONFEE_IN", "TUITIONFEE_OUT", "C150_4", "ACTCM25", "ACTCM75", "ACTCMMID", "NPT", "avg_yearly_cost")
  stats_inv <- c("RANK", "COSTT4_A", "TUITIONFEE_IN", "TUITIONFEE_OUT", "NPT", "avg_yearly_cost")
  
  # Convert selected stats into percentiles using all 2020 schools
  star_data <- data %>%
    filter(YEAR == 2020) %>% 
    mutate(across(stats_all, function(x) ecdf(x)(x))) %>% 
    mutate(across(stats_inv, function(x) 1-x)) %>% 
  select(NAME, star_stats)
  
  # Add min/max levels of 0 and 1 (needed for the starplot bounds)
  star_data <- rbind(c("MAX",rep(1,ncol(star_data)-1)),
                     c("MIN",rep(0,ncol(star_data)-1)),
                     star_data) %>% 
    mutate(across(star_stats, as.numeric))
  
  # Filter only selected school and create plot
  out <- star_data %>% 
    filter(NAME %in% c("MAX","MIN", star_school)) %>% 
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



ui <- navbarPage("US College Rankings",
                 tabPanel("Macro view",
                          fluidPage(
                            titlePanel("Get ready to make the most important decision of your life (no pressure)"),
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
                 
                 ### PAGE 2 ###
                 tabPanel("Micro view",
                          fluidPage(
                            titlePanel("Micro view"),
                            fluidRow(
                              column(2, selectInput("star_school1", "Select First School", unique(data$NAME), selected = NA)),
                              column(2, selectInput("star_school2", "Select Second School", c(NA,unique(data$NAME)), selected = NA)),
                              column(2, selectInput("star_school3", "Select Third School", c(NA,unique(data$NAME)), selected = NA))
                            ),
                            fluidRow(
                              column(3, checkboxGroupInput("star_stats", "Select Starplot Stats (at least 3)", 
                                                           c("RANK", "ADM_RATE", "UGDS", "COSTT4_A", "TUITIONFEE_IN","TUITIONFEE_OUT",
                                                             "C150_4", "ACTCM25", "ACTCM75", "ACTCMMID", "NPT", "avg_yearly_cost","SAFETY_INDEX"),
                                                           selected = c("RANK","ADM_RATE","UGDS","COSTT4_A"))),
                              column(7, plotOutput("starplot"))
                            ),
                            fluidRow(
                              dataTableOutput("startable")
                            ),
                            
                            fluidRow(
                              column(3, selectInput("line_variable", "Select Time Series Variable", 
                                                           c("RANK", "ADM_RATE", "UGDS", "COSTT4_A", "TUITIONFEE_IN", "TUITIONFEE_OUT",
                                                             "C150_4", "ACTCM25", "ACTCM75", "ACTCMMID", "NPT", "avg_yearly_cost","SAFETY_INDEX"),
                                                           selected = c("ADM_RATE"))),
                              
                              column(9, plotOutput("lineplot")),
                            ),
                            
                            fluidRow(
                              dataTableOutput("linetable")
                            )
                          )
                 )
)

server <- function(input, output, session) {
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
  
  ### PAGE 2 ###
  
  # Make the star plot school selection input options change based on page 1 filters
  observe({
    p1schools <- app_data()$NAME
    
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
    app_data() %>%
      select(NAME, input$star_stats) %>% 
      filter(NAME %in% c(input$star_school1,input$star_school2,input$star_school3))
  })
  
  # Convert data for line plot
  line_data <- reactive({
    line_data_convert(c(input$star_school1,input$star_school2,input$star_school3), input$line_variable)
  })
  
  output$lineplot <- renderPlot({
   lineplot(line_data(),input$line_variable)
  })
  
  output$linetable <- renderDataTable({
    line_data()
  })
  
}

shinyApp(ui, server)