library(tidyverse)
library(leaflet)
library(shiny)
library(fmsb)
library(bslib)
library(plotly)
theme_set(theme_minimal())

data <- read_csv("data/colleges_crime.csv") %>% 
  mutate_at(vars(contains("ACTCM")), funs(as.integer)) %>%
  mutate(
    CITY = paste0(CITY, ", ", STABBR),
    YEARLY_COST = round(COSTT4_A, digits = -1),
    UGDS = round(UGDS, digits = -1),
    TUITIONFEE_IN = round(TUITIONFEE_IN, digits = -1),
    TUITIONFEE_OUT = round(TUITIONFEE_OUT, digits = -1),
    TUITION = round(ifelse(STABBR == "WI", TUITIONFEE_IN, TUITIONFEE_OUT), digits = -1),
    selected = 1
  ) %>%
  rename(
    ADMIT_RATE = ADM_RATE,
    ACT_MEDIAN = ACTCMMID,
    COMPLETION_RATE = C150_4,
    NET_PRICE = NPT,
    UNDERGRAD_ENROLLMENT = UGDS
  ) %>%
  arrange(RANK) %>%
  drop_na()

data_2020 <- filter(data, YEAR == max(data$YEAR))

update_data <- function(home_state, act_score, min_admit, selectivity, type, cost_range, completion_range, size_range) {
  out <- data %>%
    filter(YEAR == max(data$YEAR)) %>%
    mutate(
      TUITION = round(ifelse(STABBR == home_state, TUITIONFEE_IN, TUITIONFEE_OUT), digits = -1),
      ADMIT_DIFFICULTY = case_when(
        ADMIT_RATE < .1 ~ "Reach", # Any school with admit rate <10% is reach, regardless of ACT score
        act_score > ACTCM75 ~ "Safety",
        act_score >= ACTCM25 ~ "Target",
        act_score < ACTCM25 ~ "Reach"
      ),
      inst_type = case_when(
        TYPE == "PRIV" ~ "Private",
        TYPE == "PUB" ~ "Public"
      ),
      selected = case_when(
        !(ADMIT_DIFFICULTY %in% selectivity) ~ 0,
        min_admit > ADMIT_RATE ~ 0,
        !(inst_type %in% type) ~ 0,
        NET_PRICE < cost_range[1] ~ 0,
        NET_PRICE > cost_range[2] ~ 0,
        COMPLETION_RATE < completion_range[1] ~ 0,
        COMPLETION_RATE > completion_range[2] ~ 0,
        UNDERGRAD_ENROLLMENT < size_range[1] ~ 0,
        UNDERGRAD_ENROLLMENT > size_range[2] ~ 0,
        TRUE ~ 1
      )
    )
  return(out)
}

histplot <- function(df, var, selected) {
  ggplot(df) +
    geom_histogram(
      aes(x = {{var}}, fill = factor(selected, levels = c(1, 0))), 
      position = position_stack(reverse = TRUE)
    ) +
    scale_fill_manual(values = c("#2C3E51", "#8BC3FF"), labels = c(1, 0)) +
    theme(
      legend.position = "none",
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks.y = element_blank()
    )
}

### PAGE 2 FUNCTIONS ###

# Star plot function
starplot <- function(data, schools){
  
  color <- c("#F8766D", "#00BFC4", "#7CAE00")
  
  par(mar=c(3,0,2,0))
  
  radarchart(
    data %>% select(-NAME), axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.07), plwd = 3, plty = 1,
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
    legend = data[-c(1:2),] %>% pull(NAME), horiz = TRUE,
    bty = "n", pch = 20 , col = c("#F8766D", "#00BFC4", "#7CAE00"),
    text.col = "black", cex = 1, pt.cex = 2
  )
  
}

# Setting up data for star plot
star_data_convert <- function(schools, star_stats){
  stats_all = c("RANK_SCORE", "ADMIT_RATE", "UNDERGRAD_ENROLLMENT", 
                "TUITION_AFFORDABILITY", "COST_AFFORDABILITY",
                "COMPLETION_RATE", "ACT_MEDIAN", 
                "NET_PRICE_AFFORDABILITY", "SAFETY_INDEX")
  #stats_all <- c("RANK", "ADMIT_RATE", "UNDERGRAD_ENROLLMENT", "TUITION", "YEARLY_COST", "COMPLETION_RATE", "ACT_MEDIAN", "NET_PRICE", "SAFETY_INDEX")
  stats_inv <- c("RANK_SCORE", "TUITION_AFFORDABILITY", "NET_PRICE_AFFORDABILITY", "COST_AFFORDABILITY")
  
  # Convert selected stats into percentiles using all 2020 schools
  star_data <- data %>%
    rename(RANK_SCORE = RANK, TUITION_AFFORDABILITY = TUITION, 
           NET_PRICE_AFFORDABILITY = NET_PRICE, COST_AFFORDABILITY = YEARLY_COST) %>% 
    filter(YEAR == max(data$YEAR)) %>% 
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
    filter(NAME %in% c("MAX","MIN", {{schools}})) %>% 
    arrange(match(NAME, c("MAX","MIN", {{schools}}))) %>% 
    as.data.frame()
  
  return(out)
}

star_table_out <- function(data, schools, star_stats){
  
  data = data %>%
    rename(RANK_SCORE = RANK, TUITION_AFFORDABILITY = TUITION, 
           NET_PRICE_AFFORDABILITY = NET_PRICE, COST_AFFORDABILITY = YEARLY_COST) %>%
    select(NAME, star_stats) %>% 
    filter(NAME %in% {{schools}}) %>% 
    arrange(match(NAME, {{schools}}))
  
  if("RANK_SCORE" %in% colnames(data)){data = data %>% rename(RANK = RANK_SCORE)}
  if("TUITION_AFFORDABILITY" %in% colnames(data)){data = data %>% rename(TUITION = TUITION_AFFORDABILITY)}
  if("NET_PRICE_AFFORDABILITY" %in% colnames(data)){data = data %>% rename(NET_PRICE = NET_PRICE_AFFORDABILITY)}
  if("COST_AFFORDABILITY" %in% colnames(data)){data = data %>% rename(YEARLY_COST = COST_AFFORDABILITY)}
  
  data
}

# Line plot function
lineplot = function(df, line_variable, schools){
  p=ggplot(mapping = aes(color = NAME)) +
    geom_line(data=df, aes(YEAR, .data[[line_variable]]), size = 1, alpha=0.8) +
    scale_x_continuous(breaks = min(df$YEAR):max(df$YEAR)) +
    scale_color_manual(values = c("#F8766D", "#00BFC4", "#7CAE00"), labels = {{schools}}) +
    labs(x = "Year", y = line_variable,
         title = paste0("Evolution of ", line_variable,
                        " (", min(df$YEAR), "-", max(df$YEAR), ")"), color = "") +
    theme(
      text = element_text(size = 11),
      plot.title = element_text(hjust = 0.5),
      legend.position = "bottom"
    )
  ggplotly(p, tooltip = line_variable) %>%
    style(hoveron = "fill")
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
  
  if(sum(star_school=="NA")<2){
    out$NAME = factor(out$NAME, levels=star_school)
  }
  
  return(out)
}

ui <- navbarPage("College Ranking",
                 tabPanel("Find Universities",
                          fluidPage(
                            titlePanel("Find Universities"),
                            fluidRow(
                              column(2, selectInput("home_state", "Home State", state.abb, selected = "WI")),
                              column(2, numericInput("act_score", "ACT Score", value = 36, min = 0, max = 36)),
                              column(4, sliderInput("min_admit", "Minimum Admission Rate", 0, 1, 0)),
                              column(2, checkboxGroupInput("selectivity", "Selectivity", c("Reach", "Target", "Safety"), selected = c("Reach", "Target", "Safety"))),
                              column(2, checkboxGroupInput("type", "Institution Type", c("Public", "Private"), selected = c("Public", "Private")))
                            ),
                            fluidRow(
                              column(4,
                                     plotOutput("cost", height = 100, width = 350),
                                     sliderInput("cost_range", NULL, min(data_2020$NET_PRICE), max(data_2020$NET_PRICE), c(min(data_2020$NET_PRICE), max(data_2020$NET_PRICE)), width = 350),
                                     plotOutput("completion", height = 100, width = 350),
                                     sliderInput("completion_range", NULL, min(data_2020$COMPLETION_RATE), max(data_2020$COMPLETION_RATE), c(min(data_2020$COMPLETION_RATE), max(data_2020$COMPLETION_RATE)), width = 350),
                                     plotOutput("size", height = 100, width = 350),
                                     sliderInput("size_range", NULL, min(data_2020$UNDERGRAD_ENROLLMENT), max(data_2020$UNDERGRAD_ENROLLMENT), c(min(data_2020$UNDERGRAD_ENROLLMENT), max(data_2020$UNDERGRAD_ENROLLMENT)), width = 350)
                              ),
                              column(8, leafletOutput("map", height = 500)),
                            ),
                            fluidRow(dataTableOutput("table")),
                            theme = bs_theme(bootswatch = "flatly")
                          )
                 ),
                 tabPanel("Compare Universities",
                          fluidPage(
                            titlePanel("Compare Universities"),
                            fluidRow(
                              column(4, selectInput("star_school1", "Select First School", unique(data$NAME), selected = NA)),
                              column(4, selectInput("star_school2", "Select Second School", c(NA,unique(data$NAME)), selected = character(0))),
                              column(4, selectInput("star_school3", "Select Third School", c(NA,unique(data$NAME)), selected = character(0)))
                            ),
                            fluidRow(
                              column(4, checkboxGroupInput("star_stats", "Select Starplot Stats (at least 3)", 
                                                           c("RANK_SCORE", "ADMIT_RATE", "UNDERGRAD_ENROLLMENT", 
                                                             "TUITION_AFFORDABILITY", "COST_AFFORDABILITY",
                                                             "COMPLETION_RATE", "ACT_MEDIAN", 
                                                             "NET_PRICE_AFFORDABILITY", "SAFETY_INDEX"),
                                                           selected = c("RANK_SCORE","ADMIT_RATE","COST_AFFORDABILITY","SAFETY_INDEX"))),
                              column(8, plotOutput("starplot"))
                            ),
                            fluidRow(
                              column(4, selectInput("line_variable", "Select Time Series Variable", 
                                                    c("RANK", "ADMIT_RATE", "UNDERGRAD_ENROLLMENT", "TUITION", "YEARLY_COST",
                                                      "COMPLETION_RATE", "ACT_MEDIAN", "NET_PRICE", "SAFETY_INDEX"),
                                                    selected = c("ADMIT_RATE"))),
                              
                              column(8, plotlyOutput("lineplot")),
                            ),
                            fluidRow(
                              dataTableOutput("startable")
                            )
                          )
                 )
)

server <- function(input, output, session) {
  data <- reactive({
    update_data(input$home_state, input$act_score, input$min_admit, input$selectivity, input$type, input$cost_range, input$completion_range, input$size_range)
  })
  
  selected <- reactive({
    data()$selected
  })
  
  output$cost <- renderPlot({
    histplot(data(), NET_PRICE, selected()) +
      labs(title = "Net Price of Attendance per Year")
  })
  
  output$completion <- renderPlot({
    histplot(data(), COMPLETION_RATE, selected()) +
      labs(title = "4-year completion rate")
  })
  
  output$size <- renderPlot({
    histplot(data(), UNDERGRAD_ENROLLMENT, selected()) +
      labs(title = "Undergraduate Enrollment Size")
  })
  
  output$map <- renderLeaflet({
    data() %>%
      leaflet(options = leafletOptions(minZoom = 4)) %>% 
      addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(noWrap = TRUE)) %>% 
      addCircles(lng = ~LONGITUDE, lat = ~LATITUDE, label = ~NAME, color = ~colorFactor(palette=c("#8BC3FF", "#2C3E51"), domain=selected)(selected)) %>% 
      setView(lng = -93.85, lat = 37.45, zoom = 4) %>% 
      setMaxBounds(lng1 = -40, lat1 = 10, lng2 = -160, lat2 = 60)
  })
  
  output$table <- renderDataTable({
    data() %>%
      filter(selected == TRUE) %>% 
      select(NAME, CITY, RANK, ADMIT_RATE, ADMIT_DIFFICULTY, TUITION, NET_PRICE) # RANK, CITY, STABBR, ACTCM25)
  })
  
  ### PAGE 2 ###
  
  # Make the star plot school selection input options change based on page 1 filters
  observe({
    p1schools <- data() %>% 
      filter(selected == 1) %>%
      pull(NAME)
    
    if (is.null(p1schools)){
      p1schools <- character(0)
    }
    
    updateSelectInput(session, "star_school1",
                      choices = p1schools,
                      selected = head(p1schools, 1)
    )
    
    updateSelectInput(session, "star_school2",
                      choices = c(NA,p1schools),
                      selected = character(0)
    )
    
    updateSelectInput(session, "star_school3",
                      choices = c(NA,p1schools),
                      selected = character(0)
    )
    
  })
  
  # Convert data for star plot
  star_data <- reactive({
    star_data_convert(c(input$star_school1,input$star_school2,input$star_school3), input$star_stats)
  })
  
  output$starplot <- renderPlot({
    starplot(star_data(), c(input$star_school1,input$star_school2,input$star_school3))
  })
  
  output$startable <- renderDataTable({
    # star_data()
    star_table_out(data(), c(input$star_school1,input$star_school2,input$star_school3), input$star_stats)
  })
  
  # Convert data for line plot
  line_data <- reactive({
    line_data_convert(c(input$star_school1,input$star_school2,input$star_school3), input$line_variable)
  })
  
  output$lineplot <- renderPlotly({
    lineplot(line_data(),input$line_variable, c(input$star_school1,input$star_school2,input$star_school3))
  })
  
}

shinyApp(ui, server)