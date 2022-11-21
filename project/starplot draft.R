library(tidyverse)
library(fmsb)

college <- read_csv("data/colleges.csv")

college_2020_perct <- college %>%
  filter(YEAR == 2020) %>% 
  mutate(ADM_RATE_PERC = ecdf(ADM_RATE)(ADM_RATE),
         UGDS_PERC = ecdf(UGDS)(UGDS),
         COSTT4_A_PERC = ecdf(COSTT4_A)(COSTT4_A),
         TUITIONFEE_IN_PERC = ecdf(TUITIONFEE_IN)(TUITIONFEE_IN),
         TUITIONFEE_OUT_PERC = ecdf(TUITIONFEE_OUT)(TUITIONFEE_OUT),
         C150_4_PERC = ecdf(C150_4)(C150_4)
         )  %>% 
  select(NAME,ADM_RATE_PERC,UGDS_PERC,COSTT4_A_PERC,TUITIONFEE_IN_PERC,TUITIONFEE_OUT_PERC,C150_4_PERC)


# Define the variable ranges: maximum and minimum
max_min <- data.frame(
  NAME = c("MAX","MIN"),
  ADM_RATE_PERC = c(1,0),
  UGDS_PERC = c(1,0),
  COSTT4_A_PERC = c(1,0),
  TUITIONFEE_IN_PERC = c(1,0),
  TUITIONFEE_OUT_PERC = c(1,0),
  C150_4_PERC = c(1,0)
)

# Bind the variable ranges to the data
college_2020_perct <- rbind(max_min, college_2020_perct)


select <- college_2020_perct %>% 
  filter(NAME %in% c("MAX","MIN","University of Wisconsin-Madison")) %>% 
  select(-NAME) %>% 
  create_beautiful_radarchart()


create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
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
