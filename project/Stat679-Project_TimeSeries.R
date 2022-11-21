###############
# Time series #
###############

library(readxl)
library(tidyverse)
library(gganimate)
dat=read_xlsx("/Users/albertdorador/Desktop/Stat679/Project/data/Criminal_Offenses_On_campus.xlsx")

datMC = dat %>% filter(Campus_ID == 1)

UWM = datMC %>%
  filter(Institution_name == "University of Wisconsin-Madison")

CMU = datMC %>%
  filter(Institution_name == "Carnegie Mellon University")

p = ggplot(mapping = aes(color = Institution_name)) +
  geom_line(data=UWM, aes(Survey_year, Crime_per_capita)) +
  scale_x_continuous(breaks = c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020))+
  #transition_reveal(along = Survey_year)+
  geom_line(data=CMU, aes(Survey_year, Crime_per_capita)) +
  labs(x = "Year", y = "Crimes",
       title = "Crimes per capita on campus (2010-2020)", color = "")+
  theme_minimal()

#animate(p, fps=20, renderer = gifski_renderer(loop = FALSE),
#        height = 3, width = 7, units = "in", res = 150)
