library(tidyverse)
library(readxl)

# get USNWR rankings data
usnwr <- read_xlsx("data/usnwr.xlsx", skip = 1)

# get IDs from USNWR rankings data
# ID for PSU changes to 495767 in most recent scorecard data
ID <- c(pull(usnwr, "IPEDS ID"), 495767)

# function to get scorecard data
get_scorecard <- function(filename) {
  YEAR <- str_replace(filename, "data/scorecard/", "") %>% 
    parse_number()
  
  scorecard <- read.csv(filename) %>% 
    filter(UNITID %in% ID) %>% 
    select(UNITID, INSTNM, CITY, STABBR, LATITUDE, 
           LONGITUDE, ADM_RATE, UGDS, NPT4_PUB, NPT4_PRIV, 
           COSTT4_A, TUITIONFEE_IN, TUITIONFEE_OUT, C150_4, 
           ACTCM25, ACTCM75, ACTCMMID) %>% 
    mutate(across(5:14, as.numeric), YEAR = YEAR) %>% 
    drop_na(ADM_RATE)
  
  return(scorecard)
}

# get scorecard data for past ten years
filenames <- paste0("data/scorecard/MERGED", 2011:2020, "_", 12:21, "_PP.csv")
data_list <- lapply(filenames, get_scorecard)
scorecard <- bind_rows(data_list)

# have all ten observations for PSU
# Thomas Jefferson U combined with Philadelphia U in 2017
scorecard %>% 
  group_by(UNITID) %>% 
  filter(n() < 10) %>% 
  View()

# change PSU ID to match USNWR rankings data
scorecard <- scorecard %>% 
  mutate(UNITID = replace(UNITID, UNITID == 495767, 214777))

# combine both datasets and write file
usnwr %>% 
  select(NAME = 1, UNITID = 2, as.character(2011:2020)) %>% 
  pivot_longer(3:12, names_to = "YEAR", values_to = "RANK") %>% 
  mutate(YEAR = as.double(YEAR)) %>% 
  left_join(scorecard) %>% 
  pivot_longer(starts_with("NPT4"), names_to = "TYPE", 
               names_prefix = "NPT4_", values_to = "NPT") %>% 
  drop_na(NPT) %>%
  select(-INSTNM) %>% 
  group_by(UNITID) %>% 
  mutate(LATITUDE = LATITUDE[!is.na(LATITUDE)][1L],
         LONGITUDE = LONGITUDE[!is.na(LONGITUDE)][1L]) %>% 
  write.csv("data/colleges.csv")
                    