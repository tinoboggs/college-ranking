################
# Data Merging #
################

library(readxl)

dat=read_xlsx("data/Criminal_Offenses_On_campus.xlsx")
dat2=read_xlsx("data/colleges.xlsx")

dat_merged = merge(dat,dat2,by = c("UNITID", "YEAR"))
write_csv(dat_merged, "colleges_crime.csv")