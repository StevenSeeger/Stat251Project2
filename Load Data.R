# library(readr)
# 
# AllData <- read_csv("\2015_data.csv")
# AllData <- AllData[c(8,15)]
# 
# save(AllData, file="data.RData")

library(dplyr)
married <- AllData %>% filter(marital_status == "M")
married <- married %>% mutate(detail_age = as.numeric(detail_age))
married <- married %>% filter(detail_age >= 18 & detail_age <= 150)

single <- AllData %>% filter(marital_status == "S")
single <- single %>% mutate(detail_age = as.numeric(detail_age))
single <- single %>% filter(detail_age >= 18 | detail_age <= 150)

widow <- AllData %>% filter(marital_status == "W")
widow <- widow %>% mutate(detail_age = as.numeric(detail_age))
widow <- widow %>% filter(detail_age >= 18 | detail_age <= 150)

divorced <- AllData %>% filter(marital_status == "D")
divorced <- divorced %>% mutate(detail_age = as.numeric(detail_age))
divorced <- divorced %>% filter(detail_age >= 18 | detail_age <= 150)


