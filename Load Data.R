library(readr)

AllData <- read_csv("\2015_data.csv")
AllData <- AllData[c(8,15)]

library(dplyr)
married <- AllData %>% filter(marital_status == "M")
married <- married %>% mutate(detail_age = as.numeric(detail_age))
married <- married %>% filter(detail_age >= 18)

single <- AllData %>% filter(marital_status == "S")
single <- single %>% mutate(detail_age = as.numeric(detail_age))
single <- single %>% filter(detail_age >= 18)

widow <- AllData %>% filter(marital_status == "W")
widow <- widow %>% mutate(detail_age = as.numeric(detail_age))
widow <- widow %>% filter(detail_age >= 18)

divorced <- AllData %>% filter(marital_status == "D")
divorced <- divorced %>% mutate(detail_age = as.numeric(detail_age))
divorced <- divorced %>% filter(detail_age >= 18)

save(AllData, single, married, file="data.RData")
