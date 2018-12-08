source("ParallelGraphs.R")

library(rowr)

dfGraph <- cbind.fill(married$detail_age,
                      divorced$detail_age,
                      single$detail_age,
                      widow$detail_age, fill=NA)

graphNames <- c("Married", "Divorced", "Single", "Widow")

output <- makeGraph(dfGraph, graphNames)

# source("Graph Functions/ggplot2_multiplot.R")
# multiplot(output, cols=2)

# test2 <- dfGraph
# colnames(test2) <- graphNames
# 
# ggplot(na.omit(stack(test2)), aes(x = ind, y = values, fill=ind)) +
#   geom_boxplot()

df <- dfGraph

TableSummary <- do.call(data.frame, 
                        list(mean = apply(df, 2, mean, na.rm=TRUE),
                             sd = apply(df, 2, sd, na.rm=TRUE),
                             median = apply(df, 2, median, na.rm=TRUE),
                             min = apply(df, 2, min, na.rm=TRUE),
                             max = apply(df, 2, max, na.rm=TRUE),
                             n = apply(df, 2, function(x){sum(!is.na(x))})))

row.names(TableSummary) <- graphNames

save(TableSummary, file="Summary Table.RData")
