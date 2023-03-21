1. Data summaries, tables, graphs

1.1 Categorical variables:
Tabulation (n, %)
tab = table(data$col) # values
prop.table(tab) # proportion

Cross-tabulation (n, %)
tab = table(data$col1, data$col2) # values
prop.table(tab) # proportion

Barplots and clustered barplots


1.2 Continuous variables:
Five point summary
summary(data$col)

Mean
mean(data$col)

Standard deviation
sd(data$col)

1.3 Analyses steps:
Read datasets

