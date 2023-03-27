1. Data summaries, tables, graphs

1.1 Categorical variables:
Tabulation (n, %)
tab = table(data$col) # values
prop.table(tab) # proportion

Cross-tabulation (n, %)
tab = table(data$col1, data$col2) # values
prop.table(tab) # proportion
prop.table(object, margin = 1) #sums to 1 by row instead of column

Barplots and clustered barplots
barplot(dbinom(0:10, size = 10, prob = 0.8))

1.2 Continuous variables:
Five point summary
summary(data$col)

Mean
mean(data$col)

Standard deviation
sd(data$col)

1.3 Analyses steps:
Read datasets

Skewness coefficient

<0.5 = symmetric
0.5 < x < 1 = moderate skewness
x > 1 = serious skewness
