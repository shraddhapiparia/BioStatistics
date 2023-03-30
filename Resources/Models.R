# 18. Simple linear Regression

fit = lm(col1 ~ col2, data=dataframe); summary(fit); abline(fit) # Provides intercept; r = estimate/sd(son) *sd(father) = cor.test(son, father)
#Residuals are vertical distance between y and the regression line; residuals: df = n-2, t0.975 = 1.965

# slope = intercept/sd(data$col1) * sd(data$col2)
# slope can be computed using 
cor.test(data$col1, data$col2) # WALD test

# Compute CI: 
confint(fit)

#Simple linear regression: analysis of variance table (slide 29)
anova(fit)
summary(fit)

Mean square residual s^2 = RSS/(n-2)
R^2 = MSS/TSS
