

## The dataset Anaerob.csv includes the values of the oxygen uptake VO2, in liters/minute, and expired Ventilation (EV), for 53 subjects who performed a standard exercise task.

#### Fit the linear regression model Ventilation ∼ VO2 (model 1). Plot the data, with the regression line added to the scatterplot. Produce a residual plot for this model. Does this linear regression model provide an adequate fit to the data? Justify your answer.
anaerob = read.csv("/Datasets/Anaerob.csv")
fit.vo2 = lm(Ventilation ~ VO2, data=anaerob)
summary(fit.vo2)
library(car)
scatterplot(Ventilation ~ VO2, data=anaerob)

plot(fit.vo2, which=1, pch=16, cex=0.3) # residual plot
plot(fit.vo2, which=3, pch=16, cex=0.3) # SD plot

##### The residual plot shows a clear U-shaped trend, indicating that the linear model does not fit the data well. This curvature is also visible from the scatterplot of the response against the predictor.

#### An exercise physiologist suggests that the physiological relationship between EV and VO2 is better described by the equation EV = α0 ·exp(α1 ·V O2). Applying a logarithm transformation to both sides of this equation we get the linear model
#### ln(EV)=β0 +β1 ·VO2
#### where β0 = ln(α0) and β1 = α1. Fit the linear model suggested by the equation above (model 2). What are the estimates and 95%CI for the parameters β0, β1?
anaerob$lnEV = log(anaerob$Ventilation)
fit.lnvo2 = lm(ln.ev ~ VO2, data=anaerob)
summary(fit.lnvo2)
confint(fit.lnvo2)

##### Fitting the linear model with the log-transformed response, we get βˆ0 = 2.436, 95% CI (2.382, 2.490), and βˆ1 = 0.5674, 95% CI (0.5482, 0.5866).

#### Produce residual plots for model 2. Discuss if the model assumptions are satisfied.
plot(fit.lnvo2, which=1, pch=16, cex=0.3) # residual
scatterplot(ln.ev ~ VO2, data=anaerob) 
plot(fit.lnvo2, which=3, pch=16, cex=0.3) # scatter

##### The residual plots show that the linearity assumption is well satisfied. A few residuals for low values of EV and VO2 are larger than expected, therefore violating the nor- mality distribution. (This may have to do with the precision of measurement for either outcome or predictor.) The standard deviation of the residuals has a decreasing trend, most likely due to the same large residuals on the low end. This trend is borderline acceptable.

#### Show that observation #1 is an influential point. Upon re-checking the data, this observation is valid (not a recording error). Perform a sensitivity analysis re-fitting model 2 with this observation removed. What are the 95% CI for β0, β1 in this analysis? Indicate if the sensitivity analysis gives results that are consistent, qualitatively and/or quantitatively, with the analysis of the full data.
library(car)
infl = influence.measures(fit.lnvo2)
summary(infl) # pt 1 has dfb > 1 and significantly influences the model
scatterplot(ln.ev ~ VO2, data=anaerob, pch=16, cex=1.2, id=c(1))

remove_1_model = anaerob[-1,]
fit.rem = lm(ln.ev ~ VO2, data=remove_1_model)
summary(fit.rem)
confint(fit.rem)

##### The function car::influence.measures identifies observations #1 and #11 as poten- tially influential. Observation #1 has a large DFBETA value (-1.10) for the predictor. Running the model with observation #1 excluded we get that the model coefficients are quite similar to those from the original model, βˆ0 = 2.4067, βˆ1 = 0.5762, with small standard errors and highly significant association between predictor and outcome. The quantitative and qualitative conclusions are quite similar to those of the original model. We conclude that the inference in the initial model is robust, and is not affected by influential points.

#### We would like to investigate the relationship between LDL (“bad cholesterol”) levels and age in the HERS study (HERS.csv). Fit a linear model of LDL on age, adjusting for statin use (statins), alcohol use (drinkany), and race (nonwhite) as possible confounders. Summarize the effect of age on LDL based on this model, including confidence intervals and statistical significance.

hers = read.csv("/Datasets/HERS.csv")
fit.ldl = lm(LDL ~ age + statins + drinkany + nonwhite, data=hers)
summary(fit.ldl)
confint(fit.ldl)
plot(fit.ldl)

##### The (adjusted) coefficient of age is −0.2468 (ng/mL) per year, p-value = 0.0208, 95% CI (−0.4560, −0.0376) ng/mL/year. On average, the LDL decreases by 0.25 units per additional year of age.

#### Use graphical model diagnostics to check if the model assumptions are satisfied.

library(car)
avp1 = avPlot(fit, variable="age") # AVP for age

##### The plot of the residuals against fitted values shows no violation of linearity. (The two “blobs” are due to the strong effect of statin use on the predicted values.) The normal Q-Q plot shows that the residuals are right-skewed. The sample size is large, so the normality is not a concern. The square-root absolute residual plot shows that the standard deviation of residuals is approximately constant. A more sensitive investigation of linearity will involve the added variable plot, particularly for the continuous covariate age. This plot shows no concerns regarding linearity.

#### Create a new variable for ln(LDL): hers$ln.LDL = log(hers$LDL). Fit a similar linear model but with log(LDL) as the response. What is the effect of age on LDL based on this model? Include confidence intervals and p-value.
hers$ln.LDL = log(hers$LDL)
fit.lnldl = lm(ln.LDL ~ age + statins + drinkany + nonwhite, data=hers)
summary(fit.lnldl)
confint(fit.lnldl)
##### In the model for log-transformed LDL, the coefficient of age is -0.001193 logs/year, 95% CI (−0.002618, 0.0002315), p-value 0.101. On this scale, age no longer appears a significant predictor of (log) LDL, after adjusting for confounders.


#### Use graphical diagnostics to check if the assumptions are satisfied for this model.
plot(fit.lnldl, which=1, pch=16, cex=0.3)
scatterplot(ln.LDL ~ age, data=hers)
plot(fit.lnldl, which=3, pch=16, cex=0.3)
##### The same graphical diagnostics as for the previous model indicate no issues with linear- ity of age and with constant standard deviation. In addition, the residuals’ distribution is symmetric and closer to normal than on the original LDL scale.

#### Which of the two models is more defensible on statistical grounds? What are the conse- quences of this statistical choice on the scientific conclusions regarding the relationship between LDL and age?

##### The normality of the errors makes the ln(LDL) model preferable. In this model, age is no longer significantly associated with LDL. This suggests that the association with age in the non-transformed LDL model may be spurious. (E.g., there may be some interactions between the predictors in the non-transformed LDL model that are not accounted for in the model.)




##### The same graphical diagnostics as for the previous model indicate no issues with linear- ity of age and with constant standard deviation. In addition, the residuals’ distribution is symmetric and closer to normal than on the original LDL scale.
