##### Jackson et al. (2002), PMID: 12037649, report the results of the Heritage Family Study, a cross-sectional study investigating the effect of sex, age, and race on the estimation of percent body fat from BMI in n = 655 sedentary individuals aged 15-66. The data are in Jacson.csv. The variable names are self-explanatory (pctfat = percent body fat).

rm(list=ls())

jackson = read.csv("/Datasets/Jackson.csv")
dim(jackson)
jackson[1:5,]

#### Is there a statistical difference between men and women in mean percent body fat (PBF)? Draw a boxplot and compute the mean difference between groups and its 95% CI, using a linear model.

boxplot(pctfat ~ sex, data=jackson)

fit.sex = lm(pctfat ~ sex, data=jackson)
summary(fit.sex)

df= 653
SE = 0.7340

lowerCI = -9.0442 - (qt(.975,df) * SE)
upperCI = -9.0442 + (qt(.975,df) * SE)


#### Does age have an effect on PBF? Draw a scatterplot with the regression line, and perform a simple linear regression analysis. According to the regression model, what is the change in the average (population) PBF per 1 year of age, and its 95% CI?

plot(jitter(pctfat, factor=2) ~ age, data = jackson, col="blue", pch=16, cex=0.3)
fit.age = lm(pctfat ~ age, data = jackson)
abline(fit.age, col="Red")

summary(fit.age)


#### Fit a linear model, comparing PBF between men and women while adjusting for age. Write the regression equation.
fit.sexadj = lm(pctfat ~ sex + age, data = jackson)
summary(fit.sexadj)

# PBF = 21.22304 - 9.68664(sexMALE) + 0.31838(age)

#### Based on the model PBF ∼ sex + age, is there a statistical difference between men and women? What is the adjusted mean difference between groups (and 95% CI)?
##### There is a significant effect of sex after adjusting for age, with men having lower mean PBF by 9.687 points, 95% CI (8.405, 10.988) points, p-value < 10−15. The adjusted effect of sex is slightly stronger than the unadjusted effect.

#### Calculate and interpret the coefficient of determination R2 for the model, and the η2 for sex and age from the model above.
library(lsr)
etaSquared(fit.sexadj, anova=TRUE

##### Taken together, sex and age explain R2 = 0.363, or 36.3%, of the variance in PBF. Sex explains η2 = 21.5% of the variation in PBF, while age explains η2 = 17.5% of PBF.

#### Draw a scatterplot of PBF on age, with different symbols (e.g., different colors) for men and for women. Include the regression lines for men and women implied by the model above.
plot (jackson$pctfat ~ jackson$age, pch=16, cex = 0.6, col = ifelse(jackson$sex=="MALE", 4, 2), cex.lab = 1.7, xlab="PBF", ylab="Age")
beta = fit.sexadj$coefficients
abline(a=beta[1], b=beta[3], col=2, cex=2)
abline(a=beta[1]+beta[2], b=beta[3], col=4, cex=2)
legend(x=30, y=10, lty=c(1,1), col=c(2,4), pch=c(16,16),
       legend=c("Female", "Male"))

#### The scatterplot suggests that the increase with age may not be perfectly linear. To allow for a non-linear age effect, create a categorical version of the age variable, agecat,
#### with age binned in 10-year intervals: 15-24, 25-34, . . . , 55-65 (see below). Fit the model PBF ∼ sex + agecat. Show that age is a significant predictor in this model.
jackson$agecat <- cut(jackson$age, breaks = c(15, 25, 35, 45, 55, 65), right = FALSE)
fit.agecat <- lm(pctfat ~ sex + agecat, data = jackson)
summary(fit.agecat)

##### The summary of age variable confirms that age values are between 15.94 and 65.88. The F-test for categorical age is 45.54 with df = 4,652, p-value < 10−15. Categirical age is highly significant. An investigation of the coefficients indicates that indeed the age effect does not seem to be linear and it slows down after age 45.

#### Based on the model above, find which age groups differ significantly in PBF from the 35-44 group, after adjusting for sex. Use a correction for multiple comparisons (4 comparisons).
jackson$agecat_ref3 = relevel(jackson$agecat, ref=3)
fit.agecat_ref3 <- lm(pctfat ~ sex + agecat_ref3, data = jackson)
coorected = TukeyHSD(aov(fit.agecat_ref3), alpha=0.5/4)
(coorected)

##### We re-fit the model using 35-44 group as the reference. The mean PBF is signficantly lower for the 15-24 and 25-34 age groups, compared to age 35-44, whereas age groups 45-54 and 55-65 do not differ significantly in mean PBF from 35-44 age group. (This is true with or without a Bonferroni correction.)


#### The models PBF ∼ sex + age and PBF ∼ sex + agecat are not nested, due to the binning of age into groups. For this reason, they cannot be compared directly using an F-test. Based on the proportion of outcome variance explained, which of the two models is better?
##### The coefficient of determination R2 is 36.32% for the model with continuous age, and 36.65% for the model with binned age, giving the latter a very slight advantage. (Note however that the adjusted R2, which corrects for the model degrees of freedom, is almost identical for the two models, 36.13 and 36.16%.)

#### Generally, overweight is defined as having a BMI ≥ 25. Create a binary variable, indicator of overweight. Use this variable as the outcome in this problem. What are the number, proportion, and odds of overweight participants, for the a) men, and b) women in this study?

jackson$ovrwt = (jackson$bmi >= 25) + 0
tab = table(jackson$sex, jackson$ovrwt)
prop.table(tab, margin=1)
(odds_ovrwt_women = tab[,2]/tab[,1])

#### Fit the logistic regression model overweight ∼ sex. Show that the predicted odds and probability of overweight for men and women from this model match the observed odds and proportions computed above.

fit.sex = glm(ovrwt ~ sex, family=binomial, data=jackson)
summary(fit.sex)
beta = fit.sex$coefficients
( logodds = c(beta[1], beta[1]+beta[2]) )

(Intercept) (Intercept)
-0.09477841  0.41109893

( odds = exp(logodds) )
(Intercept) (Intercept)
  0.9095745   1.5084746

( prob = odds/(1+odds) )
(Intercept) (Intercept)
  0.4763231   0.6013514

##### Based on the model, the log-odds of overweight for women and men are βˆ0 = −0.09478 and βˆ0 + βˆ1 = 0.41110.
##### The odds of overweight are 0.9096 and 1.5085.
##### Finally, the proportions of overweight are 0.476 and 0.601. These numbers match those from question 1.

#### Based on this logistic regression model, compute the odds ratio of overweight for men vs women, and its 95% confidence interval. Is the proportion of overweight different between men and women, based on this analysis?
exp(beta)
exp(confint.default(fit.sex))

##### The OR of overweight, men vs women, is 1.658, Wald 95% CI (1.214, 2.264). The difference is significant, Wald p-value = 0.00146.

#### Consider now the single-predictor logistic regression model 1: overweight ∼ age, where age is treated as a continuous predictor. Interpret the model coefficients, including the sign of the slope.
fit.age = glm(overwt ~ age, family=binomial, data=jackson)
summary(fit.age)
##### The intercept βˆ0 = −2.0148 estimates the log-odds of overweight for newborns. This is outside the range of the data, and should not be interpreted substantively. The slope βˆ1 = 0.0630 is the log-OR of overweight per year of age. The positive sign indicates an increase in odds of overweight with age, or OR>1 per year of age.

#### What is the odds ratio of overweight per one year of age, based on this model? (Include a confidence interval.) Is overweight significantly increasing or decreasing with age?
( or = exp(model1$coefficients[2]) )
( ci.or = exp(confint.default(model1, "age")) )
##### The OR of overweight per year of age is exp(βˆ1) = 1.065, Wald 95% CI (1.051, 1.079). Older age is significantly associated with an increase in overweight, p-value < 10−15.

#### What is the odds ratio of overweight per ten years of age, and its confidence interval?
or^10
ci.or^10
##### The log OR of overweight per 10 years of age is the difference in log-odds βˆ0 + βˆ1(x + 10) − (βˆ0 + βˆ1x) = 10βˆ1.
##### The odds ratio of overweight per 10 years of age is exp(10βˆ1) = (exp(βˆ1))10, or the yearly odds ratio to the power of 10.
##### The calculations apply to the confidence interval as well. The OR per 10 years of age is 1.878, 95% CI (1.643, 2.146).

#### Create a variable age.dec where age is grouped in decades: 15-19, 20-29, . . . , 60-69. Fit the logistic regression model 2: overweight ∼ [age.dec], where the square brackets signify that age.dec is a categorical variable. Is grouped age, overall, a significant predictor of overweight? Use the appropriate statistical test.
jackson$age.dec = floor(jackson$age/10)*10
table(jackson$age.dec)
fit.age_factor = glm(overwt ~ as.factor(age.dec), family=binomial, data=jackson)
summary(fit.age_factor)
drop1(fit.age_factor, test="LRT")
##### According to the likelihood ratio test, categorical grouped age is a significant predictor of overweight, p-value < 10−15.

#### What is the interpretation of the six model parameters?
##### The intercept βˆ0 is the (estimated) log-odds of overweight for the reference group, 15-19 years-old.
##### βˆ1 is the (estimated) log OR of overweight for the 20-29 group compared to the reference 15-19 group.
##### Similarly, βˆ2, . . . , βˆ5 are the log OR of overweight for each of the other age groups compared to the reference 15-19 year-old group.

#### What is the odds ratio of overweight for a person in the 30-39 age range, compared to someone in the 15-19 range? Include a confidence interval. Is the difference in the risk of overweight between the groups significant?
exp(fit.age_factor$coefficients[3])
exp(confint.default(fit.age_factor, "as.factor(age.dec)30"))
exp(confint.default(fit.age_factor, "as.factor(age.dec)30", level=1-0.05/15)) # Bonferroni
##### The OR under question is computed by exp(βˆ2) = exp(2.3224) = 10.20.
##### If this group comparison is of direct/hypothesized interest, then no correction for mul-
##### tiple comparisons is needed: 95%CI = (5.299, 19.633), p-value = 3.61 · 10−12.
##### If this group comparison is in the context of all other pairwise comparisons, then we need to correct for the 6 · 5/2 = 15 comparisons. With a Bonferroni correction, 95%CI = (3.826, 27.194), p-value = 5.4 · 10−11.

#### What is the odds ratio of overweight for a person in the 30-39 age range, compared to someone in the 20-29 range? Include a confidence interval, and a p-value for the appropriate test.
sfit = summary(fit.age_factor)
( beta = sfit$coefficients[,1] )
varbeta = sfit$cov.scaled
a = c(0, -1, 1, 0, 0, 0)
( abeta = t(a) %*% beta ) # estimate of beta2-beta1
( var.abeta = t(a) %*% varbeta %*% a )
se.abeta = sqrt(var.abeta)
( zstat = abeta/se.abeta )
( pval = 2*pnorm(-abs(zstat)) )  # p-value, nominal
exp(abeta) # 
## 95% CI for beta2-beta1 (uncorrected)
( ci.abeta = c(abeta) + c(-1,1)*1.96*c(se.abeta) )
exp(ci.abeta) # 95% CI for OR, uncorrected
## 95% CI for beta2-beta1, OR, Bonferroni-corrected
(ci.abeta.bonf= c(abeta)+c(-1,1)*qnorm(1-0.05/2/15)*c(se.abeta))
exp(ci.abeta.bonf) # 95% CI for OR, Bonferroni-corrected
pval * 15
##### The log OR in this case corresponds to the contrast β2 − β1. Using our method for computing contrasts, we get OR = 4.370, 95% CI = (2.682, 7.121), p-value = 3.2 · 10−9 (uncorrected).

#### Consider model 3: overweight ∼ age.dec, where age.dec, the age in decades rounded down created in question 7, is treated as a numeric variable. This model as- sumes a linear trend in the effect of age groups on risk (log-odds) of overweight. Inter- pret the model coefficients. Based on this model, what is the odds ratio of overweight for a person in the 40-49 age range, compared to someone in the 30-39 range?
model3 = glm(overwt ~ age.dec, family=binomial, data=jackson)
summary(model3)
exp(10*model3$coefficients[2])
##### The variable created in question 7 can simply be treated as a numeric variable in the logistic regression, representing the age in decades rounded down (10, 20, . . . , 60).
##### The intercept βˆ0 = −1.641 gives the log-odds of overweight for someone who is 0 years old - this is not interpretable.
##### The slope βˆ1 = 0.0612 gives the log OR per year of age, in the context of this decade- binned variable. A more relevant slope would be 10βˆ1 = 0.612, which corresponds to the log OR per decade, or per each jump from one age group to the next.
##### Since 40-49 and 30-39 age groups are consecutive bins, differing by 10 years of age in the age.dec grouping, the odds ratio of overweight in comparing these two groups is OR = exp(10βˆ1) = 1.844.


#### Using an appropriate test, show that model 2 fits the data better than model 3. Secondly, compare the coefficients of model 2 and model 3: does this help explain why model 2 is better than model 3?
anova(model3, model2, test="LRT")
(beta = model2$coefficients)
jumps = c(0, beta[-1])
decade = c(10, 20, 30, 40, 50, 60)
# intercept removed; 0 used as basis for reference age
plot(jumps ~ decade, pch=16, ylim=c(0, 3.2), col="blue", ylab="log OR vs 15-19yo group")
lines(jumps ~ decade, col="blue")
## add the linear model 3 to the plot
slope = model3$coefficients[2]
points(slope*(decade-10) ~ decade, col="red", pch=16)
lines(slope*(decade-10) ~ decade, col="red")
legend(x="bottomright", legend=c("model 2", "model 3"),
+        lty=c(1,1), col=c("blue", "red"))
##### Model 3 is nested within model 2: indeed, model 3 (linear effect of grouped age) corresponds to model 2, when the coefficients β1, . . . , β5 show a linear pattern (see also the last slide of lecture 6).
##### The likelihood ratio test is significant, G2 = 24.8, df = 4, p-valu e = 5.4 · 10−5.
##### The improvement in model fit from model 3 to model 2 is worth the added model
##### complexity.
##### Examining the model coefficients helps explain why model 3 is better than model 2:
##### According to model 3, each jump from one decade to the next corresponds to a log OR of 10βˆ1 = 0.612.
##### However, model 2 coefficients suggest that the jump from decade to decade is not linear, with the log OR for 20 compared to 10 of 0.85, then approximately constant log OR for all decades from 30 on compared to 10 (2.32 ≈ 2.28 ≈ 2.64 ≈ 2.02).
##### This can be illustrated on the plot below. Model 3 is an oversimplification of the age effect, not supported by the data.




