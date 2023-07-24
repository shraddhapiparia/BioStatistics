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
