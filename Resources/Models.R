# Statistical models

#### Linear analysis
fit = lm(dependent_var ~ independent_var, data=data)

##### Adjusted linear analysis: adjust for independent_var1 and independent_var2
fit1 = lm(dependent_var ~ independent_var + independent_var1 + independent_var2 , data=data)
summary(fit1)
confint(fit1)

##### Single continuous predictor: Glucose ~ BMI
plot(glucose ~ BMI, data=hers2)
plot(jitter(glucose, factor=2) ~ BMI, data=hers2, col="blue", pch=16, cex=0.3)
fit.cont = lm(glucose ~ BMI, data=hers2)
abline(fit.cont)
summary(fit.cont)


##### Single binary predictor: Glucose ~ exercise
boxplot(glucose ~ exercise, data=hers2)
t.test(glucose ~ exercise, data=hers2, var.equal=TRUE)
fit.bin = lm(glucose ~ exercise, data=hers2)
summary(fit.bin)

## Numeric non-indicator binary predictors
table(hers2$exercise)
hers2$Exerc = ifelse(hers2$exercise==1, 1, 2)
table(hers2$Exerc)
fit.bin2 = lm(glucose ~ Exerc, data=hers2) # incorrect!
summary(fit.bin2)

## Factor binary predictors
hers2$Exerc = as.factor(hers2$Exerc)
table(hers2$Exerc)
fit.bin3 = lm(glucose ~ Exerc, data=hers2) # correct!
summary(fit.bin3)

## Changing reference level
fit.bin4 = lm(glucose ~ relevel(Exerc, ref=2), data=hers2)
summary(fit.bin4)

## Character (text) binary predictors
hers2$Exx = ifelse(hers2$exercise==1, "Yes", "No")
table(hers2$Exx)
fit.bin5 = lm(glucose ~ Exx, data=hers2)
summary(fit.bin5)


##### Single categorical predictor: Glucose ~ physical activity level
table(hers2$physact)
boxplot(glucose ~ physact, data=hers2)

### One-way ANOVA, F-test
fit = aov(glucose ~ as.factor(physact), data=hers2)
summary(fit)

### Pairwise comparisons, Tukey correction
(tfit = TukeyHSD(fit))  # p-values, CI

### Pairwise comparisons, Holm (p-values)
pairwise.t.test(hers2$glucose, hers2$physact, p.adjust.method="holm")

#### One-way ANOVA as linear regression
hers2$physact.cat = as.factor(hers2$physact)
fit.cat = lm(glucose ~ physact.cat, data=hers2)
summary(fit.cat)
confint(fit.cat)



### Change reference level
hers2$physact5.cat = relevel(hers2$physact.cat, ref=5)
fit.cat5 = lm(glucose ~ physact5.cat, data=hers2); summary(fit.cat5)
confint(fit.cat5)  # 95% CI, not Bonferroni-corrected
confint(fit.cat5, level=1-0.05/10) # 95% CI, Bonferroni-corrected (c=10)


### Use contrast()
# install.packages("contrast")
library("contrast")
summary(fit.cat)
## compute difference b/w groups 5, 2
contrast(fit.cat, a=list(physact.cat="5"), b=list(physact.cat="2"))

## compute several differences at once: 2 vs 1, 3 vs 1, ..., 5 vs 4
lhs = list(physact.cat=c("2", "3", "4", "5", "3", "4", "5", "4", "5", "5"))
rhs = list(physact.cat=c("1", "1", "1", "1", "2", "2", "2", "3", "3", "4"))
contrast(fit.cat, a=lhs, b=rhs)

## compute differences w/ Bonferroni-corrected 95% CI
contrast(fit.cat, a=lhs, b=rhs, conf.int=1-0.05/10)

## compute Holm-corrected p-values
(pval.nominal = contrast(fit.cat, a=lhs, b=rhs)$Pvalue)
(pval.holm = p.adjust(pval.nominal, method="holm"))


#### Linear trend
fit.lin = lm(glucose ~ physact, data=hers2)
summary(fit.lin)

plot(glucose ~ physact, data=hers2, pch=16, cex=0.5)
points(1:5, with(hers2, tapply(glucose, physact, mean)), pch=15, col=2)
abline(fit.lin)

### Model matrix
mmfit.cat = model.matrix(fit.cat)
dim(mmfit.cat)
hers2$physact.cat[1:5]
mmfit.cat[1:5,]

mmfit.lin = model.matrix(fit.lin)
dim(mmfit.lin)
mmfit.lin[1:5,]


###### Adjusted analysis: adjust for age, drinking, BMI
fit.adj = lm(glucose ~ exercise + age + drinkany + BMI, data=hers3)
summary(fit.adj)
confint(fit.adj)

#### Analysis of variance table
summary(fit.adj)
sd(hers3$glucose) # sd.y
fit.null = lm(glucose ~ 1, data=hers3) # no predictors
anova(fit.null, fit.adj) # F-test for effect of predictors

#### F-test for individual covariates
## Variation explained by exercise, in addition to other covariates
fit.noexercise = lm(glucose ~ age + drinkany + BMI, data=hers3)
anova(fit.noexercise, fit.adj)

## Get all individual covariate F-tests at once
drop1(fit.adj, test="F")

## Warning! do NOT use this:
anova(fit.adj) # F-tests are sequential!

#### Eta squared, partial eta squared
# install.packages("lsr")
library(lsr)
etaSquared(fit.adj, anova=TRUE)


###### Visual representation
fit2 = lm(glucose ~ exercise + BMI, data=hers3)
summary(fit2)
beta = fit2$coefficients
plot(glucose ~ BMI, data=hers3, pch=16, cex=0.6,
     col=ifelse(exercise, 4, 2), cex.lab=1.5)
abline(a=beta[1], b=beta[3], col=2, cex=2)
abline(a=beta[1]+beta[2], b=beta[3], col=4, cex=2)
legend(x=30, y=60, lty=c(1,1), col=c(2,4), pch=c(16,16),
    legend=c("Non-exerciser", "Exerciser"))

#### Add regression lines for subset models
plot(glucose ~ BMI, data=hers3, pch=16, cex=0.6,
     col=ifelse(exercise, 4, 2), cex.lab=1.5)
fit21 = lm(glucose ~ BMI, data=hers3, subset=(exercise==0))
abline(fit21, col=2, lty=2, lwd=2)
fit22 = lm(glucose ~ BMI, data=hers3, subset=(exercise==1))
abline(fit22, col=4, lty=2, lwd=2)

#### Model with interaction
fit2int = lm(glucose ~ exercise * BMI, data=hers3)
summary(fit2int)

## Plot data with regression lines from interaction model
beta = fit2int$coefficients
plot(glucose ~ BMI, data=hers3, pch=16, cex=0.6,
     col=ifelse(exercise, 3, 6), cex.lab=1.5)
abline(a=beta[1], b=beta[3], col=6, cex=2)
abline(a=beta[1]+beta[2], b=beta[3]+beta[4], col=3, cex=2)
legend(x=30, y=60, lty=c(1,1), col=c(6,3), pch=c(16,16),
       legend=c("Non-exerciser", "Exerciser"))


#### Model with interaction, centered BMI
mean(hers3$BMI)
hers3$BMIc = hers3$BMI - mean(hers3$BMI)
fit23int = lm(glucose ~ exercise * BMIc, data=hers3)
summary(fit23int)
## beta_2, beta_3 same as in fit2int
## what is interpretation of beta_0, beta_1?

#### Additive model, centered BMI
fit23 = lm(glucose ~ exercise + BMIc, data=hers3)
summary(fit23)


###### Categorical predictors in multi-predictor models
hers4 = hers2[, c("glucose", "exercise", "age", "drinkany", "BMI", "physact")]
hers4 = na.omit(hers4); dim(hers4)  # same rows as hers3

table(hers4$physact)
boxplot(glucose ~ physact, data=hers4)
hers4$pa.cat = as.factor(hers4$physact)

fit.pa = lm(glucose ~ pa.cat + BMI, data=hers4)
summary(fit.pa)
confint(fit.pa)


#### Plot data + regression lines
plot(glucose ~ BMI, data=hers4, type="n", cex.lab=1.5)
points(glucose~BMI, data=hers4, subset=(physact==1), pch=16, cex=0.5, col=2)
points(glucose~BMI, data=hers4, subset=(physact==2), pch=16, cex=0.5, col=7)
points(glucose~BMI, data=hers4, subset=(physact==3), pch=16, cex=0.5, col=4)
points(glucose~BMI, data=hers4, subset=(physact==4), pch=16, cex=0.5, col=5)
points(glucose~BMI, data=hers4, subset=(physact==5), pch=16, cex=0.5, col=6)
beta = fit.pa$coefficients
abline(a=beta[1], b=beta[6], col=2)
abline(a=beta[1]+beta[2], b=beta[6], col=7)
abline(a=beta[1]+beta[3], b=beta[6], col=4)
abline(a=beta[1]+beta[4], b=beta[6], col=5)
abline(a=beta[1]+beta[5], b=beta[6], col=6)

#### F-test for covariates
drop1(fit.pa, test="F")
etaSquared(fit.pa, anova=TRUE)


#### Compare mean response for PA5 vs PA2, direct computation
summary(fit.pa, correlation=TRUE)
(se52 = sqrt(1.1211^2+1.0842^2-2*1.1211*1.0842*0.73))
(t.stat52 = (-3.2777-(-0.8584))/se52)
(p.val52 = 2*pt(-abs(t.stat52), df=2027))

## Use instead change of reference group
fit.pa2 = lm(glucose ~ relevel(pa.cat, ref=5), data=hers2); summary(fit.pa2)

#### Covariance of parameter estimates
(beta = fit.pa$coefficients)
sfit = summary(fit.pa)
names(sfit)
sfit$sigma
sfit$cov.unscaled
sfit$df
varbeta = sfit$sigma^2 * sfit$cov.unscaled
sqrt(diag(varbeta)) # comapre to SE of coefficients

#### SE for linear contrast beta4-beta1
a = c(0, -1, 0, 0, 1)
( abeta = t(a) %*% beta )  # t(a) transposed; %*% matrix multiplication
( var.abeta = t(a) %*% varbeta %*% a )
( se.abeta = sqrt(var.abeta) )
( tstat = abeta/se.abeta )
( pval = 2*pt(-abs(tstat), df=sfit$df[2]) )


###### Effect of PA on glucose, adjusting for BMI
fit.pa3 = lm(glucose ~ pa.cat + BMI, data=hers2)
summary(fit.pa3)

## Compare mean response for PA5 vs PA2 adjusting for BMI
summary(fit.pa3, correlation=TRUE)
(se52 = sqrt(1.09576^2+1.05201^2-2*1.09576*1.05201*0.72))
(t.stat52 = (-1.76912-(-1.07499))/se52)
(p.val52 = 2*pt(-abs(t.stat52), df=2027))

#### Estimate mean glucose for PA3, BMI=30, and 95% CI
## estimate yhat
a = c(1, 0, 1, 0, 0, 30)
beta = fit.pa3$coefficients
( yhat = t(a) %*% beta ) # I use yhat instead of abeta

## SE(yhat)
sfit = summary(fit.pa3)
varbeta = sfit$sigma^2 * sfit$cov.unscaled
var.yhat = t(a) %*% varbeta %*% a
( se.yhat = sqrt(var.yhat) )

## 95% CI for yhat
me = qt(0.975, df=sfit$df[2]) * se.yhat
( ci.yhat = c(yhat) + c(-1,1)*c(me) )


###### Effect of hormone therapy treatment on LDL at year 1
## Unadjusted analysis
fit.ht = lm(LDL1 ~ HT, data=hers)
summary(fit.ht) # HT reduced LDL by 15.2 mg/dL

## Adjust by statin use
fit.ht2 = lm(LDL1 ~ HT + statins, data=hers)
summary(fit.ht2) # adjusted effect of HT is similar, 15.4 mg/dL

## Does statin use modify/moderate effect of HT?
fit.ht3 = lm(LDL1 ~ HT * statins, data=hers)
summary(fit.ht3)

## ANOVA F-test for interaction
drop1(fit.ht3, test="F")

## Test H0:beta_1+beta_3=0 (HT effect for women on statins)
sfit = summary(fit.ht3)
( beta = sfit$coefficients[,1] )
varbeta = sfit$sigma^2 * sfit$cov.unscaled

a = c(0, 1, 0, 1)
( abeta = t(a) %*% beta ) 
( var.abeta = t(a) %*% varbeta %*% a )
( se.abeta = sqrt(var.abeta) )
( tstat = abeta/se.abeta )
( pval = 2*pt(-abs(tstat), df=sfit$df[2]) )
( c(abeta) +c(-1,1)*qt(0.975, df=sfit$df[2])*se.abeta ) # 95% CI for beta_1+beta_3

###### Effect of PA (3 levels) and BMI on glucose
hers2$pa3 = NA
hers2$pa3[hers2$physact ==1] = "low"
hers2$pa3[hers2$physact %in% c(2,3)] = "active"
hers2$pa3[hers2$physact %in% c(4,5)] = "veryactive"
hers2$pa3 = relevel(as.factor(hers2$pa3), ref="low")
table(hers2$pa3, hers2$physact)

## glucose ~ PA3 + BMI + PA3 x BMI
fit = lm(glucose ~ pa3 * BMI, data=hers2)
summary(fit)

## test for the interaction
drop1(fit, test="F")

### which slopes differ?
## Test H0: beta5-beta4=0
a = c(0, 0, 0, 0, -1, 1)
sfit = summary(fit)
# the rest of the code is copy-paste:
( beta = sfit$coefficients[,1] )
varbeta = sfit$sigma^2 * sfit$cov.unscaled
( abeta = t(a) %*% beta ) 
( var.abeta = t(a) %*% varbeta %*% a )
( se.abeta = sqrt(var.abeta))
( tstat = abeta/se.abeta )
( pval = 2*pt(-abs(tstat), df=sfit$df[2]) )

## Post-hoc comparison of 3 slopes with Holm correction
p.adjust(p=c(0.30, 0.0352, 0.0477), method="holm")

# 18. Simple linear Regression

fit = lm(col1 ~ col2, data=dataframe); summary(fit); abline(fit) # Provides intercept; r = estimate/sd(son) *sd(father) = cor.test(son, father)
#Residuals are vertical distance between y and the regression line; residuals: df = n-2, t0.975 = 1.965

# slope = intercept/sd(data$col1) * sd(data$col2)
# slope can be computed using 
cor.test(data$col1, data$col2) # WALD test

# Compute CI: 
confint(fit)
confint(fit, "exercise") # if multiple values can select the variable for which CI is needed else it prints all

#Simple linear regression: analysis of variance table (slide 29)
anova(fit)
summary(fit)

Mean square residual s^2 = RSS/(n-2)
R^2 = MSS/TSS

