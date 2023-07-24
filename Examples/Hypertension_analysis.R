##### Smith et al. (2017), PMC6410709, reports on a study of UCSD Student-Run Free Clinic patients diagnosed and treated for hypertension. The dataset Hypertension.csv includes 409 study participants, and was previously discussed in Biostatistics 1. The participants have their systolic blood pressure (SBP) measured at study baseline, and at a 6-12 month following hypertension treatment. A number of baseline characteristics are also recorded.


#### 2. Hypertension study, compare baseline-followup
###### Clean working directory
rm(list=ls())

ht = read.csv("./Datasets/Hypertension.csv")
View(ht)

## Boxplot of SBP.Baseline, SBP.Followup
boxplot(ht$SBP.Baseline, ht$SBP.Followup)

with(ht, boxplot(SBP.Baseline, SBP.Followup, 
                 names = c("Baseline", "Followup"), ylab="Systolic blood pressure", 
                 col="skyblue", boxwex=0.5, cex.lab=1.5, cex.axis=1.5) )

## Boxplot of differences (change from baseline)
boxplot(ht$SBP.Change); abline(h=0) # most people reduce SBP

with(ht, boxplot(SBP.Change, ylab="Change in SBP",
                 col="coral", cex.lab=1.5, cex.axis=1.5))
abline(h=0, col=8)

## Spaghetti plot - create a pdf
pdf(file="HtSpaghetti.pdf", width=7, height=7)
xbase = rep(1, dim(ht)[1])
xfup = rep(2, dim(ht)[1])
plot(1:2, type="n", xlab="", ylab="Systolic blood pressure", xaxt="n", 
     cex.lab=1.5, cex.axis=1.5,
     xlim=c(0.9, 2.1), ylim=range(c(ht$SBP.Baseline, ht$SBP.Followup)))
segments(x0=xbase, y0=ht$SBP.Baseline, x1=xfup, y1=ht$SBP.Followup,
         col=(1:length(xbase)))
segments(x0=1, y0=mean(ht$SBP.Baseline), x1=2, y1=mean(ht$SBP.Followup),
         col=1, lwd=4) # add thick line for means
dev.off()     # prints out the plot

## One-sample t-test for change from baseline
t.test(ht$SBP.Change)

## Paired-samples t-test (equivalent)
t.test(ht$SBP.Followup, ht$SBP.Baseline, paired=TRUE)

#### Hypertension study: compare homeless status by site
(tabH = matrix(c(29, 39, 4, 0, 167, 103, 109, 13), ncol=2))

### step 1: overall test
chisq.test(tabH)  # Not appropriate (warning); 2 cells < 5
fisher.test(tabH)

### step 2: pairwise comparisons
chisq.test(tabH[c(1,2),], cor=F)     # DT vs PB

chisq.test(tabH[c(1,3),], cor=F)     # DT vs MV

chisq.test(tabH[c(1,4),], cor=F)     # DT vs LG, small samples
fisher.test(tabH[c(1,4),])

chisq.test(tabH[c(2,3),], cor=F)     # PB vs MV

chisq.test(tabH[c(2,4),], cor=F)     # PB vs LG, small samples
fisher.test(tabH[c(2,4),])

chisq.test(tabH[c(3,4),], cor=F)     # MV vs LG, small samples
fisher.test(tabH[c(3,4),])

pairwise.prop.test(x=c(145,577,299),n=c(190,838,350), p.adj="bonf", correct=FALSE)

#### erform single-predictor analyses for this outcome, for each of the predictors listed below. Use the lm() function for all analyses. For each predictor test whether the predictor is associated with the response, and quantify numerically the difference between groups, based on the appropriate regression coefficient(s). 
#### In each case include the appropriate graph. Perform the analysis for the following predictors:
#### Linear regression models
dim(ht)
ht[1:5,]

# age - continuous predictor
plot(jitter(SBP.Change, factor=2) ~ Age, data = ht, col="blue", pch=16, cex=0.3)
fit.age = lm(SBP.Change ~ Age, data = ht)
abline(fit.age)
summary(fit.age)

# gender - binary predictor
boxplot(SBP.Change ~ Gender, data=ht)
ht$GenderInt = ifelse(ht$Gender=="male",0,1)
table(ht$GenderInt)
fit.gender = lm(SBP.Change ~ GenderInt, data = ht)
summary(fit.gender)

# ethnicity - categorical predictor
boxplot(SBP.Change ~ Ethnicity, data=ht)
ht$Ethnicity.cat = as.factor(ht$Ethnicity)
fit.ethnicity = lm(SBP.Change ~ Ethnicity.cat, data = ht)
summary(fit.ethnicity)
# correction 
library("contrast")
#ht$EthnicityInt = ifelse(ht$Ethnicity=="Asian","1",ifelse(ht$Ethnicity=="Black","2",ifelse(ht$Ethnicity=="Caucasian","3",ifelse(ht$Ethnicity=="Hispanic","4","5"))))
lhs = list(Ethnicity.cat=c("Black", "Caucasian", "Hispanic", "Other", "Caucasian", "Hispanic", "Other", "Hispanic", "Other", "Other"))
rhs = list(Ethnicity.cat=c("Asian", "Asian", "Asian", "Asian", "Black", "Black", "Black", "Caucasian", "Caucasian", "Hispanic"))
contrast(fit.ethnicity, a=lhs, b=rhs)

# homeless status - binary predictor
boxplot(SBP.Change ~ Homeless, data=ht)
table(ht$Homeless)
fit.homeless = lm(SBP.Change ~ Homeless, data = ht)
summary(fit.homeless)

# primary language - binary predictor
table(ht$Language)
boxplot(SBP.Change ~ Language, data=ht)
ht$LanguageInt = ifelse(ht$Language=="English",0,1)
table(ht$LanguageInt)
fit.language = lm(SBP.Change ~ LanguageInt, data = ht)
summary(fit.language)


# diabetes - binary predictor
table(ht$Diabetes)

boxplot(SBP.Change ~ Diabetes, data=ht)
fit.diabetes = lm(SBP.Change ~ Diabetes, data = ht)
summary(fit.diabetes)

# baseline SBP - continuous predictor
plot(SBP.Change ~ SBP.Baseline, data=ht, col="blue", pch=16, cex=0.3)
fit.baselineSBP = lm(SBP.Change ~ SBP.Baseline, data = ht)
abline(fit.baselineSBP)
summary(fit.baselineSBP)

# find if ethnicity is a significant predictor of SBP.Change (change in systolic blood pressure from baseline)
fit = lm(SBP.Change ~ Ethnicity, data=ht)
summary(fit)
## Ethnicity is a significant categorical predictor, F-test statistic is 3.95 on 4 and 404 df, p-value = 0.0037.

# effect of race/ethnicity on SBP.Change adjusting for baseline SBP.
fit.adj = lm(SBP.Change ~ Ethnicity + SBP.Baseline, data=ht)
summary(fit.adj)
drop1(fit.adj, test="F")
## In the adjusted model, ethnicity has Fstat = 2.095 on (4, 403) df , p-value = 0.081. Ethnicity is not significant at the 0.05 level, but there is trend-level evidence of association with change in SBP, adjusting for baseline SBP.

# difference in mean SBP.Change between Black and Hispanic groups
sfit = summary(fit.adj)
beta = sfit$coefficients[,1]
varbeta = sfit$sigma^2 * sfit$cov.unscaled > a = c(0, 1, 0, -1, 0, 0)
abeta = t(a) %*% beta # -11.48394
var.abeta = t(a) %*% varbeta %*% a
se.abeta = sqrt(var.abeta) # 3.570
## 95% CI for abeta = a'beta 
me = qt(0.975, df=sfit$df[2]) * se.abeta
 ( ci.abeta = c(abeta) + c(-1,1)*c(me) )
[1] -8.754243  5.280483
## 95% CI for abeta, with Bonferroni correction
 meBonf = qt(1-0.05/10/2, df=sfit$df[2]) * se.abeta
 ( ciBonf.abeta = c(abeta) + c(-1,1)*c(meBonf) )
[1] -11.812346   8.338586
## Test beta_1-beta_3 = 0, or a'beta=0
 tstat = abeta/sqrt(var.abeta) # -0.4865
 ( pval = 2*pt(-abs(tstat), df=sfit$df[2]) )
            [,1]
  [1,] 0.6268238

## The difference between the two groups is βˆ1 − βˆ3 = −11.54684 − (−9.80996) = −1.737 mm Hg.
To compute the 95% CI, one option is to compute the contrast between the Black and Hispanic group directly from the model with the Asian group as reference. The corresponding linear combination vector is a = c(0, 1, 0, −1, 0, 0). The 95% CI is (−8.75, 5.28) mm Hg, without adjustment for multiple comparisons. This CI is appropriate if the Black vs Hispanic comparison is of primary interest.
If however the comparison is in the context of all race/ethnicity pairwise comparisons, an adjustment for multiple comparisons should be done. A Bonferroni correction corresponds to a comparison at the level 0.05/10 = 0.005, and a corresponding 1 − 0.05/10 confidence interval. The Bonferroni-corrected 95% CI is (−11.81, 8.34) mm Hg.

# interpret the coefficient for SBP.Baseline in substantive terms.

## For each additional mm Hg of baseline SBP, the change in SBP from baseline receives an additional reduction of 0.612 mm Hg, on average.

# Interpret your findings.

## The change from baseline in SBP depends on baseline SBP. The higher the baseline SBP, the higher the reduction in SBP.
After adjusting for baseline SBP, the change from baseline in SBP does not depend significantly on race. However, there is some trend-level evidence of such an association, with less of a reduction among the Asian group and more of a reduction among the Black group.

# Check if the effect of race/ethnicity on SBP.Change depend on baseline SBP

fit.int = lm(SBP.Change ~ Ethnicity*SBP.Baseline, data=ht)
summary(fit.int)
drop1(fit.int, test="F")

## The interaction is significant, Fstat = 3.45, df = (4, 399), p-value = 0.0086.

# model equation for the Caucasian patients. Also check the coefficient for SBP.Baseline in this equation, and its 95% CI (no corrections for multiple comparisons).

sfit = summary(fit.int)
beta = sfit$coefficients[,1]
varbeta = sfit$sigma^2 * sfit$cov.unscaled
a = c(0, 0, 0, 0, 0, 1, 0, 1, 0, 0)
abeta = t(a) %*% beta # -0.5446
var.abeta = t(a) %*% varbeta %*% a
se.abeta = sqrt(var.abeta) # 0.06638
## 95% CI for abeta = a'beta
me = qt(0.975, df=sfit$df[2]) * se.abeta > ( ci.abeta = c(abeta) + c(-1,1)*c(me) )
[1] -0.6750887 -0.4140779

## The equation for Caucasian patients is SBP.Change = (48.0761 + 21.4036) + (−0.3193 − 0.2253)· SBP.Baseline, or SBP.Change = 69.480 − 0.5446· SBP.Baseline. The coefficient for SBP.Baseline is βˆ5+βˆ7 = −0.5446. Its 95% CI is (−0.6751, −0.4141).

# model equation for the Black patients

a = c(0, 0, 0, 0, 0, 1, 1, 0, 0, 0) 
abeta = t(a) %*% beta # -1.01311
var.abeta = t(a) %*% varbeta %*% a 
se.abeta = sqrt(var.abeta) # 0.1208 > ## 95% CI for abeta = a'beta
me = qt(0.975, df=sfit$df[2]) * se.abeta
( ci.abeta = c(abeta) + c(-1,1)*c(me) )

## The equation for Black patients is SBP.Change = (48.0761 + 93.4180) + (−0.3193 − 0.6938)· SBP.Baseline, or SBP.Change = 141.4941 − 1.0131· SBP.Baseline. The coefficient for SBP.Baseline is βˆ5+βˆ6 = −1.0131. Its 95% CI is (−1.2506, −0.7756).

# mean SBP.Change for hispanic patients with a baseline SBP of 140 mm Hg with a 95% CI

a = c(1, 0, 0, 1, 0, 140, 0, 0, 140, 0)
abeta = t(a) %*% beta # -6.952
var.abeta = t(a) %*% varbeta %*% a
se.abeta = sqrt(var.abeta) # 1.284
## 95% CI for abeta = a'beta
me = qt(0.975, df=sfit$df[2]) * se.abeta 
( ci.abeta = c(abeta) + c(-1,1)*c(me) )
[1] -9.477159 -4.426650

## For Hispanic patients with baseline SBP of 140 mm Hg, the mean SBP.Change is SBP.Change = (48.0761 + 27.7504) + (−0.3193 − 0.2720) · 140 = βˆ0 + βˆ3 + (βˆ5 + βˆ8) · 140 = −6.95 mm Hg. The 95% CI for this value is (−9.477, −4.427) mm Hg.



