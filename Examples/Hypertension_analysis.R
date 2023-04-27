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

