# One sample continuous outcome
1. Binomial distribution 
dbinom(x=10, size = 10, prob=0.8) # pr of 10 out of 10 surviving
dbinom(x=8, size=10, prob=0.8)    # pr of 8 out of 10 surviving
barplot(dbinom(0:40, size=40, prob=0.3))

pnorm() # probability norm
qnorm(c(0.05,0.95)) # quantile norm z score

2. Student t confidence interval

xbar=2.4; s=1.8; n=20
sem = s/sqrt(n)
me = qt(0.975, df=n-1)*sem # margin of error c(xbar, xbar-me, xbar+me) # print estimate + 95% CI


3. One sample t-test

## Check assumptions
table(iwk$group)
boxplot(Age1 ~ group, data=iwk)
with(iwk, tapply(Age1, group, sd))       # comparable sd's
t.test(pa.dm2$Charlson)

## Hand calculations for t.test, 95% CI
qt(0.975, df=35)           # critical value = 2.03
pt(-0.747, df=35)*2        # p-value = 0.46

# Paired-samples t-test: same subject but different intervention or follow-up before and after to identify if difference is significant
t.test(ht$SBP.Followup, ht$SBP.Baseline, paired=TRUE)

# Two samples continuous outcome
Assumption 1: Both samples have normally distributed data or if skewed, large samples with size >= 30
Assumption 2: Equal SD or SD ratio < 2.

4. Two-independent samples t-test: use when assumptions satisfy
t.test(Age1 ~ group, data=iwk, var.equal=TRUE)

## Calculate critical value and p-value
qt(0.975, df=n-2) # 95% CI
pt(tstat, df=n-2)*2

5. Welch t-test: use when assumption 1 satisfied and 2 not satisfied
t.test(Age1 ~ group, data=iwk, var.equal=FALSE) # default var.equal is FALSE

6. Wilcoxon rank sum test: Non-parametric tests (no assumption is satisifed); tests median values instead of mean
wilcox.test(iwtdaily ~ group, data=iwk) 
or 
wilcox.test(iwt.control, iwt.treatment)

## To print confidence interval use:
wilcox.test(iwt.control, iwt.treatment, conf.int=TRUE)

## verify Wilcoxon normal approximation (illustration)
nC = table(iwk$group)[1]; nT = table(iwk$group)[2]; n=nC+nT
iwk$iwtd.rank = rank(iwk$iwtdaily)
with(iwk, tapply(iwtd.rank, group, mean))
rc = mean(iwk$iwtd.rank[iwk$group==1])
expected.rc = (n+1)/2
se.rc = sqrt(nT*(n+1)/(12*nC))
zstat = (rc-expected.rc)/se.rc
pvalue = 2*pnorm(-abs(zstat))

## Paired wilcoxon rank sum test
wilcox.test(iwk$PCS1, iwk$PCS2, paired=TRUE)
wilcox.test(iwk$PCS_change, mu=0)     # same

## Sample size calculation 
power.t.test(n=40, sd=7.4, power=0.8, type = “two.sample”)

# One sample binary outcome
Assumption: Normal approximation need at least 5 success (np >5 (s = 5)); failures >5
prop.test (x=number of interest, n = total sample); provides confidence interval and proportion
binom.test(x=number of interest, n = total, probability =p0); Provides p value, 95% confidence interval; probability of success

## paired test
binom.test(x=c, n = b+c); Focus on discordant pairs: pc and pb

7. McNemar test
mcnemar.test(tab, correct=TRUE) # always set continuity correction to TRUE
## McNemar exact test (binomial) & CI;
install.packages("exact2x2")
library(exact2x2)
mcnemarExactDP(x=9, m=11, n=161) # need total n for CI

# Two independent samples binary outcome
General recommendation: 
Use PCST, no contibuity correction, for large samples i.e counts in each cell > 5
Use Exact PCST or Fishers exact test for small samples i.e. counts < 5
prop.test(x=c(58,49), n=c(407,414), correct = FALSE)

8. Pearsons chi-square test
pch = chisq.test(tabD, correct=FALSE) # column/row order not important
### PCH, use exact null distribution, not chi^2(df=1) approximation
chisq.test(tabD, simulate.p.value=TRUE, B=1000000)   # permutation test

9. Fishers exact test
fisher.test(tabD)
# direct calculation
nC=407; nT=414; SC=58; ST=49
pC = SC/nC; pT = ST/nT
pbar = (SC+ST)/(nC+nT)
(zstat = (pT-pC)/sqrt(pbar*(1-pbar)*(1/nT+1/nC)))
zstat^2     # compare to X-squared
(pval = 2*pnorm(-abs(zstat)))

10. Odds Ratio & 95% CI
# install.packages("epitools")
library(epitools)
oddsratio(tabS, method='wald')

11. Power calculation
power.prop.test (p1=0.1, p2 = 0.05, power=0.9) #results in n which is per group

# Comparing 3 or more continuous groups 
ASSUMPTIONS: Same as two group
STEP 1: Test overall hypothesis of equal means (use F-test)
STEP 2: Post-hoc analysis (pairwise comparison and correction for multiple comparisons)

12. One-way ANOVA test; parametric
Step 1: F-test
fit ← aov(measurement outcome ~ group, data = dataframe)
summary(fit) # provides f-test
Step 2: Pairwise testing
pairwise.t.test(measurement outcome, group, p.adjust.method=”bonferroni”) #can also do “holm” for holm correction, better to do than bonferroni
tfit ← TukeyHSD(fit) # Tukey correction

13.	Nonparametric one-way ANOVA: Kruskal-Wallis test; rank-based; does not give CI
STEP 1: Overall comparison use kruskal.test(col ~ group, data=dataframe). If cannot reject H0, stop. 
STEP 2: Pairwise comparisons (slide 31)
•	Bonferroni correction – pairwise.wilcox.test(data$col, data$group, p.adj=”bonf")
•	Holm correction – pairwise.wilcox.test(data$col, data$group, p.adj=”holm")


# Comparing 3 or more binary groups 
ASSUMPTIONS: Same as two group
STEP 1: Test overall hypothesis of equal means (use F-test)
STEP 2: Post-hoc analysis (pairwise comparison and correction for multiple comparisons)

14. PCST
chisq.test requires >5 counts in >80% of cells
STEP 1: 
chisq.test(tabE)
STEP 2: 
chisq.test(tabE[c(1,2), ], cor=FALSE) # G1 vs G2
chisq.test(tabE[c(1,3), ], cor=FALSE) # G1 vs G3
chisq.test(tabE[c(2,3), ], cor=FALSE) # G2 vs G3

15. Fisher exact test: small samples
STEP 1: 
fisher.test(tabE)
STEP 2: 
fisher.test(tabE[c(1,2), ], cor=FALSE) # G1 vs G2
fisher.test(tabE[c(1,3), ], cor=FALSE) # G1 vs G3
fisher.test(tabE[c(2,3), ], cor=FALSE) # G2 vs G3

# Correlation
r closer to 1 is a tight correlation. direction based on sign; p>0.5 is considered strong
Correlation does not mean causation, correlation establishes an association
Pearson - normal distribution
Spearman - based on ranks and can be used for non-normal distribution

16.	Pearson’s correlation coefficient 
Assumption: data is normally distributed (regardless of sample size) and association is linear.
Scatter plot: plot(col~group, data=dataframe, pch=16, col=4)
cor.test(dataframe$col, dataframe$group)

17.	Spearman rank correlation = Pearson correlation of ranks of X,Y
Based on ranks, it does not require normality
cor.test(dataframe$col, dataframe$group, method=”spearman”) # default method is pearson
OR can be computed using this to get 95%CI
df2 = dataframe[!is.na(dataframe$col) & !is.na(dataframe$group), ]
cor.test(rank(df2$col), rank(df2$group), meth=”pearson”)


fit ← lm(son~father, data=mgal); summary(fit) 

confint(fit) # provides confidence intervals

# Simple linear Regression

fit = lm(col1 ~ col2, data=dataframe); summary(fit); abline(fit) # Provides intercept; r = estimate/sd(son) *sd(father) = cor.test(son, father)
Residuals are vertical distance between y and the regression line; residuals: df = n-2, t0.975 = 1.965

slope = intercept/sd(data$col1) * sd(data$col2)
slope can be computed using cor.test(data$col1, data$col2) – WALD test

Compute CI: confint(fit)

Simple linear regression: analysis of variance table (slide 29)
anova(fit)
summary(fit)

Mean square residual s^2 = RSS/(n-2)
R^2 = MSS/TSS





