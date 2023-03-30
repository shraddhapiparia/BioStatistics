#### Read in and explore dataset
dinf = read.csv("./Datasets/DualHIVInfection.csv")
dim(dinf)
dinf[1:5,]

table(dinf$DualHIV)
summary(dinf)

## Prevalence of NC impairment
(tab = table(dinf $Impaired))
(prop.table(tab))
## 95% CI for proportion impaired
prop.test(x=tab[2], n=sum(tab))
## Binomial (exact) 95% CI
binom.test(x=tab[2], n=sum(tab))

#### 1. Compare mean Tscore to 50 (normal population mean)
boxplot(dinf$Tscore, horizontal=TRUE, col="orange", xlab="Neurocognitive T-score")
abline(v=50) 

### alternatively: vertical boxplot
pdf(file="Boxplot1.pdf")
boxplot(dinf$Tscore, col="orange", xlab="Neurocognitive T-score")
abline(h=50)  # horizontal line at 50
dev.off()

t.test(x=dinf$Tscore, mu=50)


#### 2. Compare mean Tscore b/w dual- and single-HIV infected groups
boxplot(Tscore ~ DualHIV, data=dinf, col="purple")
sd(dinf$Tscore[dinf$DualHIV==0])
sd(dinf$Tscore[dinf$DualHIV==1])  # SD's are comparable
t.test(Tscore ~ DualHIV, data=dinf) # Welch t-test by default!
t.test(Tscore ~ DualHIV, data=inf, var.equal=TRUE)

#### 3. Compare mean log10VL b/w plasma and CSF
plot(log10VLpl ~ log10VLcsf, data=dinf)
abline(a=0, b=1)
log10VLdiff = dinf$log10VLpl - dinf$log10VLcsf
boxplot(log10VLdiff); abline(h=0)
t.test(x=dinf$log10VLpl, y=dinf$log10VLcsf, paired=TRUE)


####### Statistical Analysis for Binary Responses

### Binary response, one group
## One-sample z-test: prop(impaired) = 0.15
# x = 21 impaired out of n=38 individuals
prop.test(x=21, n=38, p=0.15)
tab = table(dinf$Impaired)
prop.test(x=rev(tab), p=0.15)

## One-sample binomial test
binom.test(x=21, n=38, p=0.15)
binom.test(x=rev(tab), p=0.15)


### Compare binary response between two groups
tabimp = with(dinf, table(DualHIV, Impaired))
prop.table(tabimp, margin=1)

prop.test(tabimp[,2:1], correct=FALSE)
chisq.test(tabimp[,2:1], correct=FALSE)

fisher.test(tabimp)

#### DualHIVInfection.csv BINARY GROUPS
## Groups:
(table(dinf$DualHIV))

## independent proportions z-test
(tabD = table(dinf$DualHIV, dinf$Impaired))
prop.table(tabD, margin=1)
prop.test(x=c(14,7), n=c(29,9), correct=FALSE)
prop.test(tabD[,2:1], correct=FALSE) # successes should be 1st column

### Pearson's chi-square test
(pch = chisq.test(tabD, correct=FALSE)) # column/row order not important
# get expected counts:
pch$expected
##            0         1
##  0 12.973684 16.026316
##  1  4.026316  4.973684      # two cells <5

### PCH, use exact null distribution, not chi^2(df=1) approximation
chisq.test(tabD, simulate.p.value=TRUE, B=1000000)   # permutation test

### Fisher's exact test
fisher.test(tabD)

## verify FET p-value:
sum(dhyper(c(0:2, 7:9), m=21, n=17, k=9))
        

#### Hydroxychloroquine RCT
### independent proportions z-test
tabH = matrix(c(58,49, 349, 365), ncol=2)
prop.test(tabH, correct=FALSE)              # test H0: pT=pC
prop.test(x=c(58,49), n=c(407,414), cor=F) # same thing

## direct calculation:
nC=407; nT=414; SC=58; ST=49
pC = SC/nC; pT = ST/nT
pbar = (SC+ST)/(nC+nT)
(zstat = (pT-pC)/sqrt(pbar*(1-pbar)*(1/nT+1/nC)))
zstat^2     # compare to X-squared
(pval = 2*pnorm(-abs(zstat)))

### Pearson's chi-square test
chisq.test(tabH, correct=FALSE)

### Exact Pearson's chi-square test
chisq.test(tabH, simulate=T, B=10000)


#### Chi-square (df=1)
qchisq(0.95, df=1)     # alpha=0.05 critical value
1-pchisq(2.42, df=1) # p-value, Dual HIV infection


#### Strepto RCT 1948
(tabS = matrix(c(51,38,4,14), ncol=2))

## OR & 95% CI, Wald
# install.packages("epitools")
library(epitools)
oddsratio(tabS, method='wald')

## OR & 95% CI, Fisher
fisher.test(tabS)


#### Power calculations, Hydroxy RCT
power.prop.test(p1=0.10, p2=0.05, power=0.90)




