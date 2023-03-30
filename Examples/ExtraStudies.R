#### ENE-COVID study
## hand calculation
n = 51958; S = 2390
(phat = S/n)
(SE = sqrt(phat*(1-phat)/n))
(me = 1.96*SE)
(CI95 = phat + c(-1,1)*me)
## direct computation
prop.test(x=2390, n=51958)

#### YouGov poll
prop.test(x=195, n=390)
# 0.4506193 0.5493807


#### CR7, binomial test
binom.test(x=13, n=15, p=0.755)

## detailed calculations:
sum(dbinom(13:15, size=15, prob=0.755)) # right tail prob
sum(dbinom(0:10, size=15, prob=0.755))  # left tail prob
sum(dbinom(c(0:10, 13:15), size=15, prob=0.755))  # left tail prob


#### YouGov, one-sample z-test
prop.test(x=195, n=390, p=0.4, correct=FALSE)
## verify calculations:
(zstat = (0.5-0.4)/sqrt(0.4*0.6/390))
(pvalue = 2*pnorm(-abs(zstat)))
sqrt(16.25)  # X-squared = z^2

## continuity correction (default)
prop.test(x=195, n=390, p=0.4)
## verify calculations
(zstatc = (0.5-0.4-1/780)/sqrt(0.4*0.6/390))
(pvaluec = 2*pnorm(-abs(zstatc)))
sqrt(15.836)  # X-squared = z^2


#### McNemar test, Botswana military
## McNemar exact test
binom.test(x=9, n=9+2)  # p=0.5 by default
## McNemar z-test, continuity correction
prop.test(x=9, n=9+2)
## McNemar z-test, no continuity correction
prop.test(x=9, n=9+2, correct=FALSE)

### McNemar functions
## McNemar z-test, with/without continuity correction
(tab = matrix(c(146,9,2,4), ncol=2))  # cross-tabulation
mcnemar.test(tab)
mcnemar.test(tab, correct=FALSE)

## McNemar exact test (binomial) & CI
# install.packages("exact2x2")
library(exact2x2)
mcnemarExactDP(x=9, m=11, n=161) # need total n for CI


###### Binary Outcomes: Ebola study
tabE = matrix(c(145, 577, 299, 45, 261, 51), ncol=2)

#### Pearson's chi-square test
### step 1: overall Pearson's chi-square test
chisq.test(tabE)

### step 2: pairwise comparisons
## H01: p1=p2
chisq.test(tabE[c(1,2),], correct=FALSE)  # pick rows 1,2 of tabE

## H02: p1=p3
chisq.test(tabE[c(1,3),], correct=FALSE)  # pick rows 1,3 of tabE

## H03: p2=p3
chisq.test(tabE[c(2,3),], correct=FALSE)