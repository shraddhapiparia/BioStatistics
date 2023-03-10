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

3. One sample t test
t.test(pa.dm2$Charlson)
