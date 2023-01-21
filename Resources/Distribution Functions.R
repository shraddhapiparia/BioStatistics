1. Binomial distribution 
dbinom(x=10, size = 10, prob=0.8) # pr of 10 out of 10 surviving
dbinom(x=8, size=10, prob=0.8)    # pr of 8 out of 10 surviving
barplot(dbinom(0:40, size=40, prob=0.3))

pnorm() # probability norm
qnorm(c(0.05,0.95)) # quantile norm z score
