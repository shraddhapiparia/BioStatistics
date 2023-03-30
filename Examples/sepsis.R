sep = read.csv("./Datasets/Sepsis.csv")
# Check data
dim(sep)
sep[1:3,]
table(sep$treat)

# Proportion of deaths after 30 days in the placebo arm
(tab = table(sep$treat, sep$death30))
prop.table(tab, margin=1)
prop.test(x=92, n=231)

# Proportion of deaths after 30 days in the ibuprofen arm
prop.test(x=84, n=224)

# Checking effectiveness of Ibuprofen
chisq.test(tab, correct=FALSE)

boxplot(apache ~ treat, data=sep)

tapply(sep$apache, sep$treat, sd, na.rm=TRUE)

t.test(apache ~ treat, data=sep, var.equal=TRUE)

summary(sep$race); table(sep$race)

sep$race = as.factor(sep$race) # treat race as categorical variable

boxplot(apache ~ race, data=sep)
tapply(sep$apache, sep$race, sd, na.rm=TRUE)

fit = aov(apache ~ race, data=sep)
summary(fit)

TukeyHSD(fit)


t.test(sep$temp0)
t.test(sep$temp24)
t.test(sep$temp24, sep$temp0, paired=TRUE)
t.test(sep$temp24-sep$temp0)
boxplot(sep$temp0, sep$temp24)

boxplot(temp24 ~ treat, data=sep)
with(sep, tapply(temp24, treat, sd, na.rm=TRUE))

t.test(temp24 ~ treat, data=sep, var.equal=TRUE)

(tab = t(matrix(c(8, 17, 7, 20), ncol=2)) )
mcnemar.test(tab)
binom.test(x=7, n=24)




