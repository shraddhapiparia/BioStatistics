#### Read in InterWalk study dataset
iwk = read.csv("./Datasets/interwalk.csv")
dim(iwk)
iwk[1:3,]
table(iwk$group) # 1 = ctr, 2 = treatment
summary(iwk$Totaltimemin) # total IWT time over 12 weeks, in min


#### Daily IWT: independent samples t-test, 95% CI
iwk$iwtdaily = iwk$Totaltimemin/(12*7) # daily IWT
boxplot(iwtdaily ~ group, data=iwk)
t.test(iwtdaily ~ group, data=iwk, var.equal=TRUE)

## Direct computation of t-test (for illustration)
table(iwk$group)
tapply(iwk$iwtdaily, iwk$group, mean)  # xbar_C, xbar_T
tapply(iwk$iwtdaily, iwk$group, sd)    # s_C, s_T


#### Compare baseline age b/w groups
## Check assumptions
table(iwk$group)
boxplot(Age1 ~ group, data=iwk)
with(iwk, tapply(Age1, group, sd))       # comparable sd's

## Run t-test, 95% CI
t.test(Age1 ~ group, data=iwk, var.equal=TRUE)

## Hand calculations for t.test, 95% CI
qt(0.975, df=35)           # critical value = 2.03
pt(-0.747, df=35)*2        # p-value = 0.46


#### Welch t-test
t.test(iwtdaily ~ group, data=iwk)
t.test(Age1 ~ group, data=iwk, var.equal=FALSE)


### Transformations: log
with(iwk, tapply(PAEE_b, group, sd, na.rm=TRUE))
with(iwk, tapply(log(PAEE_b), group, sd, na.rm=TRUE))

t.test(PAEE_b ~ group, data=iwk, var.equal=TRUE)
t.test(PAEE_b ~ group, data=iwk)
t.test(log(PAEE_b) ~ group, data=iwk, var.equal=TRUE)


### Wilcoxon rank sum test
boxplot(iwtdaily ~ group, data=iwk)
hist(iwk$iwtdaily)
library("moments")
with(iwk, tapply(iwtdaily, group, skewness))
wilcox.test(iwtdaily ~ group, data=iwk)

## equivalent syntax:
iwt.control = iwk$iwtdaily[iwk$group==1]
iwt.treatment = iwk$iwtdaily[iwk$group==2]
wilcox.test(iwt.control, iwt.treatment)

## verify Wilcoxon normal approximation (illustration)
nC = table(iwk$group)[1]; nT = table(iwk$group)[2]; n=nC+nT
iwk$iwtd.rank = rank(iwk$iwtdaily)
with(iwk, tapply(iwtd.rank, group, mean))
rc = mean(iwk$iwtd.rank[iwk$group==1])
expected.rc = (n+1)/2
se.rc = sqrt(nT*(n+1)/(12*nC))
zstat = (rc-expected.rc)/se.rc
pvalue = 2*pnorm(-abs(zstat))

## 95% CI for "difference in location"
wilcox.test(iwtdaily ~ group, data=iwk, conf.int=TRUE)


### Paired Wilcoxon signed rank test
## InterWork study, physical well-being
iwk$PCS2 = iwk$PCS1 + iwk$PCS_change  # follow-up PCS
boxplot(iwk$PCS1, iwk$PCS2)
## paired samples t-test
t.test(iwk$PCS2, iwk$PCS1, paired=TRUE)
t.test(iwk$PCS_change, mu=0)          # same
## paired Wilcoxon signed-rank test
wilcox.test(iwk$PCS1, iwk$PCS2, paired=TRUE)
wilcox.test(iwk$PCS_change, mu=0)     # same


### Power calculations
## Delta=4 min, sigma=7.4 min, power=80%
power.t.test(delta=4, sd=7.4, power=0.80, type="two.sample")

## n=80 total, sigma=7.4 min, power=80%
power.t.test(n=40, sd=7.4, power=0.80, type="two.sample")


#### Pearson correlation
plot(Totaltimemin ~ VO2max1, data=iwk, pch=16)
cor.test(iwk$Totaltimemin, iwk$VO2max1)

plot(HbA1c1 ~ bmi, data=iwk, pch=16, col=4)
cor.test(iwk$HbA1c1, iwk$bmi) # pearson by default
cor.test(iwk$HbA1c1, iwk$bmi, meth="spear")

fit = lm(HbA1c1~bmi, data = iwk)
abline(fit, col=4)
summary(fit)

# class dataset
tmp = iwk[,c("height","Weight")]
dim(tmp)
rank(tmp$height)
rank(tmp$Weight)
tmp$height = rank(tmp$height)
tmp$Weight = rank(tmp$Weight)

cor.test(tmp$height, tmp$Weight)

# make it work
tmp2= tmp[!is.na(tmp$height),]
tmp2$rheight = rank(tmp2$height)
tmp2$rWeight = rank(tmp2$Weight)
cor.test(tmp2$rheight, tmp2$rWeight)

#### Spearman correlation
cor.test(iwk$Totaltimemin, iwk$VO2max1, method="pearson")
cor.test(iwk$Totaltimemin, iwk$VO2max1, method="spearman")

## Direct calculation:
iwk2 = iwk[!is.na(iwk$VO2max1) & !is.na(iwk$Totaltimemin),]
rank(iwk2$Totaltimemin)
rank(iwk2$VO2max1)
cor.test(rank(iwk2$Totaltimemin), rank(iwk2$VO2max1), meth="pearson")

## Spearman correlation: t-test
cor.test(iwk$HbA1c1, iwk$bmi, method="spear")

## Spearman correlation: 95% CI
iwk2 = iwk[!is.na(iwk$HbA1c1) & !is.na(iwk$bmi),]
cor.test(rank(iwk2$HbA1c1), rank(iwk2$bmi), meth="pearson")

