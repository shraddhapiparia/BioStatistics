#### 2. Hypertension study, compare baseline-followup
ht <- read.csv("./Datasets/Hypertension.csv")
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